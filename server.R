# SERVER.R

function(input, output){
  
  first = reactive({
    df0 %>% filter(TRIPDURATION <= 60*60*2) %>% 
      filter(hour(TIMEOFDAY) >= input$time[1] & hour(TIMEOFDAY) <=input$time[2]) %>% 
      filter(TRIPDURATION > input$duration[1]*60 & TRIPDURATION < input$duration[2]*60)
  })
  output$dailytripduration = renderPlot(
    # first() %>% select(TRIPDURATION, DAYOFWEEK, TIMEOFDAY) %>% 
      first() %>% select(TRIPDURATION, DAYOFWEEK) %>% 
      ggplot(aes(x=TRIPDURATION)) + 
      geom_freqpoly(binwidth = 60, aes(color=DAYOFWEEK)) +
      labs(x = "Duration (seconds)" , y = 'Number of Trips')
  )
  output$dailyusertype = renderPlot(
    # first() %>% select(USERTYPE, DAYOFWEEK, TRIPDURATION, TIMEOFDAY)%>% 
      first() %>% select(USERTYPE, DAYOFWEEK, TRIPDURATION)%>% 
      group_by(USERTYPE, DAYOFWEEK) %>%  summarise(n = n()) %>% 
      ggplot(aes(x = DAYOFWEEK, y = n)) + 
      geom_col(aes( fill = USERTYPE), position = 'dodge') + 
      labs(y="Number of Trips", x = "Day of the Week") +
      theme(legend.position = "top") +
      scale_fill_discrete(name = "User Type", breaks = c("Subscriber","Customer"), labels = c("Subscribers", "Customers"))
    
  )

  output$trip_bygender = renderPlot(
    df0 %>% filter(STARTDAY >= input$timelog[1] & STARTDAY <= input$timelog[2]) %>% 
      select(STARTDAY, GENDER, YEAR) %>% group_by(Date = STARTDAY, GENDER) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x=Date, y = n)) +
      geom_point(aes(color = GENDER)) +
      geom_smooth(method = "gam", aes(color = GENDER)) +
      labs(y = "Number of Trips") +
      theme(legend.key = element_blank(), legend.position = "bottom") + 
      scale_color_discrete(name = "Gender", breaks = c('Male', 'Female'), labels = c('Male', 'Female'))
  )
  
  output$hourlytrips = renderPlot(
    # df0 %>% select(STARTDAY, TIMEOFDAY, GENDER)%>% group_by(hr = hour(parse_date_time(df0$TIMEOFDAY, 'H:M:S'))) %>%
      df0 %>% select(STARTDAY, TIMEOFDAY, GENDER)%>% group_by(hr = hour(df0$TIMEOFDAY)) %>%
      filter(STARTDAY >= input$timelog & STARTDAY <= input$timelog) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x = hr, y = n)) +
      geom_bar(stat = 'identity', fill = "blue") +
      labs(x = "Time of day (hour)", y = "Number of Trips")
  )

  output$dailycyclists = renderPlot(
    df3 %>% ggplot(aes(x=TIMEOFDAY_)) +
      geom_point(aes(y=Male, color = "pink"), stat='identity') +
      geom_point(aes(y=Female, color = "blue"), stat='identity') +
      theme(legend.position = "none") +
      geom_line(aes(y=Male, group = 1, color = 'pink')) +
      geom_line(aes(y=Female, group = 1, color = 'blue')) +
      labs(x = 'Time of Day', y = 'Number of Cyclists',
           title = 'Daily Frequency of Cyclists', size=3.5) +
      ylim(c(0,400000))
  )
  
  age_reactive = reactive({
    df0 %>% filter(AGE >= input$riderage[1] &  AGE <= input$riderage[2])
  })
  
  output$bikeheatmap = renderLeaflet(
  age_reactive()  %>% leaflet() %>% addTiles() %>%  
    addHeatmap(lng = ~STARTSTATIONLONGITUDE, lat = ~STARTSTATIONLATITUDE, radius = 10, 
               gradient = "Blues" )
  )
  
  output$photo = renderImage({
    return(list(src='hubway4.jpg',
                filetype='image/jpeg',
                alt='photo'))
  }, deleteFile = F)
}