# UI.R

# df0 = read.csv('Bluebike_data.csv')


dashboardPage(
    dashboardHeader(title = "Boston Bluebikes",
                    titleWidth = 230,
                    tags$li(' by Mohamad Sayed',style = 'text-align: left ;padding-top:18px; font-family: Arial;
                      font-weight: bold;  font-size: 14px;', class = 'dropdown'),
                    tags$li(a(href = 'https://github.com/mo-sayed/Bluebikes-Shiny_Visualization',
                              img(src = 'https://pngimg.com/uploads/github/github_PNG20.png', 
                                  title = "Link for Github", height = "20px")),
                            class = "dropdown")),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Customer Trip Duration", tabName = "Page1"), 
            menuItem("Trip Variation per Gender", tabName = "Page2"),
            menuItem("Daily Active Cyclists", tabName = "Page3"),
            menuItem("Bluebikes Heatmap", tabName = "Page4"),
            menuItem("About Me", tabName = "Page5")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Page1",
                    fluidRow(
                        box(plotOutput("dailytripduration")),
                        box(plotOutput("dailyusertype")),
                        box(sliderInput('duration', label = 'Trip Duration (minutes)',
                                        min = 1,max = 60,value = c(1,30)
                        )),
                        box(sliderInput("time", label = 'Time of Day (hour)',
                                        min = 0,max = 24,value = c(7,12)
                        )
                        )
                    )
            ),
            tabItem(tabName = "Page2",
                    fluidRow(
                        box(plotOutput("trip_bygender")),
                        box(plotOutput("hourlytrips")),
                        box(sliderInput('timelog', label = 'Range of Dates',
                                        min = min(df0$STARTDAY),
                                        max = max(df0$STARTDAY),
                                        value = c(min(df0$STARTDAY),max(df0$STARTDAY), width = 12)
                        ))
                    )
            ),
            tabItem(tabName = "Page3",
                    fluidRow(
                        box(plotOutput("dailycyclists"), width = 12)
                        # box(checkboxGroupInput('weekdays', label = 'Select Day(s)',
                        #                choices = c("Monday", "Tuesday", "Wednesday", "Thursday",
                        #                            "Friday", "Saturday", "Sunday"),
                        #                selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
                        #                 )),
                        # box(checkboxGroupInput('gender', label = "Select Gender",
                        #                 choices = c("Male", "Female"),
                        #                 selected = c("Male", "Female")
                        #                 ))

                        )
                    ),
            tabItem(tabName = "Page4",
                    fluidRow(
                        box(leafletOutput("bikeheatmap"), width = 12),
                        box(sliderInput('riderage', label = 'Age Range of Cyclist',
                                        min = min(df0$Age),
                                        max = max(df0$Age),
                                        value = c(min(df0$Age), max(df0$Age)
                        ))
                    )
            )),
            tabItem(tabName = 'Page5', 
                    box(title = 'About Me',
                        status = 'info',
                        width = 6,
                        h5("  Hello, and welcome to my Visualization of Boston's Bluebikes rides spanning from 2017 to 2019.",
                           "My goal is to present the different metrics that I obtained from Bluebikes' open data."),
                        br(),
                        h5(
                           "  For more  background on my code, please click on the github icon on the top right of the screen."),
                        br(),
                         h5("Cheers,"),
                         h5("Mohamad Sayed")
                    ),
                    box(imageOutput('photo'))
            )
        )
    )
)