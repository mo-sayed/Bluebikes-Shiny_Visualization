# 1- Load and combine csv files for all blue bike data across 3 years
# 2- Clean data to remove any obscure outliers that would misrepresent data/trends
#    and remove unnecessary information
# 3-  Exploratory Data Analysis

library(dplyr)
library(stringr)
library(tidyr)
library(data.table)

# ************************************************************************
# 1- DATA LOADING
# ************************************************************************

# set current working directoy to the BlueBike folder
setwd("~/Desktop/NYC DSA/Lecture Slides/Projects/R Shiny/BlueBike")

csv_filenames_2017 = list.files('./data', pattern = 'csv')
csv_filepaths_2017 = paste0( './data/', csv_filenames_2017, sep = '' )
all_dfs_2017 <- lapply( csv_filepaths_2017, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ) )

csv_filenames_2018 = list.files('./data', pattern = 'csv')
csv_filepaths_2018 = paste0( './data/', csv_filenames_2018, sep = '' )
all_dfs_2018 <- lapply( csv_filepaths_2018, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ) )

# csv_filenames_2019 = list.files('./data', pattern = 'csv')
# csv_filepaths_2019 = paste0( './data/', csv_filenames_2019, sep = '' )
# all_dfs_2019 <- lapply( csv_filepaths_2019, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ) )

# ************************************************************************
# 1.1- Loading data one by to sample for 20% of the data (for memory capacity)
# ************************************************************************

# sampling 25% of each month, with seed being set to 1 to obtain recurring results
for (i in (1:12)){
  all_dfs_2017[[i]] = all_dfs_2017[[i]][sample(nrow(all_dfs_2017[[i]]),
                                               (round(0.3*nrow(all_dfs_2017[[i]])))),]
  all_dfs_2018[[i]] = all_dfs_2018[[i]][sample(nrow(all_dfs_2018[[i]]),
                                               (round(0.3*nrow(all_dfs_2018[[i]])))),]
  # all_dfs_2019[[i]] = all_dfs_2019[[i]][sample(nrow(all_dfs_2019[[i]]),
  #                                              (round(0.35*nrow(all_dfs_2019[[i]])))),]
}

# clean column names to remove ".", " ", and change to upper case
clean_colname = function(x){
  x = toupper(gsub(".", "", (gsub(" ", "", x, fixed = TRUE)), fixed=TRUE))
}

# change column names for all dataframes
for (i in (1:12)){
  colnames(all_dfs_2017[[i]]) = clean_colname(colnames(all_dfs_2017[[i]]))
  colnames(all_dfs_2018[[i]]) = clean_colname(colnames(all_dfs_2018[[i]]))
  # colnames(all_dfs_2019[[i]]) = clean_colname(colnames(all_dfs_2019[[i]]))
}

# rbind all list of dataframes into one dataframe for each year
df2017 = rbindlist(all_dfs_2017)
df2018 = rbindlist(all_dfs_2018)
# df2019 = rbindlist(all_dfs_2019)

# combine dataframes for each year into one master dataframe 'df0'
# df0 = rbind(df2017, df2018, df2019)
df0 = rbind(df2017, df2018)

write.csv(df0, "Bluebike_MasterData.csv")

# load csv file to df0 in order to clean data once Master csv file is created
df0 = read.csv("Bluebike_MasterData.csv")


# ************************************************************************
# 2- DATA CLEANING
# ************************************************************************

# clean data set to remove NAs, NULL values and people bron before 1940
df0 = df0 %>% filter(BIRTHYEAR >=1940 , BIRTHYEAR <= 2003)

# remove rows/data with no gender information
df0 = df0 %>% filter(GENDER!= 0)
df0$GENDER = ifelse(df0$GENDER == 2, 'Female', 'Male')

# remove rides that ended and started at the same station
df0 = df0 %>% filter(STARTSTATIONID != ENDSTATIONID)

# remove trips under 60 seconds and over 60 minutes
df0 = df0 %>% filter(TRIPDURATION <= 60*60, TRIPDURATION >= 60)

# remove rides that started and dnded in the same station
df0 = df0 %>% filter((STARTSTATIONLATITUDE != ENDSTATIONLATITUDE), 
                     (STARTSTATIONLONGITUDE != ENDSTATIONLONGITUDE) )

# remove start/end station IDs and stop time of ride
df0 = df0 %>% select(-c(STARTSTATIONID, ENDSTATIONID, STOPTIME))

# create a column for day of the week
df0$DAYOFWEEK = weekdays(as.Date(df0$STARTTIME))

# create a column for weekdays/weekends
df0$WEEKDAYEND= ifelse(df0$DAYOFWEEK %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# create a column for start date of ride
df0$STARTDAY = as.Date(df0$STARTTIME)

# change birthyear to integer/numeric
df0$BIRTHYEAR = as.numeric(df0$BIRTHYEAR)


as.ITime(head(df0$STARTTIME))+60
(class(df0$STARTTIME))

# Change birthyear class to integer from character
df0$BIRTHYEAR = as.integer(df0$BIRTHYEAR)

# # create a column for the time of day and year
df0$TIMEOFDAY = as.ITime(df0$STARTTIME)
# df0$TIMEOFDAY = format(as.POSIXct(df0$STARTTIME) ,format = "%H:%M:%S")  Used earlier and didn't work

# Create column for the year the ride was taken
df0$YEAR = year(as.Date(df0$STARTTIME))

head(year(as.Date(df0$STARTTIME))) - tail(df0$BIRTHYEAR)
class(year(as.Date(df0$STARTTIME)))
class(tail(df0$BIRTHYEAR))
View(head(df0))
# create column to show age of rider
df0$AGE = (df0$YEAR  - df0$BIRTHYEAR)
  
# remove birthyear column
# df0 = df0 %>% select(-c(BIRTHYEAR))

# function -->convert to time of day; i.e. Morning, Noon, Evening etc..
time_of_day = function(x){
  ifelse(hour(x)<10 & hour(x)>=5, "Morning", ifelse(hour(x) >= 10 & hour(x)<13, "Noon",
                                                    ifelse(hour(x)>=13 & hour(x)<17, "Afternoon", ifelse(hour(x)>=17 & hour(x)<21, "Evening","Late Night"))))
}

# create a column for morning/noon/evening/afternoon etc..
df0$TIMEOFDAY_ = time_of_day(df0$STARTTIME)

# remove STARTTIME column as it is redundant with other columns
df0 = df0 %>% select(-c(STARTTIME))

# convert timeofday format to date-time
# df0$TIMEOFDAY = (as.POSIXct(df0$TIMEOFDAY,format="%H:%M:%S"))

# convert startday to date format
df0$STARTDAY = as.Date(df0$STARTDAY)

# Writing 'Bluebike_data' csv file for cleaned dataframe to use for Analysis and Visualization
write.csv(df0, "Bluebike_data.csv")



# ************************************************************************
# 3- Data Analysis
# ************************************************************************

# this shows that %74.3 of riders are male
df0 %>% group_by(GENDER) %>% summarise(n=100*n()/nrow(df0))

# plot to show ratio of male vs female cyclists
df0 %>% group_by(GENDER) %>% summarise(ratio=n()/nrow(df0)) %>% 
  ggplot(aes(x=GENDER, y=ratio, fill = GENDER)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.35) + 
  labs(fill = "Gender", size = 3.5)+
  scale_y_continuous(labels = percent) +
  labs(x = 'Gender', y = 'Percentages', 
       title = '  Ratio of Male vs Female Cyclists', vjust = 5) +
  geom_text(aes(label=percent(round(ratio,3))),position="stack",vjust=-0.5, size = 3.5) + 
  ylim(0,1)

# variation of male/female cyclists per given day of the week
df0 %>% group_by(GENDER, DAYOFWEEK) %>% summarise(nbr_cyclists = n()) %>% 
  ggplot(aes(x=DAYOFWEEK, y=nbr_cyclists, fill=GENDER)) +
  labs(x = 'Day of the Week', y = 'Number of Cyclists', 
       title = '     Number of Cyclists on a given Day of the Week', size=3.5) +
  geom_point(aes(color = GENDER)) +
  geom_text(aes(label=nbr_cyclists),vjust=-1, size=2.75) + 
  ylim(0,300000)

df0 %>% group_by(GENDER, DAYOFWEEK) %>% summarise(nbr_cyclists = n()) %>% 
  spread(key = GENDER, value = nbr_cyclists) %>% 
  ggplot(aes(x=DAYOFWEEK)) +
  geom_point(aes(y=male, color = "pink"), stat='identity')+
  geom_point(aes(y=female, color = "blue"), stat='identity') + 
  geom_line(aes(y=male, group = 1, color = 'pink'))+
  geom_line(aes(y=female, group = 1, color = 'blue')) +
  scale_fill_discrete(name = "Dose", labels = c("Male", "Female")) +
  labs(x = 'Day of the Week', y = 'Number of Cyclists', 
       title = '     Number of Cyclists on a given Day of the Week', size=3.5)


# # create a column for the time of day and year
# df0$TIMEOFDAY = format(as.POSIXct(df0$STARTTIME) ,format = "%H:%M:%S")
# df0$YEAR = year(as.Date(df0$STARTTIME))

# # function to convert to time of day
# time_of_day = function(x){
#   ifelse(hour(x)<10 & hour(x)>=5, "Morning", ifelse(hour(x) >= 10 & hour(x)<13, "Noon",
#     ifelse(hour(x)>=13 & hour(x)<17, "Afternoon", ifelse(hour(x)>=17 & hour(x)<21, "Evening","Late Night"))))
# }
# 
# # create a column for morning/noon/evening/afternoon etc..
# df0$TIMEOFDAY_ = time_of_day(df0$STARTTIME)

# plot for frequency of male/female cyclists during different times of day
df0 %>% group_by(TIMEOFDAY_,GENDER) %>% summarise(frequency = n()) %>% 
  spread(key = GENDER, value = frequency) %>% 
  ggplot(aes(x=TIMEOFDAY_)) +
  geom_point(aes(y=male, color = "pink"), stat='identity') +
  geom_point(aes(y=female, color = "blue"), stat='identity') + 
  geom_line(aes(y=male, group = 1, color = 'pink')) +
  geom_line(aes(y=female, group = 1, color = 'blue')) +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
  labs(x = 'Time of Day', y = 'Number of Cyclists', 
       title = '     Cyclists on a Given Time of Day', size=3.5) +
  ylim(c(0,400000))


# df0 %>% group_by(TIMEOFDAY_,GENDER) %>% summarise(frequency = n()*100/nrow(df0))  

# Plot: ratio of male/female cyclists on a given time of day
df0 %>% group_by(TIMEOFDAY_,GENDER) %>% summarise(frequency = n()) %>% 
  spread(key = GENDER, value = frequency) %>% group_by(TIMEOFDAY_) %>% 
  mutate(Female_ratio = sum(female)/(sum(female)+sum(male)), Male_ratio = sum(male)/(sum(female)+sum(male))) %>% 
  ggplot(aes(x=TIMEOFDAY_)) +
  geom_point(aes(y=Male_ratio, color = "blue"), stat='identity') +
  geom_point(aes(y=Female_ratio, color = "pink"), stat='identity') +
  geom_line(aes(y=Male_ratio, color="blue", group=1)) +
  geom_line(aes(y=Female_ratio, color="pink", group=1)) +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
  labs(x = 'Time of Day', y = 'Percentage of Cyclists', 
       title = '     Cyclists on a Given Time of Day', size=3.5) +
  ylim(0.1,0.9)

# Plot: ratio of male/female cyclists on a given time during the weekday
df0 %>% filter(WEEKDAYEND == "Weekday") %>% group_by(TIMEOFDAY_,GENDER) %>% summarise(frequency = n()) %>% 
  spread(key = GENDER, value = frequency) %>% group_by(TIMEOFDAY_) %>% 
  mutate(Female_ratio = sum(female)/(sum(female)+sum(male)), Male_ratio = sum(male)/(sum(female)+sum(male))) %>% 
  ggplot(aes(x=TIMEOFDAY_)) +
  geom_point(aes(y=Male_ratio, color = "blue"), stat='identity') +
  geom_point(aes(y=Female_ratio, color = "pink"), stat='identity') +
  geom_line(aes(y=Male_ratio, color="blue", group=1)) +
  geom_line(aes(y=Female_ratio, color="pink", group=1)) +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
  labs(x = 'Time of Day', y = 'Percentage of Cyclists', 
       title = '     Cyclists on a Given Time of Day', size=3.5) +
  ylim(0.1,0.9)

# Plot: ratio of male/female cyclists on a given time during the weekend
df0 %>% filter(WEEKDAYEND == "Weekend") %>% group_by(TIMEOFDAY_,GENDER) %>% summarise(frequency = n()) %>% 
  spread(key = GENDER, value = frequency) %>% group_by(TIMEOFDAY_) %>% 
  mutate(Female_ratio = sum(female)/(sum(female)+sum(male)), Male_ratio = sum(male)/(sum(female)+sum(male))) %>% 
  ggplot(aes(x=TIMEOFDAY_)) +
  geom_point(aes(y=Male_ratio, color = "blue"), stat='identity') +
  geom_point(aes(y=Female_ratio, color = "pink"), stat='identity') +
  geom_line(aes(y=Male_ratio, color="blue", group=1)) +
  geom_line(aes(y=Female_ratio, color="pink", group=1)) +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
  labs(x = 'Time of Day', y = 'Percentage of Cyclists', 
       title = '     Cyclists on a Given Time of Day', size=3.5) +
  ylim(0.1,0.9)

# Plot: ratio of male/female cyclists on a given time during the weekend
df0 %>% filter(WEEKDAYEND == "Weekday", USERTYPE=='Subscriber', (YEAR - BIRTHYEAR)<=23 ) %>% group_by(TIMEOFDAY_,GENDER) %>% summarise(frequency = n()) %>% 
  spread(key = GENDER, value = frequency) %>% group_by(TIMEOFDAY_) %>% 
  mutate(Female_ratio = sum(female)/(sum(female)+sum(male)), Male_ratio = sum(male)/(sum(female)+sum(male))) %>% 
  ggplot(aes(x=TIMEOFDAY_)) +
  geom_point(aes(y=Male_ratio, color = "blue"), stat='identity') +
  geom_point(aes(y=Female_ratio, color = "pink"), stat='identity') +
  geom_line(aes(y=Male_ratio, color="blue", group=1)) +
  geom_line(aes(y=Female_ratio, color="pink", group=1)) +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
  labs(x = 'Time of Day', y = 'Percentage of Cyclists', 
       title = '     Cyclists on a Given Time of Day', size=3.5) +
  ylim(0.1,0.9)

# total travel time per year (male/female)
df0 %>% group_by(YEAR, GENDER) %>% summarise(total_duration = sum(TRIPDURATION)) %>% 
  spread(key = GENDER, value = total_duration) %>% 
  ggplot(aes(x=YEAR)) +
  geom_bar(aes(y=male, color = "blue", fill="blue", group=1), stat='identity') +
  geom_bar(aes(y=female, color = "pink", fill="pink", group=1), stat='identity') +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
  # scale_x_discrete(labels = "YEAR") +
  labs(x = 'Year', y = 'Number of Cyclists', 
       title = '     Number of Male/Female Cyclists per Year', fill = "Gender", size = 3)


#  HEATMAP
df0 %>% filter(GENDER == 'female') %>% leaflet()%>% addTiles() %>%
  addHeatmap(lng = ~STARTSTATIONLONGITUDE, lat = ~STARTSTATIONLATITUDE, 
             radius = 5, gradient = "Greens" )

# df0 %>% filter(GENDER == 'male') %>% leaflet() %>% addTiles() %>%  
#   addHeatmap(lng = ~STARTSTATIONLONGITUDE, lat = ~STARTSTATIONLATITUDE, radius = 5, 
#              gradient = "Blues" ) %>%  addHeatmap(lng = ~STARTSTATIONLONGITUDE, 
#                                                  lat = ~STARTSTATIONLATITUDE, radius = 5, gradient = "Greens" )


df0 %>% filter(GENDER == "female") %>% group_by(YEAR) %>% summarise(total_duration = sum(TRIPDURATION))

ggplot(yt.views, aes(Date, Views)) + geom_line() +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")
