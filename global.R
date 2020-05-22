library(shiny)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(lubridate)
library(shinydashboard)
library(tidyr)


# set current working directoy to R Shiny folder
setwd("~/Desktop/NYC DSA/Lecture Slides/Projects/R Shiny/BlueBike")
# 
# # After having created a new csv file and to save memory, load the new csv file into the dataframe 'df0'
df0 = read.csv('Bluebike_data.csv')

# convert timeofday format to date-time
# df0$TIMEOFDAY = (as.POSIXct(df0$TIMEOFDAY,format="%H:%M:%S"))
# class(df0$TIMEOFDAY)

# convert startday to date format
df0$STARTDAY = as.Date(df0$STARTDAY)


df1 = df0 %>% select(GENDER, TIMEOFDAY_, DAYOFWEEK, WEEKDAYEND) %>%
  group_by(TIMEOFDAY_, GENDER) %>% summarise(frequency = n()) %>%
  spread(key = GENDER, value = frequency) %>% select(TIMEOFDAY_, Male)

df2 = df0 %>% select(GENDER, TIMEOFDAY_, DAYOFWEEK, WEEKDAYEND) %>%
  group_by(TIMEOFDAY_, GENDER) %>% summarise(frequency = n()) %>%
  spread(key = GENDER, value = frequency) %>% select(TIMEOFDAY_, Female)

df3 = merge(df1, df2)


