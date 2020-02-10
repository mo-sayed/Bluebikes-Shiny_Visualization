library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(shinydashboard)
library(tidyr)


# set current working directoy to R Shiny folder
# setwd("/Users/moesayed/Desktop/NYC DSA/Lecture Slides/Projects/R Shiny/BlueBike")
# 
# # After having created a new csv file and to save memory, load the new csv file into the dataframe 'df0'
df0 = read.csv('Bluebike_data.csv')
df0$STARTDAY = as.Date(df0$STARTTIME)


df0$Age = (as.integer(format(Sys.Date(), "%Y")) - df0$BIRTHYEAR)


df0 %>% select(GENDER, TIMEOFDAY_, DAYOFWEEK)

df1 = df0 %>% select(GENDER, TIMEOFDAY_, DAYOFWEEK, WEEKDAYEND) %>%
  group_by(TIMEOFDAY_, GENDER) %>% summarise(frequency = n()) %>%
  spread(key = GENDER, value = frequency) %>% select(TIMEOFDAY_, male)

df2 = df0 %>% select(GENDER, TIMEOFDAY_, DAYOFWEEK, WEEKDAYEND) %>%
  group_by(TIMEOFDAY_, GENDER) %>% summarise(frequency = n()) %>%
  spread(key = GENDER, value = frequency) %>% select(TIMEOFDAY_, female)

df3 = merge(df1, df2)


