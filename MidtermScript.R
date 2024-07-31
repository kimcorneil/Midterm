# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Weekday Rush Hours #######
# Make trip4 so trip3 remains unaltered
trip4 <- trip3
summary(trip4)

# Clean up dates and times
## add lubridate to the library (previously installed)
library(lubridate)

## separate start_date and start_time (same with end_date and end_time)
trip4 <- separate(trip4, start_date, into = c("start_date", "start_time"), sep = " ")
trip4 <- separate(trip4, end_date, into = c("end_date", "end_time"), sep = " ")

## fix up dates with lubridate
## convert start_date and end_date to dmy format so it can be recognized as a date and not a character
trip4 <- trip4 %>% mutate(start_date = mdy(start_date)) %>% # convert to mdy format as that is what it is given in
 mutate(start_date = format(start_date, "%d-%m-%Y")) %>% # format to dmy format with "-" as a separator
  mutate(end_date = mdy(end_date)) %>% # do the same for end_date
  mutate(end_date = format(end_date, "%d-%m-%Y")) 
## convert start_date and end_date to dates not characters
trip4$start_date <- dmy(trip4$start_date)
trip4$end_date <- dmy(trip4$end_date)

## create a new data frame with just the weekdays 
## use weekdays to give the day of the week of start_date and filter based on if the day of the week is one Monday-Friday
trip_weekdays <- trip4 %>% filter(weekdays(start_date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

# convert start_time to a time (if that is even a thing)
trip_weekdays$start_time <- strftime(trip_weekdays$start_time, "%H:%M")
hist(trip_weekdays$start_time)
#### THIS DOES NOT WORK FIX IT AND FIGURE IT OUT 

# Plan
## first create 2 new columns for start times and end times and separate them from dates
## then create a new dataframe with just the weekdays in it
## then convert times to 24 hour clock ## OR DONT and do histogram by the hour ya i like that or like the 30 min or whatever
## then make them numeric (or something im a bit confused but make it like lowkey a histogram type shit i think)








