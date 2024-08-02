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

# convert start_time to a time
## as.POSIXct converts to a calendar date and time but adds the current date
trip_weekdays$start_time <- as.POSIXct(trip_weekdays$start_time, format = "%H:%M")
## use cut to divide times into 30 minute intervals to analyze rush hour by the half hour
trip_weekdays$start_time_half <- cut(trip_weekdays$start_time, breaks = "30 min")
## as.POSIXct converts to a calendar date and time but adds the current date
trip_weekdays$start_time_half <- as.POSIXct(trip_weekdays$start_time_half, format = "%Y-%m-%d %H:%M:%S")
## remove the added date
trip_weekdays$start_time_half <- format(trip_weekdays$start_time_half, "%H:%M")

# Make a historgram
## Install ggplot2
library(ggplot2)
## use ggplot to make the histogram
ggplot(trip_weekdays, aes(x = start_time_half)) +
  geom_histogram(stat = "count", binwidth = 1) +
  labs(title = "Bike Rental Start Time Frequencies",
       x = "Start Time",
       y = "Frequency") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1)) ## tilt titles so that they can be seen

# Define a rush hour as any frequency as anything with a frequency over 10000
## Given that, the rush hours are: 7:30-9:30 and 16:00-18:30

####### Determine 10 most frequent starting and ending stations during weekday rush hours #######

# Make a new data frame including only data from the rush hour times 
rush_weekdays <- trip_weekdays %>% 
  filter(start_time_half == c("7:30", "8:00", "8:30", "9:00", "9:30", "16:00", "16:30", "17:00", "17:30", "18:00", "18:30"))

# starting stations
## make starting stations id into a factor
rush_weekdays$start_station_id <- factor(rush_weekdays$start_station_id)
## make a table of the factor
rush_table <- table(rush_weekdays$start_station_id)
# sort the table by decreasing, subset 1:10 to get top 10
sort(rush_table, decreasing = T)[1:10]
# see that most frequent starting stations have the ID: 70  65  77  61  64  60  69  51  67  74  
## in report link these to their names of the stations

# ending stations
## make starting stations id into a factor
rush_weekdays$end_station_id <- factor(rush_weekdays$end_station_id)
## make a table of the factor
rush_table2 <- table(rush_weekdays$end_station_id)
# sort the table by decreasing, subset 1:10 to get top 10
sort(rush_table2, decreasing = T)[1:10]
# see that most frequent ending stations have the ID: 70   69   50   74   55   77   39   61   72   60 
## in report link these to their names of the stations

####### Determine 10 most frequent starting and ending stations during weekend rush hours #######
# create a new data frame called trip_weekends
trip_weekends <- trip4 %>% filter(weekdays(start_date) %in% c("Sunday", "Saturday"))

## DO THE SAME THING AS WITH WEEKDAYS TO GET THE HISTOGRAM TO DETERMINE RUSH HOURS
# convert start_time to a time
## as.POSIXct converts to a calendar date and time but adds the current date
trip_weekends$start_time <- as.POSIXct(trip_weekends$start_time, format = "%H:%M")
## use cut to divide times into 30 minute intervals to analyze rush hour by the half hour
trip_weekends$start_time_half <- cut(trip_weekends$start_time, breaks = "30 min")
## as.POSIXct converts to a calendar date and time but adds the current date
trip_weekends$start_time_half <- as.POSIXct(trip_weekends$start_time_half, format = "%Y-%m-%d %H:%M:%S")
## remove the added date
trip_weekends$start_time_half <- format(trip_weekends$start_time_half, "%H:%M")

# Make a historgram
## use ggplot to make the histogram
ggplot(trip_weekends, aes(x = start_time_half)) +
  geom_histogram(stat = "count", binwidth = 1) +
  labs(title = "Bike Rental Start Time Frequencies",
       x = "Start Time",
       y = "Frequency") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1)) ## tilt titles so that they can be seen

# Define a rush hour as any frequency as anything with a frequency over 1700 ## FUN FACT they both have 11 times
### DIFFERENT THAN WEEKDAY AS DEALING WITH DIFFERENT FREQUENCIES 
## Given that, the rush hours are: 11:30 - 16:30

# Make a new data frame including only data from the rush hour times 
rush_weekends <- trip_weekends %>% 
  filter(start_time_half == c("11:30", "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", "15:00", "15:30", "16:00", "16:30"))

# starting stations
## make starting stations id into a factor
rush_weekends$start_station_id <- factor(rush_weekends$start_station_id)
## make a table of the factor
rush_table3 <- table(rush_weekends$start_station_id)
# sort the table by decreasing, subset 1:10 to get top 10
sort(rush_table3, decreasing = T)[1:10]
# see that most frequent starting stations have the ID: 50  60  54  61  76  39  70  72  65  64 
## in report link these to their names of the stations

# ending stations
## make starting stations id into a factor
rush_weekends$end_station_id <- factor(rush_weekends$end_station_id)
## make a table of the factor
rush_table4 <- table(rush_weekends$end_station_id)
# sort the table by decreasing, subset 1:10 to get top 10
sort(rush_table4, decreasing = T)[1:10]
# see that most frequent ending stations have the ID: 50  60  61  70  39  54  76  74  65  48 
## in report link these to their names of the stations
