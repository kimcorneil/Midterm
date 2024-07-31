# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Cleaning the data #######

# Remove any trip starting and ending at the same station with a duration of less than 3 minutes from 'trip'
## create trip2 so the original data set 'trip' remains unaltered
trip2 <- trip
## convert start_station_id and end_station_id to character
trip2$start_station_id <- as.character(trip2$start_station_id)
trip2$end_station_id <- as.character(trip2$end_station_id)
## convert duration from integer to numeric
trip2$duration <- as.numeric(trip2$duration)
## since duration has a minimum of 60 and the units were not reported, I am making the assumption it was reported in seconds
## replace any trips starting and ending at the same station with a duration less than 3 minutes (180 seconds) with NA
trip2 <- trip2 %>% mutate(duration = 
                            case_when(duration < 180 & end_station_id == start_station_id ~ NA, # replace any conditions in duration as specified with NA
                                      .default = duration)) # if condition is not met the default is to maintain the value in duration
## move all the rows that have NAs in duration to a separate data frame (cancelled_trips) before removing them 
cancelled_trips <- trip2 %>% filter(is.na(duration))
## remove any rows in trip2 that have NAs in duration
trip2 <- trip2 %>% drop_na(duration)
## summarize trip2 to see changes
summary(trip2)



