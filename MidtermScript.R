# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Cleaning the data #######
# Remove any trip starting or ending at the same station with a duration of less than 3 minutes from 'trip'
## create trip2 so the original data set 'trip' remains unaltered
trip2 <- trip
## convert start_station_id and end_station_id to character
trip2$start_station_id <- as.character(trip2$start_station_id)
trip2$end_station_id <- as.character(trip2$end_station_id)
## since duration has a minimum of 60 and the units were not reported, I am making the assumption it was reported in seconds
