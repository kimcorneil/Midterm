# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Calculate the average utilization of bikes for each month #######
# make trip5 so trip4 remains unaffected
trip5 <- trip4

# create a new column named month using months() to get the month using mutate
## I know it could technically start and end on a different month but we are assuming it doesnt
trip5 <- trip5 %>% mutate(trip5, month = months(start_date))

# make month a factor
trip5$month <- factor(trip5$month)

# calculate total duration per month
trip5 %>% 
  group_by(month) %>%
  sum(duration)
### THIS DID NOT WORK DO SOMETHING ESLE


