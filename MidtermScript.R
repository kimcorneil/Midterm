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
trip5$month <- factor(trip5$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# add a column for the number of seconds in a month 
trip5 <- trip5 %>% mutate(trip5, seconds_in_month = (days_in_month(trip5$start_date)*24*60*60))

# get only the unique values for month and seconds in a month into a new data frame
months_with_seconds <- data.frame(unique(trip5$month, trip5$seconds_in_month))

# calculate total duration per month
mytable <- trip5 %>% 
  group_by(month) %>%
  summarize(sum(duration)) 








