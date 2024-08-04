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


# calculate total duration of bikes used per day per month
hours_day <- trip5 %>% 
  group_by(month) %>%
  dplyr::summarize(hours_per_day = sum(duration / 3600)/mean(day_in_month)) 

# calculate the total hours bikes were used per month
total_hours <- trip5 %>%
  group_by(month) %>%
  dplyr::summarize(total_duration_hours = sum(duration / 3600))

# make a bar plot of the Hours of Average Daily Bike Usage Per Month
ggplot(hours_day, aes(x = month, y=hours_per_day)) +
  geom_bar(stat = "identity") +
  labs(title = "Hours of Average Daily Bike Usage Per Month") +
  xlab("Month") +
  ylab("Average Hours per Day")


