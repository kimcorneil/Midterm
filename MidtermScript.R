# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Weather and bike rental correlation #######
# duplicate weather and call it weather2 so the original data set remains unaffected
weather2 <- weather

# make date into a date (same format as trip4)
weather2 <- weather2 %>% mutate(date = mdy(date)) %>% # convert to mdy format as that is what it is given in
  mutate(date = format(date, "%d-%m-%Y")) %>% # format to dmy format with "-" as a separator
  mutate(date = dmy(date)) # convert to a date with dmy format
## see that weather2 and trip4 have the same ranges for dates 
range(weather2$date)
range(trip4$start_date)
##### CRYING SCREAMING THROWING UP BECAUSE IS IT SUPPOSED TO BE JUST 2014 DATA CAUSE IF IT IS I HAVE TO RESTART

# convert zip code to numeric in trip5 OR 4 so they are the same for comparison

