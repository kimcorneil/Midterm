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
View(trip5)
View(weather2)

## plan: add start city and end city to trip based on station info
## create new data set with weather and station info; unless cor() works on diff df's

# Create trip6 from trip4
trip6 <- trip4

# Create station2 so that station remains unaffected
station2 <- station

# make cities in station a factor
station2$city <- factor(station2$city)
levels(station2$city)

# make cities in station a character
station2$id <- as.character(station2$id)


table(station2$city, sum(station2$id))
## create a table with all the stations in the cities
station_table <- station2 %>%
  select(city, id) %>% # use only cities and ids
  arrange(city, id) # sorts a table 

# Add a new column for start city and end city into trip6
trip6 <- trip6 %>% left_join(trip6, by = c(station2$ID))





