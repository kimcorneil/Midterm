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

# rename id in trip6 so it doesnt get confusing 
names(trip6)[names(trip6) == "id"] <- "trip_id"

# Add a new column for start city and end city into trip6
trip6 <- trip6 %>% left_join(station2, by = c("start_station_id" = "id"))

# remove columns added that were not needed
trip6 = trip6[,!(names(trip6) %in% c("installation_date","dock_count", "lat", "long", "name"))]

# rename city to start_city
names(trip6)[names(trip6) == "city"] <- "start_city"

# repeat for end city
trip6 <- trip6 %>% left_join(station2, by = c("end_station_id" = "id"))
trip6 = trip6[,!(names(trip6) %in% c("installation_date","dock_count", "lat", "long", "name"))]
names(trip6)[names(trip6) == "city"] <- "end_city"

# make the start and end cities factors
trip6$start_city <- factor(trip6$start_city)
trip6$end_city <- factor(trip6$end_city)

# check how many times a city appears in start and end city
table(trip6$start_city)
table(trip6$end_city) ## notice they are not the same meaning some trips start in one city and end in another

# To deal with cities starting and ending in different places I will do weather correlation analysis for both
start_city_weather <- trip6 %>% left_join(weather2, by = c("start_city" = "city"))

# cor plot time
# install corrplot
install.packages("corrplot")
# add it to library
library(corrplot)


