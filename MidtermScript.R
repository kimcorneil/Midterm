# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Weather and bike rental correlation #######
# duplicate cleaned weather and call it weather2 so the original data set remains unaffected
weather2 <- clean_weather

# make date into a date (same format as trip4)
weather2 <- weather2 %>% mutate(date = mdy(date)) %>% # convert to mdy format as that is what it is given in
  mutate(date = format(date, "%d-%m-%Y")) %>% # format to dmy format with "-" as a separator
  mutate(date = dmy(date)) # convert to a date with dmy format
## gut check to see that weather2 and trip4 have the same ranges for dates 
range(weather2$date)
range(trip4$start_date)

## plan: add start city and end city to trip based on station info
## create new data set with weather and station info

# Create trip6 from trip4
trip6 <- trip4

# Create station2 so that station remains unaffected
station2 <- station

# make cities in station a factor
station2$city <- factor(station2$city)
levels(station2$city) # confirm the levels are as expected

# make ID in station a character
station2$id <- as.character(station2$id)

# rename id in trip6 so it doesnt get confusing (station has a column named id for station id)
names(trip6)[names(trip6) == "id"] <- "trip_id"

# Add a new column for start city and end city into trip6
trip6 <- trip6 %>% left_join(station2, by = c("start_station_id" = "id"))

# remove columns added in left_join that were not needed
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

# create a new data frame summarizing the total duration biked per day per city
summary_start <- trip6 %>% 
  group_by(start_date, start_city) %>% # group by start_date and start_city
  dplyr::summarize(total_duration = sum(duration)) %>% #create total duration which is a summary of duration #specify dplyr here as code doesnt run if summarize() from plyr is used
  ungroup() # ungroup() to ensure downstream analysis is not grouped

# group weather and start_summary by date and city
start_weather <- weather2 %>% left_join(summary_start, by = c("date" = "start_date", "city" = "start_city"))

# clean start_weather so it is ready to go for correlation analysis
summary(start_weather)
# events is not a numerical category and will cause issues in the correlation analysis
## make events a factor to see what we are dealing with
start_weather$events <- factor(start_weather$events)
table(start_weather$events)
### the levels of events are not well defined. If there was rain or fog or a rain-thundertorm (only 1) it is dealt with in the other columns

## lots of issues with precipitation_inches 
## set T to 0.011, this comes with potential errors (see report)
# Replace T with 0.011
## first find where T is
TT <- which(start_weather$precipitation_inches == "T")
start_weather$precipitation_inches <- replace(start_weather$precipitation_inches, TT, 0.011)
# make precipitation numeric
start_weather$precipitation_inches <- as.numeric(start_weather$precipitation_inches)

# remove events for correlation
# remove columns added that were not needed (events and zip codes)
start_weather2 <- start_weather[,!(names(start_weather) %in% c("events", "zip_code"))]

# make city a factor
start_weather2$city <- factor(start_weather2$city)

# summarize start_weather2
summary(start_weather2)
# get the dimenstions of start_weather2
dim(start_weather2)

# make a data frame for each city for the correlation analysis and only allow numeric columns
Mountain_view <- start_weather2 %>% filter(city == "Mountain View") %>%
  select_if(is.numeric)
Palo_alto <- start_weather2 %>% filter(city == "Palo Alto") %>%
  select_if(is.numeric)
Redwood_city <- start_weather2 %>% filter(city == "Redwood City") %>%
  select_if(is.numeric)
San_fran <- start_weather2 %>% filter(city == "San Francisco") %>%
  select_if(is.numeric)
San_jose <- start_weather2 %>% filter(city == "San Jose") %>%
  select_if(is.numeric)

####### Run correlation analysis #######
# install corrplot
install.packages("corrplot")
# add it to library
library(corrplot)

# set correlation matrix with cor
corMV <- cor(Mountain_view, use = "complete.obs")
corPA <- cor(Palo_alto, use = "complete.obs")
corRC <- cor(Redwood_city, use = "complete.obs")
corSF <- cor(San_fran, use = "complete.obs")
corSJ <- cor(San_jose, use = "complete.obs")

# Warning message from corMV, corRC, and corSJ: stdev is zero
## check which column
apply(Mountain_view, 2, sd, na.rm = TRUE) # max_visibility_miles
apply(Redwood_city, 2, sd, na.rm = TRUE) # No category with 0 sd, but warning from cor() still occurs
apply(San_jose, 2, sd, na.rm = TRUE) #max_visibility_miles
## remove the affected column
Mountain_view2 <- Mountain_view[,!(names(Mountain_view) %in% c("max_visibility_miles"))]
San_jose2 <- San_jose[,!(names(San_jose) %in% c("max_visibility_miles"))]
Redwood_city2 <- Redwood_city[,!(names(Redwood_city) %in% c("max_visibility_miles"))]
corMV2 <- cor(Mountain_view2, use = "complete.obs")
corSJ2 <- cor(San_jose2, use = "complete.obs")
corRC2 <- cor(Redwood_city2, use = "complete.obs")

# plot with corrplot
corrplot(corMV2, method = 'circle', type = 'lower', 
         addCoef.col ='black', tl.col = 'black', number.cex = 0.8, order = 'alphabet', diag=FALSE,
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
corrplot(corPA, method = 'circle', type = 'lower', 
         addCoef.col ='black', tl.col = 'black', number.cex = 0.8, order = 'alphabet', diag=FALSE,
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
corrplot(corRC2, method = 'circle', type = 'lower', 
         addCoef.col ='black', tl.col = 'black', number.cex = 0.8, order = 'alphabet', diag=FALSE,
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
corrplot(corSF, method = 'circle', type = 'lower', 
         addCoef.col ='black', tl.col = 'black', number.cex = 0.8, order = 'alphabet', diag=FALSE,
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
corrplot(corSJ2, method = 'circle', type = 'lower', 
         addCoef.col ='black', tl.col = 'black', number.cex = 0.8, order = 'alphabet', diag=FALSE,
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
## method = 'circle' creates a circle with size corresponding to strength of the correlation
## type = 'lower' creates the staircase appearance
## addCoef.col = 'black' adds black correlation numbers
## tl.col = 'black' makes column names black
## number.cex = 0.8 makes the correlation numbers less bold (thins the font)
## order = 'alphabet' orders alphabetically
## tl.srt = 45 tilts text to a 45 degree angle
## col = COL2('PiYG', 10)) specifies the colour scale


