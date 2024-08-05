# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Determining Outliers - Trip #######
# for trip: outlier determination not required for: 
## ID is an identifier for the trip so outliers do not apply
## start_date and end_date are within a defined time period and outliers do not apply
## start_station_name, start_station_id, end_station_name, and end_station_id, and zip_code are for with a specified region 

# check if subscription_type has outliers by making it a factor and checking the number of levels
trip2$subscription_type <- factor(trip2$subscription_type)
levels(trip2$subscription_type)
## there are only 2 levels so outliers do not apply

# make trip3 from trip2 to leave trip2 unaffected from outlier removal
trip3 <- trip2
# Determine outliers for duration
# make a histogram to view data distribution
hist(trip3$duration) 
## notice in the histogram that there is just one big bin, indicates large range and potential outliers
summary(trip3$duration)
## notice the max is 17270400 (assumed seconds), which is:
((17270400/60)/60)/24 # almost 200 days 
## sort the data to see 10 largest values - good starting place
sort(trip3$duration,decreasing = T)[1:10] # sort trip3$duration by decreasing values and subset to show top 10 largest values
## notice some very large values 
## since the bottom of the data was cleaned previously (by removing excessively short trips that were likely cancelled) only deal with outliers on the top
### verify this by quickly viewing 10 smallest values
sort(trip3$duration,decreasing = F)[1:10]
### values are 61-66 seconds, reasonable trip duration

## View the histogram on a logarithmic scale to see distribution without influence of outliers
hist(log10(trip3$duration), main = "Historgam of Bike Ride Duration", xlab = "Log10 Duration (Seconds)")

# analyze data outside of middle 95% with quanatile
quantile(trip3$duration, 0.025) # view the number that defines the bottom 2.5%, 171 seconds
quantile(trip3$duration, 0.975) # view the number that defines the bottom 2.5%, 4025 seconds
# analyze data at the top 1% with quanatile
quantile(trip3$duration, 0.99) # view the number that defines the bottom 2.5%, 9478 seconds is about 2.6 hours 

# Find out how many seconds 7 hours is for the 7-hour cut off (see report for why 7 hour cut off)
60*60*7 # 25200 is 7 hours

# create trip_outliers3 containing all the trips of 7 hours or longer
Trips_outliers3 <- trip3 %>% filter(trip3$duration >= 25200)
# get the number of outliers to be removed
nrow(Trips_outliers3)
# just save IDs
Trips_outlier_IDs3 <- data.frame(Trips_outliers3$id)
# save outliers IDs as a file
write_csv(Trips_outlier_IDs3, "trips_outliers4.csv")

# remove trips over 7 hours from trip3
trip3 <- trip3 %>% filter(trip3$duration < 25200)

# Create new histograms after cleaning duration
hist(trip3$duration, main = "Historgam of Bike Ride Duration", xlab = "Duration (Seconds)")
hist(log10(trip3$duration),  main = "Historgam of Bike Ride Duration", xlab = "Log 10 Duration (Seconds)") 

####### Determining Outliers - Station #######
summary(station)
# irrelevant, see report for justification

####### Determining Outliers - Weather #######
summary(weather)
# only outliers in wind and gust speeds, see report for justification

# Replace any wind speeds above 75 mph in max_wind and max_gust speed with NA
## Duplicate weather so the original data set remains unaffected
clean_weather <- weather
ufos7$country[ufos7$country == ""] <- NA
clean_weather$max_wind_Speed_mph[clean_weather$max_wind_Speed_mph > 75] <- NA
clean_weather$max_gust_speed_mph[clean_weather$max_gust_speed_mph > 75] <- NA
hist(clean_weather$max_wind_Speed_mph, main = "Histogram of Maximum Wind Speeds", xlab = "Maximum Wind Speed (mph)")
hist(clean_weather$max_gust_speed_mph, main = "Histogram of Maximum Gust Speeds", xlab = "Maximum Gust Speed (mph)")
