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
## !!!!!!!start_station_name, start_station_id, end_station_name, and end_station_id, and zip_code are for with a specified region so outliers DO NOT SEEM RELEVANT
#### ASK THE ABOVE IN CLASS
# check if subscription_type has outliers by making it a factor and checking the number of levels
trip2$subscription_type <- factor(trip2$subscription_type)
levels(trip2$subscription_type)
## there are only 2 levels so outliers do not apply

# make trip3 from trip2 to leave trip2 unaffected from outlier removal
trip3 <- trip2
# Determine outliers for duration
# make a histogram to view data distribution
hist(trip3$duration) ### KIMBERLY MAKE THIS NICE IF YOU WANT TO INCLUDE IT IN REPORT
## notice in the histogram that there is just one big bin, indicates large range and potential outliers
summary(trip3$duration)
## notice the max is 17270400 (assumed seconds), which is:
((17270400/60)/60)/24 # qlmost 200 days CAN I MATH
## sort the data to see 10 largest values - good starting place
sort(trip3$duration,decreasing = T)[1:10]
## notice some very large values 
## since the bottom of the data was cleaned previously (by removing excessively short trips that were likely cancelled) only deal with outliers on the top
### verify this by quickly viewing 10 smallest values
sort(trip3$duration,decreasing = F)[1:10]
### notice they are 60 or 61 and there are multiple of the same number

## View the historgram on a logarithmic scale to see proper distribution
hist(log10(trip3$duration)) ### KIMBERLY MAKE THIS NICE IF YOU WANT TO INCLUDE IT IN REPORT
## notice very small bins on left, right skewed (RIGHT) ## LOWKEY AND ONE ON THE RIGHT SO MAYBE CHANGE THE ABOVE
boxplot(log10(trip3$duration)) ## WHAT THE ACTUAL FRIG

# analyze data outside of middle 95% with qunatile
quantile(trip3$duration, 0.025) #170
quantile(trip3$duration, 0.975) #5109
# with this method all values below 165 and above 5085 are considered outliers
5085/60 # an 84.75 minute bike ride is not unreasonable, so 95% may be too high
# once again not removing bottom because we already dealt with cancelled trip and a 1-2 minute bike ride is not unreasonable between stations - ie simple google maps for ID 4299 shows they are a 1 min bike ride apart
# view top 2%
quantile(trip3$duration, 0.98) #6726 is still reasonable
quantile(trip3$duration, 0.99) #12990 is about 3.6 hours which is verging on excessive, so try it as 99%

# move all the rows from the top 1%
Trips_outliers <- trip3 %>% filter(trip3$duration >= quantile(trip3$duration, 0.99))
# get the number of outliers to be removed
nrow(Trips_outliers)
# just save IDs
Trips_outlier_IDs <- data.frame(Trips_outliers$id)
# save outliers IDs as a file
write_csv(Trips_outlier_IDs, "trips_outliers.csv")

# remove top 1% of trip durations
## set them to NA
trip3$duration[trip3$duration >= quantile(trip3$duration, .99)]<- NA 
## remove NAs
trip3 <- trip3 %>% drop_na(duration)
## new historgam
hist(trip3)

####### Determining Outliers - Station #######
summary(station)
# irrelevant

####### Determining Outliers - Weather #######
summary(weather)
# maybe in wind degrees or events but otherwise seems pretty reasonable to me
## MAYBE NOT THOUGH; run tests and shit



