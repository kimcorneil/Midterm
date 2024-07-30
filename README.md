# Midterm - BTC1855H
# Kimberly Corneil
# August 5, 2024

####### MY MASTER PLAN #######
# Read the .csv files as data frames and load them into the environment
# Explore the data with an EDA (following the steps in the blog outlined in the assignment)
# With each significant alteration I will name a new variable (ie if I have mydata and I remove all rows with NA I will call this mydata2)
# remove any cancelled trip by making any durations less than 3 minutes NA and removing the affected rows
## I will move the affected rows IDs to a separate data frame so they can be easily located
# remove outliers (subject to change after data analysis, but tentitive definition is anything outside of the 95th percentile)
## I will move the affected rows IDs to a separate data frame so they can be easily located
# make a histogram of volume (tentitively with 30 minute intervals) on weekdays
## use lubridate to sort out weekends
## sort by station to determine the 10 most often start and end stations within those hours
## do the steps from lines 12-14 for weekends
# determine the average utilization of bikes per month
## use lubridate to convert to proper dates for an easier breakdown
# combine trip data and weather data
## look at correlations between weather and bike rentals
## deal with trips starting in one city and ending in another 
## flag the highest to be pursued further
# prepare the proper visauls and reports
# submit the assignment
# celebrate 
