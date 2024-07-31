# Script for the midterm
# August 5, 2024
# BTC1855H
# Kimberly Corneil 

# Load the data
trip <- read.csv("trip.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv")

####### Exploratory Data Analysis #######
# Follow the blog post outlined in the assignment description
# Install the required packages and add them to library
library(tidyverse)
install.packages("funModeling")
install.packages("Hmisc")
library(funModeling)
library(Hmisc)

# summarize the data
summary(trip)
summary(station)
summary(weather)

# View the data
View(trip)
View(station)
View(weather)

# use status() to view types of data, zeros, NAs, and infinite numbers
status(trip)
status(station)
status(weather)

# use freq to examine frequency and percents of all categorical and factors
freq(trip)
freq(station)
freq(weather)

# use plot_num to visually analyze numerical variables
plot_num(trip)
plot_num(station)
plot_num(weather)

# ensure the columns were named appropriately
names(trip)
names(station)
names(weather)

# some stuff I would like to clean: 
#' trip: ID, start and end station ID, and bike ID should not be numeric
#' station: ID should not be numeric (unsure lat and long and dock_count, depends on later analysis)
#' weather: precipitation_inches should be numeric and zip_code should not be

