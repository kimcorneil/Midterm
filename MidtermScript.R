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
library(funModeling)
library(Hmisc)

# summarize the data
summary(trip)
summary(station)
summary(weather)

# get the dimensions the data
dim(trip)
dim(station)
dim(weather)

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

## determine which stations were installed in 2014
late_start <- station[grep("2014", station$installation_date), ]
print(late_start)

# use plot_num to visually analyze numerical variables
plot_num(trip)
plot_num(station)
plot_num(weather) 

## Check that every ID value in trip is unique
unique_ID <- unique(trip$id)
length(unique_ID) # length is 326339 which is the same number of observations in trip

## Check that every ID value in station is unique
unique_ID2 <- unique(station$id)
length(unique_ID2) # length is 70 which is the same number of observations 

## analyze numeric distributors of weather by city by using filter from dyplr to separate by city name
MV <- weather %>% filter(city == "Mountain View") 
PA <- weather %>% filter(city == "Palo Alto")
RC <- weather %>% filter(city == "Redwood City")
SF <- weather %>% filter(city == "San Francisco")
SJ <- weather %>% filter(city == "San Jose")

plot_num(MV)
plot_num(PA)
plot_num(RC)
plot_num(SF)
plot_num(SJ)

# ensure the columns were named appropriately
names(trip)
names(station)
names(weather)


