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

# use plot_num to visually analyze numerical variables
plot_num(trip)
plot_num(station)
plot_num(weather) 

## analyze numeric distributors of weather by city by using filter from dypler to separate by city name
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


