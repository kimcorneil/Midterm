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

