install.packages(c("lubridate", "ggplot2", "dplyr", "olsrr"))
install.packages("forecast")
install.packages("PerformanceAnalytics")

library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
library(olsrr)
library(PerformanceAnalytics)

ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")

# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")

#####Homework 5
#Question 1


