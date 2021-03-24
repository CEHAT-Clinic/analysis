suppressPackageStartupMessages({
  library(ggplot2)
  library(geoR)
  library(data.table)
  library(gridGraphics)
  library(gridExtra)
  library(lattice)
  #for the mapping
  library(dplyr)
  library(gdata)
  library(sf)
  library(tigris)
  #you need to let R know to bring in the spatial data as sf objects
  options(tigris_class = "sf")
  library(tmap)
  library(raster)
  library(Ecdat)
  library(boot)
  library(lubridate)
  library(caret)
  # detach(package:gstat)
  library(gstat)
  library(Metrics)
  library(zoo)
  library(usethis)
  library(testthat)
  library(devtools)
  devtools:: install_github("CEHAT-Clinic/analysis")
  library(PurpleAirCEHAT)
})

# reading the csv file
test1 <- read.csv("december2020_readings.csv")

myData <- test1
# *************find the days that have readings over the EPA threshold***************

# for this, specifically, we are looking at 24 hour average periods
# that surpass the EPA threshold in December

Sensor1 <- filter(myData, latitude == '33.9509')
Sensor2 <- filter(myData, latitude == '33.9606')
Sensor3 <- filter(myData, latitude == '33.9498')
Sensor4 <- filter(myData, latitude == '33.9619')
Sensor5 <- filter(myData, latitude == '33.9411')

# finds the rolling mean for each sensor
sensor1rm <- data.frame(rollingmean = rollmean(Sensor1$PM2.5, 24))
sensor2rm <- data.frame(rollingmean = rollmean(Sensor2$PM2.5, 24))
sensor3rm <- data.frame(rollingmean = rollmean(Sensor3$PM2.5, 24))
sensor4rm <- data.frame(rollingmean = rollmean(Sensor4$PM2.5, 24))
sensor5rm <- data.frame(rollingmean = rollmean(Sensor5$PM2.5, 24))

# adding the necessary null values into the dataframes
# so that we can merge it to the larger df
sensor1rm[nrow(sensor1rm)+ (length(Sensor1$PM2.5) - length(sensor1rm$rollingmean)),] <- NA
sensor2rm[nrow(sensor2rm)+ (length(Sensor2$PM2.5) - length(sensor2rm$rollingmean)),] <- NA
sensor3rm[nrow(sensor3rm)+ (length(Sensor3$PM2.5) - length(sensor3rm$rollingmean)),] <- NA
sensor4rm[nrow(sensor4rm)+ (length(Sensor4$PM2.5) - length(sensor4rm$rollingmean)),] <- NA
sensor5rm[nrow(sensor5rm)+ (length(Sensor5$PM2.5) - length(sensor5rm$rollingmean)),] <- NA

# merging the dataframes together so that it includes rolling mean
Sensor1 <- bind_cols(Sensor1, sensor1rm)
Sensor2 <- bind_cols(Sensor2, sensor2rm)
Sensor3 <- bind_cols(Sensor3, sensor3rm)
Sensor4 <- bind_cols(Sensor4, sensor4rm)
Sensor5 <- bind_cols(Sensor5, sensor5rm)


# finds the days that the rolling mean is over the EPA threshold
# for 24hr, it's 35 micrograms per cubic meter
daysOverSensor1 <- filter(Sensor1, rollingmean >= 35)
daysOverSensor2 <- filter(Sensor2, rollingmean >= 35)
daysOverSensor3 <- filter(Sensor3, rollingmean >= 35)
daysOverSensor4 <- filter(Sensor4, rollingmean >= 35)
daysOverSensor5 <- filter(Sensor5, rollingmean >= 35)

#plotting for the rolling mean
plot(Sensor1$datehour, Sensor1$rollingmean)

