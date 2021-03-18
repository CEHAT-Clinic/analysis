suppressPackageStartupMessages({
  library(ggplot2)
  library(geoR)
  library(data.table)
  library(ipdw)
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
  
})

# reading the csv file
test1 <- read.csv("december2020_readings.csv")

#defining relevant variables from csv file
chA <- test1$channelAPm25
chB <- test1$channelBPm25
longitude <- test1$longitude
latitude <- test1$latitude
time <- test1$timestamp
humidity <- test1$humidity

#making dataframe 
df <- data.frame(time,chA,chB,longitude,latitude,humidity)

#defining constants
raw_Threshold = 5
percent_Threshold = .7

#taking the mean and difference of channelA and channelB at every row
df$mean <- rowMeans(df[,c('chA','chB')],na.rm=TRUE) 
df$difference <- abs(df$chA - df$chB)

#filtering out the numbers that exceed the thresholds
df <- df%>% 
  filter(difference <= raw_Threshold & difference/mean <= percent_Threshold)

#redefining PM2.5 according to the EPA equation
df$PM2.5 <- df$mean * .534 - df$humidity * .0844 + 5.604

#redefining our variables, now that we have the valid values
time<- df$time
PM2.5 <- df$PM2.5
humidity <- df$humidity
latitude <- df$latitude
longitude <- df$longitude

#making our new dataframe
test <- data.frame(time,PM2.5,humidity,latitude,longitude)

#divide the time into year,month,day, hour, minute, seconds
timestamp = lubridate::ymd_hms(test$time, tz="America/Los_Angeles")

#reordering our dataframe based on time
test$time <- timestamp
test <- test[order(test$time),]

#rounding the longitude and latitude 
latitude <- round(test[,4],4)
longitude <- round(test[,5],4)

#break up data by hour and put it in the dataframe
test$time <- cut(test$time, breaks="hour")
datehour <- test$time 
PAhourly <- data.frame(datehour, PM2.5, humidity, latitude, longitude)

PAhourly1 <- data.frame(Date=as.Date(character()),
                        PM2.5 = double(),
                        humidity=double(),
                        latitude=double(),
                        longitude=double())

#get the different sensor locations
locations <- unique(PAhourly[,c(5,4)])
sensorNum <- nrow(locations)

#aggregate the data to get hourly averages per sensor location
for (i in 1:sensorNum) {
  
  data <- PAhourly[latitude == locations[i,2],]
  agg <- aggregate(data,by=list(data$datehour),FUN=mean)
  PAhourly1 <- rbind(PAhourly1,agg)}

#cleaning up our new dataframe
PAhourly1$datehour <- PAhourly1$Group.1 
PAhourly1$Group.1 <- NULL
PAhourly1$humidity <- round(PAhourly1[,3],1)

#cleaned PM2.5 data
myData <- PAhourly1

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

