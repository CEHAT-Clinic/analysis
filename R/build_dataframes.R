#' Clean Raw Sensor Data
#'
#' This function cleans up the raw readings from the PA sensors and sets up a data frame.
#' It assumes that the data has the columns
#' c('timestamp', "channelAPm25", "channelBPm25", "humidity", "latitude", "longitude") in that order.
#' It also uses an EPA correction factor for PurpleAir sensors.
#' Any readings with faulty channel A/B values will be dropped
#'
#' @param data a .csv of PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the full PurpleAir sensor data, with corrected timezone and combined channels
#' @export

cleanPA <- function(data){
  time_clean = lubridate::ymd_hms(data$timestamp, tz="America/Los_Angeles")

  data$timestamp <- time_clean
  data <- data[order(data$timestamp),]
  data[,5] <- round(data[,5],5)
  data[,6] <- round(data[,6],4)

  #throwing out unusable data (by EPA standard)
  data$avgPM <- (data$channelAPm25+ data$channelBPm25)/2
  data$difference <- abs(data$channelAPm25- data$channelBPm25)
  data$pct_difference <- data$difference/ data$avgPM

  percent_threshold <- 0.7
  raw_threshold <- 5

  data <- dplyr::filter(data, difference <= raw_threshold & pct_difference <= percent_threshold)

  #EPA correction method for PurpleAir sensors
  data$PM2.5 <- 0.534 * data$avgPM - 0.0844 * data$humidity + 5.604
  data <- data[,c(1,10,4:6)]
  data <- dplyr::filter(data, PM2.5 >= 0)

  data
}


#' Aggregate Data by Hour
#'
#' This function aggregates the frequent readings from each sensor and averages them by hour.
#' It assumes that the data has the columns 'timestamp' and 'longitude'.
#' It also uses an EPA correction factor for PurpleAir sensors.
#' Any readings with faulty channel A/B values will be dropped
#'
#' @param data a CLEANED dataframe of PurpleAir sensor data
#' @return a dataframe of the hourly PA sensor data, with sensor names, days of the week, hour, and time of day
#' @export

hourlyPA <- function(data){
  #-------------------------------------------------------------------------------------#
  sensors <- unique(data[,c('longitude','latitude')])
  sensors <- dplyr::mutate(sensors,
                    names = dplyr::case_when(longitude == -118.1901 & latitude == 33.94106 ~ "Sensor: SCSG-14",
                                      longitude == -118.1953 & latitude == 33.94354 ~ "Sensor: CEHAT 7-CD",
                                      longitude == -118.2201 & latitude == 33.94178 ~ "Sensor: CEHAT-01",
                                      longitude == -118.1985 & latitude == 33.96063 ~ "Sensor: CEHAT 5",
                                      longitude == -118.2184 & latitude == 33.96757 ~ "Sensor: CCA Mountainview and Olive",
                                      longitude == -118.2146 & latitude == 33.95058 ~ "Sensor: CEHAT-St. Helens-STEM",
                                      longitude == -118.1685 & latitude == 33.93553 ~ "Sensor: SCSG_15",
                                      longitude == -118.1673 & latitude == 33.92019 ~ "Sensor: SCSG_20",
                                      longitude == -118.2225 & latitude == 33.95094 ~ "Sensor: CEHAT 7-SE",
                                      longitude == -118.1965 & latitude == 33.93868 ~ "Sensor: CEHAT 8",
                                      longitude == -118.2181 & latitude == 33.96192 ~ "Sensor: CEHAT 3")
  )

  sensorNum <- nrow(sensors)
  #-------------------------------------------------------------------------------------#

  data$timestamp <- cut(data$timestamp, breaks="hour") #uses the "timestamp" column

  PAhourly <- data.frame(Date=as.Date(character()),
                         PM2.5=double(),
                         humidity=double(),
                         latitude=double(),
                         longitude=double())

  #aggregate the data to get hourly averages per sensor location
  for (i in 1:sensorNum) {
    agg <- aggregate(cbind(PM2.5, humidity, latitude, longitude) ~ timestamp,
                     data = data[data$longitude == sensors[i,1],], mean)

    PAhourly <- rbind(PAhourly,agg)
  }

  PAhourly <- PAhourly[order(PAhourly$timestamp),]
  PAhourly[,2] <- round(PAhourly[,2],2)
  PAhourly[,3] <- round(PAhourly[,3],2)

  PAhourly$timestamp <- lubridate::ymd_hms(as.character(PAhourly$timestamp)) #change class from factor to POSIXct

  #add days field so that we can aggregate max and min values by day. do this with hours as well
  PAhourly$day <- lubridate::mday(PAhourly$timestamp)
  PAhourly$hour <- lubridate::hour(PAhourly$timestamp)
  PAhourly$weekday <- lubridate::wday(PAhourly$timestamp, label=T, abbr = F)

  #map the time of day to corresponding timestamps
  PAhourly <- dplyr::mutate(PAhourly,
                     timeofday = dplyr::case_when(hour >= 0 & hour <= 5 ~ "night",
                                           hour >= 6 & hour <= 11 ~ "morning",
                                           hour >= 12 & hour <= 17 ~ "afternoon",
                                           hour >= 18 & hour <= 23 ~ "evening" )
  )

  PAhourly <- dplyr::left_join(PAhourly, sensors, by = c("latitude","longitude"), keep= F)

  PAhourly
}





#' Hourly Summary Data for South Gate
#'
#' This function aggregates data (output from the hourlyPA/cleanPA function) by hour for mean, median, max, min, and count of sensors
#' It assumes that the data has the columns 'timestamp', 'day', 'hour' 'PM2.5.
#'
#' @param data the output dataframe of the hourlyPA or cleanPA functions
#' @return a dataframe of the holistic South Gate data, including *weighted* sensor averages, sensor median, the max, and the min, and active count.
#' @export

summarySG <- function(data) {

  data$timestamp <- cut(data$timestamp, breaks="hour") #uses the "timestamp" column

  data$timestamp <- lubridate::ymd_hms(as.character(data$timestamp))

  avgSG <- aggregate(cbind(PM2.5, lubridate::hour(timestamp),lubridate::mday(timestamp)) ~ timestamp,
                     data = data[lubridate::month(data$timestamp) >1,],
                     FUN=function(x) c(mean=round(mean(x),2), median = round(median(x),2), count =round(length(x),0), max = round(max(x),2), min=round(min(x),2)  ))

  PM2.5.median <- avgSG$PM2.5[,2]
  PM2.5.count <- avgSG$PM2.5[,3]
  PM2.5.max <- avgSG$PM2.5[,4]
  PM2.5.min <- avgSG$PM2.5[,5]

  avgSG <- data.frame()

  avgSG <- aggregate(cbind(PM2.5, lubridate::hour(timestamp),lubridate::mday(timestamp)) ~ timestamp,
                     data = data[lubridate::month(data$timestamp) >1,], FUN= function(x) {round(mean(x),2)} )

  avgSG <- cbind(avgSG, PM2.5.median, PM2.5.max, PM2.5.min, PM2.5.count)

  colnames(avgSG) <- c('timestamp', "average_PM2.5", "hour", 'day', "median_PM2.5", "max", "min", "active sensors")

  avgSG

}


#' Collect Highs and Lows for PM2.5 per day
#'
#' This function collects the daily high and low values of PM2.5 for each sensor into a dataframe.
#' It assumes that the data has the columns 'timestamp' and 'longitude'.
#' Any identical readings of duplicate type will be dropped
#'
#' @param data a dataframe of hourly PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the highs and lows of PurpleAir sensor data, organized by sensor
#' @export

highslows <- function(data) {

  sensors <- unique(data[,c('longitude','latitude')])

  sensorNum <- nrow(sensors)

  hi_lo <- data.frame()

  for (i in 1:sensorNum) {
    high <- aggregate(cbind(PM2.5, latitude, longitude) ~ day,
                      data = data[data$longitude == sensors[i,1] & lubridate::month(data$timestamp) >1,], max)

    nhighs <- nrow(high)
    type <- c(type = rep(c("high"), nhighs))
    high$type <- type

    low <- aggregate(cbind(PM2.5, latitude, longitude) ~ day,
                     data = data[data$longitude == sensors[i,1] & lubridate::month(data$timestamp)>1, ], min)

    nvals <- nrow(low)
    type <- c(type = rep(c("low"), nvals))
    low$type <- type

    hi_lo <- rbind(hi_lo,high,low)
  }

  hi_lo <- dplyr::left_join(hi_lo, data, by = c("PM2.5", "day","latitude","longitude"), keep= F)
  hi_lo <- hi_lo[order(hi_lo$day),]

  hi_lo <- hi_lo[!duplicated(hi_lo[,c(1,2,5,11)]),]

  hi_lo
}


#' This function takes in data, specifically the name of a csv file with
#' timestamps, ChannelA, ChannelB, humidity, latitude and longitude
#' It then cleans the data frame and returns AQI values.
#' @param data, specifically the name of a csv file
#' @return it returns a dataframe with hourly time, AQI, humidity, latitude and longitude
#' @export
#'

AQIdataframe <- function(data){

  myData <- cleanPA(data)
  myAQIValues <- lapply(myData[2],aqiFromPm25)
  myAQIData <- data.frame(myData$datehour, myAQIValues, myData$humidity,
                          myData$latitude, myData$longitude)
  colnames(myAQIData) <- c("datehour","AQI","humidity","latitude","longitude")

  myAQIData
}





#'
#' This function cleans up the raw readings from the PA sensors and sets up a data frame.
#' It assumes that the data has the columns
#' c('timestamp', "channelAPm25", "channelBPm25", "humidity", "latitude", "longitude") in that order.
#' It also uses an EPA correction factor for PurpleAir sensors.
#' Any readings with faulty channel A/B values will be dropped
#'
#' @param data a .csv of PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the full PurpleAir sensor data (by the hour), with corrected timezone and combined channels
#' @export

hourlyCleanPA<- function(test1){

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
df <- dplyr::filter(df, difference <= raw_Threshold & difference/mean <= percent_Threshold)

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
timestamp = lubridate::ymd_hms(test$time,tz="America/Los_Angeles")

#reordering our dataframe based on time
test$time <- timestamp
test <- test[order(test$time),]

#rounding the longitude and latitude
latitude <- round(test[,4],4)
longitude <- round(test[,5],4)


#break up data by hour and put it in the dataframe
test$time <- cut(test$time, breaks="hour")
timestamp <- test$time
PAhourly <- data.frame(timestamp, PM2.5, humidity, latitude, longitude)

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
  agg <- aggregate(data,by=list(data$timestamp),FUN=mean)
  PAhourly1 <- rbind(PAhourly1,agg)}

#cleaning up our new dataframe
PAhourly1$timestamp <- PAhourly1$Group.1
PAhourly1$Group.1 <- NULL
PAhourly1$humidity <- round(PAhourly1[,3],1)
PAhourly1$PM2.5 <- round(PAhourly1[,2],0)
return (PAhourly1)
}






#' This function takes in data, specifically the name of a read csv file with
#' timestamps, PM2.5, humidity, latitude and longitude
#' It then cleans the changes the timestamps so that they match the
#' timstamps of the purple air data.
#' @param data, specifically the name of a csv file
#' @return it returns a dataframe with hourly time (in the format of Ymd hms),
#' PM2.5, humidity, latitude and longitude
#' @export

cleanAQMD <- function(data){
  data <- dplyr::filter(data, Value != "--")
  for (i in (1:length(data$Date.Time))){

    date1 <- data[i,1]

    if(substr(date1,10,10) == ' '){
      date <- substr(date1,1,10)
      date <- format(strptime(date, "%m/%d/%Y"), format="%Y-%m-%d")
      time <- substring(date1, 11,last = 1000000L)
      time <- format(strptime(time, "%I:%M:%S %p"), format="%H:%M:%S")
      data[i,1] <- paste(date, time,sep = ' ')
    }
    else {
      date <- substr(date1,1,11)
      date <- format(strptime(date, "%m/%d/%Y"), format="%Y-%m-%d")
      time <- substring(date1, 12,last = 1000000L)
      time <- format(strptime(time, "%I:%M:%S %p"), format="%H:%M:%S")
      data[i,1] <- paste(date, time,sep = ' ')
    }
  }
  return(data)
}




#' This function takes in cleaned purple air data, a dataframe with
#' timestamps, PM2.5, humidity, latitude and longitude, and a AQMD csv files.
#' It then combines these two files to make a dataframe with matching days.
#' @param SGdata and otherCitydata, two csv files
#' @return it returns a dataframe with matching days
#' @export

matchingDays <- function(SGdata, otherCitydata){
  otherCitydata <- cleanAQMD(otherCitydata)
  # finding all the non matching days among the two data frames
  nonMatchingDays <- SGdata$timestamp[!SGdata$timestamp %in% otherCitydata$Date.Time]

  nonMatchingDays1 <- otherCitydata$Date.Time[!otherCitydata$Date.Time %in% SGdata$timestamp]
  print(nonMatchingDays1)
  print(nonMatchingDays)
  `%notin%` <- Negate(`%in%`)

  # negating that to find the matching days

  Matching1 <- dplyr::filter(otherCitydata, Date.Time %notin% nonMatchingDays1)
  Matching2 <- dplyr::filter(SGdata, timestamp %notin% nonMatchingDays)

  #making the data frame with the PM2.5 values
  Matching1$PM2.5<- round(Matching2$PM2.5,0)

  ourData <- Matching1[-c(3,4)]
  names(ourData)[1] <- "timestamp"
  names(ourData)[2] <- "otherCityPM"
  names(ourData)[3] <- "southGatePM"
  ourData$otherCityPM <- as.numeric(as.character(ourData$otherCityPM))

  return(ourData)
}


#' Down Sensors?
#'
#' This reports which sensors were down and on which days that they went
#'
#' @param data a dataframe of hourly PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the highs and lows of PurpleAir sensor data, organized by sensor
#' @export
