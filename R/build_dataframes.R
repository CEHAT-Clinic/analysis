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





#' This function takes in five parameters: highIndexBreakpoint,lowIndexBreakpoint,
#' highConcentrationBreakpoint,lowConcentrationBreakpoint,pm25Concentration, in
#' that order. It then calculates the AQI guven the specified indices.
#' @param highIndexBreakpoint,lowIndexBreakpoint, highConcentrationBreakpoint,lowConcentrationBreakpoint,pm25Concentration
#' @return it returns the AQI that corresponds to the given PM2.5 measurement
#' @export


indexCalculation <-function(highIndexBreakpoint,lowIndexBreakpoint,
                            highConcentrationBreakpoint,lowConcentrationBreakpoint,pm25Concentration){
  
  indexRange <- highIndexBreakpoint - lowIndexBreakpoint
  
  concentrationRange <- highConcentrationBreakpoint - lowConcentrationBreakpoint
  rangeRelativeConcentration <- pm25Concentration - lowConcentrationBreakpoint
  
  return (
    (indexRange / concentrationRange) * rangeRelativeConcentration +
      lowIndexBreakpoint)
}



#' This function takes in one parameters:pm25Concentration.
#'  It then calculates the AQI given that specified value.
#' @param pm25Concentration
#' @return it returns the AQI that corresponds to the given PM2.5 measurement
#' @export



aqiFromPm25 <- function(pm25Concentration) {
  # Source of bound values is Table 6 of the paper at
  # https://www.airnow.gov/sites/default/files/2018-05/aqi-technical-assistance-document-may2016.pdf
  # EPA formulas require PM 2.5 to be truncated to one decimal place
  
  truncatedPm25 <- floor(10 * pm25Concentration) / 10
  
  aqi <-  0
  highAqiBound <-  0
  lowAqiBound <-  0
  highPmBound <-  0
  lowPmBound <-  0
  
  # Assign appropriate bounds
  if (truncatedPm25 < 12.1) {
    highAqiBound <-  50
    lowAqiBound <-  0
    highPmBound <-  12
    lowPmBound <-  0
  } else if (truncatedPm25 < 35.5) {
    highAqiBound <-  100
    lowAqiBound <-  51
    highPmBound <-  35.4
    lowPmBound <-  12.1
    
  } else if (truncatedPm25 < 55.5) {
    highAqiBound <-  150
    lowAqiBound <-  101
    highPmBound <-  55.4
    lowPmBound <-  35.5
    
  } else if (truncatedPm25 < 150.5) {
    highAqiBound <-  200
    lowAqiBound <-  151
    highPmBound <-  150.4
    lowPmBound <-  55.5
    
  } else if (truncatedPm25 < 250.5) {
    highAqiBound <-  300
    lowAqiBound <-  201
    highPmBound <-  250.4
    lowPmBound <-  150.5
    
  } else if (truncatedPm25 < 350.5) {
    highAqiBound <-  400
    lowAqiBound <-  301
    highPmBound <-  350.4
    lowPmBound <-  250.5
    
  } else if (truncatedPm25 < 500.5) {
    highAqiBound <-  500
    lowAqiBound <-  401
    highPmBound <-  500.4
    lowPmBound <-  350.5
  }
  
  # Values beyond the range are indicated by infinite values
  if (truncatedPm25 < 0) {
    aqi <-  -Inf
  } else if (truncatedPm25 >= 500.5) {
    aqi <-  Inf
  } else {
    aqi <-  indexCalculation(
      highAqiBound,
      lowAqiBound,
      highPmBound,
      lowPmBound,
      truncatedPm25
    )
  }
  # /* eslint-enable no-magic-numbers */
  return (round(aqi, digits = 0))
}


#' This function takes in one parameter:degrees.
#'  It then calculates the radian of that specified value.
#' @param degrees
#' @return it returns the radian value of that degree value
#' @export

toRad <- function(degrees) {
  return ((pi*degrees)/180)
}



#' This function takes in four parameters: lat_1,long_1,lat_2,long_2.
#' It then calculates the distance between these two locations using
#' the Haversine formula.
#' @param latitude of the first location, longitude of the first location, latitude
#' of the second location, longitude of the second location
#' @return it returns the distance between these locations in miles
#' @export
#'

distance <- function(lat_1,long_1,lat_2,long_2) {
  # converting all degrees to radians
  lat1 <- toRad(lat_1)
  long1 <- toRad(long_1)
  lat2 <- toRad(lat_2)
  long2 <- toRad(long_2)
  
  # calculating dist using the Haversine Formula
  answer <- 2 * asin(sqrt((sin((lat2 - lat1)/2))^2 +
                            cos(lat1) * cos(lat2)*
                            (sin((long2 - long1)/2))^2))
  
  # use 6371 instead of 3956 to calculate in kilometers
  return (answer*3956)
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
  
  return (myAQIData)
}



#' This function takes in data, specifically the name of a csv file with
#' timestamps, PM2.5, humidity, latitude and longitude
#' It then cleans the changes the timestamps so that they match the
#' timstamps of the purple air data.
#' @param data, specifically the name of a csv file
#' @return it returns a dataframe with hourly time (in the format of Ymd hms),
#' PM2.5, humidity, latitude and longitude
#' @export
#'
cleanAQMD <- function(data){
  
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




#' Down Sensors?
#'
#' This reports which sensors were down and on which days that they went
#'
#' @param data a dataframe of hourly PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the highs and lows of PurpleAir sensor data, organized by sensor
#' @export
