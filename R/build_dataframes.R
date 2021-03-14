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
#' This function aggregates HOURLYPA data (output from the hourlyPA function) by hour for mean, median, and count of sensors
#' It assumes that the data has the columns 'timestamp', 'day', 'hour' 'PM2.5.
#'
#' @param data the output dataframe of the hourlyPA function
#' @return a dataframe of the South Gate data, including *weighted* sensor averages, sensor median, the max, and the min, and active count.
#' @export

summarySG <- function(data) {

  avgSG <- aggregate(cbind(PM2.5, hour,day) ~ timestamp,
                     data = data[lubridate::month(data$timestamp) >1,],
                     FUN=function(x) c(mean=round(mean(x),2), median = round(median(x),2), count =round(length(x),0), max = max(x), min=min(x)  ))

  PM2.5.median <- avgSG$PM2.5[,2]
  PM2.5.count <- avgSG$PM2.5[,3]
  PM2.5.max <- avgSG$PM2.5[,4]
  PM2.5.min <- avgSG$PM2.5[,5]

  avgSG <- data.frame()

  avgSG <- aggregate(cbind(PM2.5, hour,day) ~ timestamp,
                     data = data[month(data$timestamp) >1,], FUN= function(x) {round(mean(x),2)} )

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







#' Down Sensors?
#'
#' This reports which sensors were down and on which days that they went
#'
#' @param data a dataframe of hourly PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the highs and lows of PurpleAir sensor data, organized by sensor
#' @export
