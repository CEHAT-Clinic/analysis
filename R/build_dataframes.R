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


#'
#' This function cleans up the raw readings from the PA sensors and sets up a data frame.
#' It assumes that the data has the columns
#' c('Timestamp', "Names", "PM2.5", "Percent.Difference","Humidity", "Latitude",
#' "Longitude") in that order. It drops negative PM2.5 values and cleans the time to match LA time.
#' @param data a .csv of PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the full PurpleAir sensor data, with corrected timezone and PM2.5 levels
#' @export

newCleanPA <- function(data){

  colnames(data) <- c("timestamp","names","PM2.5","percent.diff","humidity","latitude","longitude")

  # converting to LA time
  time_clean = lubridate::ymd_hms(data$timestamp, tz="America/Los_Angeles")
  data$timestamp <- time_clean
  data <- data[order(data$timestamp),]

  #throwing out negative PM2.5 values
  data <- dplyr::filter(data, PM2.5 >= 0)

  #reorganizing the data frame

  data <- data[,c(1,3,5,6,7,2,4)]

  data

}

#' Aggregate Data by Hour
#'
#' This function aggregates the frequent readings from each sensor and averages them by hour.
#' It assumes that the data has the columns 'timestamp' and 'longitude'.
#' It also uses an EPA correction factor for PurpleAir sensors.
#' Any readings with faulty channel A/B values will be dropped
#'
#' @param data a CLEANED dataframe of PurpleAir sensor data a boolean specifying if we are using the new
#' @param new a boolean specifying if we are using the new data (in which case you put TRUE) or the old data (in which case you put FALSE)
#' @return a dataframe of the hourly PA sensor data, with sensor names, days of the week, hour, and time of day
#' @export

hourlyPA <- function(data,new){
  if (new == FALSE){
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
  }

  else{
  sensors <- unique(data[,c("longitude","latitude","names")])
  sensorNum <- length(unique(data$names))
  }

  #-------------------------------------------------------------------------------------#

  data$timestamp <- cut(data$timestamp, breaks="hour") #uses the "timestamp" column

  PAhourly <- data.frame(Date=as.Date(character()),
                         PM2.5=double(),
                         humidity=double(),
                         latitude=double(),
                         longitude=double()
                        )

  #aggregate the data to get hourly averages per sensor location
  if(new == TRUE){

  for (i in 1:sensorNum) {
    agg <- aggregate(cbind(PM2.5, humidity, latitude, longitude) ~ timestamp,
                     data = data[data$names == sensors[i,3],], mean)

    PAhourly <- rbind(PAhourly,agg)
  }
  }

  else{
    for (i in 1:sensorNum) {
      agg <- aggregate(cbind(PM2.5, humidity, latitude, longitude) ~ timestamp,
                       data = data[data$longitude == sensors[i,1],], mean)

      PAhourly <- rbind(PAhourly,agg)
    }}

  PAhourly <- PAhourly[order(PAhourly$timestamp),]
  PAhourly[,2] <- round(PAhourly[,2],2)
  PAhourly[,3] <- round(PAhourly[,3],2)

  PAhourly$timestamp <- lubridate::ymd_hms(as.character(PAhourly$timestamp),tz="America/Los_Angeles") #change class from factor to POSIXct

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

  PAhourly <- dplyr::mutate(PAhourly,
                            category = dplyr::case_when(PM2.5 >= 0 & PM2.5 <=12 ~ "Good",
                                                        PM2.5 >= 12.01 & PM2.5 <=35.4 ~ "Moderate",
                                                        PM2.5 >= 35.41 & PM2.5 <= 55.4 ~ "Unhealthy for Sensitive Groups",
                                                        PM2.5 >= 55.41 & PM2.5 <= 150.4 ~ "Unhealthy",
                                                        PM2.5 >= 150.41 & PM2.5 <= 250.4 ~ "Very Unhealthy",
                                                        PM2.5 >= 250.41 & PM2.5 <= 500 ~ "Hazardous",
                                                        PM2.5 >= 500.01 ~ "Hazardous+")
  )


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

  data$timestamp <- lubridate::ymd_hms(as.character(data$timestamp), tz ="America/Los_Angeles")

  avgSG <- aggregate(cbind(PM2.5, lubridate::hour(timestamp),lubridate::mday(timestamp)) ~ timestamp,
                     data = data,
                     FUN=function(x) c(mean=round(mean(x),2), median = round(median(x),2), count =round(length(x),0), max = round(max(x),2), min=round(min(x),2), range = range(x)  ))

  PM2.5.median <- avgSG$PM2.5[,2]
  PM2.5.count <- avgSG$PM2.5[,3]
  PM2.5.max <- avgSG$PM2.5[,4]
  PM2.5.min <- avgSG$PM2.5[,5]
  PM2.5.range <- PM2.5.max - PM2.5.min

  avgSG <- data.frame()

  avgSG <- aggregate(cbind(PM2.5, lubridate::hour(timestamp),lubridate::mday(timestamp)) ~ timestamp,
                     data = data[month(data$timestamp) >1,], FUN= function(x) {round(mean(x),2)} )

  avgSG <- cbind(avgSG, PM2.5.median, PM2.5.max, PM2.5.min, PM2.5.range, PM2.5.count)

  colnames(avgSG) <- c('timestamp', "average_PM2.5", "hour", 'day', "median_PM2.5", "max", "min", "range", "active sensors")
  avgSG <- dplyr::mutate(avgSG,
                         category = dplyr::case_when(average_PM2.5 >= 0 & average_PM2.5 <=12 ~ "Good",
                                                     average_PM2.5 >= 12.01 & average_PM2.5 <=35.4 ~ "Moderate",
                                                     average_PM2.5 >= 35.41 & average_PM2.5 <= 55.4 ~ "Unhealthy for Sensitive Groups",
                                                     average_PM2.5 >= 55.41 & average_PM2.5 <= 150.4 ~ "Unhealthy",
                                                     average_PM2.5 >= 150.41 & average_PM2.5 <= 250.4 ~ "Very Unhealthy",
                                                     average_PM2.5 >= 250.41 & average_PM2.5 <= 500 ~ "Hazardous",
                                                     average_PM2.5 >= 500.01 ~ "Hazardous+")
  )

  avgSG

}


#' Daily Summary Data for South Gate
#'
#' This function aggregates data (output from the hourlyPA/cleanPA function) by hour for mean, median, max, min, and count of sensors
#' It assumes that the data has the columns 'timestamp', 'day', 'hour' 'PM2.5.
#'
#' @param data the output dataframe of the hourlyPA or cleanPA functions
#' @return a dataframe of the holistic South Gate data, including *weighted* sensor averages, sensor median, the max, and the min, and active count.
#' @export

dailySG <- function(data) {

  data$timestamp <- cut(data$timestamp, breaks="hour") #uses the "timestamp" column

  data$timestamp <- lubridate::ymd_hms(as.character(data$timestamp), tz = "America/Los_Angeles")

  avgSG <- aggregate(cbind(PM2.5, longitude) ~ as.Date(timestamp),
                     data = data,
                     FUN=function(x) c(mean=round(mean(x),2), median = round(median(x),2), count =round(length(unique(x)),0), max = round(max(x),2), min=round(min(x),2)  ))

  PM2.5.median <- avgSG$PM2.5[,2]
  PM2.5.count <- avgSG$longitude[,"count"]
  PM2.5.max <- avgSG$PM2.5[,4]
  PM2.5.min <- avgSG$PM2.5[,5]
  PM2.5.range <- PM2.5.max - PM2.5.min

  avgSG <- data.frame()

  avgSG <- aggregate(cbind(PM2.5) ~ as.Date(timestamp),
                     data = data, FUN= function(x) {round(mean(x),2)} )

  avgSG <- cbind(avgSG, PM2.5.median, PM2.5.max, PM2.5.min, PM2.5.range, PM2.5.count)

  names(avgSG) <- c('day', "average_PM2.5", "median_PM2.5", "max", "min", "range", "active sensors")

  avgSG <- dplyr::mutate(avgSG,
                         category = dplyr::case_when(average_PM2.5 >= 0 & average_PM2.5 <=12 ~ "Good",
                                                     average_PM2.5 >= 12.01 & average_PM2.5 <=35.4 ~ "Moderate",
                                                     average_PM2.5 >= 35.41 & average_PM2.5 <= 55.4 ~ "Unhealthy for Sensitive Groups",
                                                     average_PM2.5 >= 55.41 & average_PM2.5 <= 150.4 ~ "Unhealthy",
                                                     average_PM2.5 >= 150.41 & average_PM2.5 <= 250.4 ~ "Very Unhealthy",
                                                     average_PM2.5 >= 250.41 & average_PM2.5 <= 500 ~ "Hazardous",
                                                     average_PM2.5 >= 500.01 ~ "Hazardous+")
  )

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
    high <- aggregate(cbind(PM2.5, latitude, longitude) ~ as.Date(timestamp, tz = "America/Los_Angeles"),
                      data = data[data$longitude == sensors[i,1],], max)

    nhighs <- nrow(high)
    type <- c(type = rep(c("high"), nhighs))
    high$type <- type

    low <- aggregate(cbind(PM2.5, latitude, longitude) ~ as.Date(timestamp, tz = "America/Los_Angeles"),
                     data = data[data$longitude == sensors[i,1], ], min)

    nvals <- nrow(low)
    type <- c(type = rep(c("low"), nvals))
    low$type <- type

    hi_lo <- rbind(hi_lo,high,low)
  }
  names(hi_lo)[1] <- "date"
  hi_lo$day <- lubridate::mday(hi_lo$date)

  hi_lo <- dplyr::left_join(hi_lo, data, by = c("PM2.5", "day","latitude","longitude"), keep= F)

  hi_lo <- hi_lo[order(hi_lo$date),]

  hi_lo <- hi_lo[!duplicated(hi_lo[,c("date","PM2.5","type","names")]),]

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





#' This function takess in data, specifically the name of a read csv file with
#' timestamps, PM2.5, humidity, latitude and longitude
#' It then cleans the changes the timestamps so that they match the
#' timstamps of the purple air data.
#' @param data, specifically the name of a csv file
#' @return it returns a dataframe with hourly time (in the format of Ymd hms),
#' PM2.5, humidity, latitude and longitude
#' @export

cleanAQMD <- function(data){
  # converting aqmd csv to usable data frame
  data <- as.data.frame(data)

  data <- data[-c(1,2),-c(5)]
  names(data)[1] <- 'Date.Time'
  names(data)[2] <- 'Value'
  data <- subset(data, select = -c(3,4))


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
  #data$Date.Time <- lubridate::ymd_hms(data$Date.Time, tz="America/Los_Angeles")
  data
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
  SGdata$timestamp <- strftime(SGdata$timestamp)

  nonMatchingDays <- SGdata$timestamp[!SGdata$timestamp %in% otherCitydata$Date.Time]

  nonMatchingDays1 <- otherCitydata$Date.Time[!otherCitydata$Date.Time %in% SGdata$timestamp]

  `%notin%` <- Negate(`%in%`)

  # negating that to find the matching days

  Matching1 <- dplyr::filter(otherCitydata, Date.Time %notin% nonMatchingDays1)
  Matching2 <- dplyr::filter(SGdata, timestamp %notin% nonMatchingDays)

  Matching1 <- Matching1[,-c(3,4)]
  #making the data frame with the PM2.5 values
  Matching1$southGatePM<- round(Matching2$average_PM2.5,0)

  names(Matching1)[1] <- "timestamp"
  names(Matching1)[2] <- "otherCityPM"
  Matching1$otherCityPM <- as.numeric(as.character(Matching1$otherCityPM))

  Matching1
}


#' This function takes in a data frame of values with columns timestamp,
#' otherCityPM, southGatePM, and PM2.5. It also takes in the name of the city
#' returns the results after conducting t-tests on them
#' @param two arrays of data with matching days and the name of the non-South Gate city
#' @return it returns the t test results of the data sets
#' @export

compareDataDF <- function(ourData,nameOfCity){
  daysOfMonth <- aggregate(cbind(otherCityPM, southGatePM) ~ lubridate::mday(timestamp),
                           data = ourData,
                           FUN=mean)
  names(daysOfMonth)[1] <- "day"

  df2 <- data.frame(day = c(daysOfMonth[,"day"], daysOfMonth[,"day"]),
                    city = c(rep(nameOfCity, times = length(daysOfMonth$day)),
                             rep("South Gate",times = length(daysOfMonth$day))),
                    PM2.5 = c(daysOfMonth[,"otherCityPM"],daysOfMonth[,"southGatePM"]))

  df2
}

#' This function takes in a data frame of values from one sensor and returns a dataframe
#' of the days where the PM2.5 values surpass the EPA threshold.
#' @param a dataframe of values from one sensor and the name of the PM2.5 category
#' @return a dataframe of the days where the PM2.5 values surpass the EPA threshold.
#' @export
#'
overEPA <- function(ourData){
  names(ourData)[2] <- "PM2.5"
  rolling <- data.frame(rollingmean = rollmean(ourData$PM2.5, 24))
  rolling[nrow(rolling)+ (length(ourData$PM2.5) - length(rolling$rollingmean)),] <- NA
  Sensor <- dplyr::bind_cols(ourData, rolling)
  daysOver <- dplyr::filter(Sensor, rollingmean >= 35)

  daysOver
}

#' This function takes in a data frame of values that is cleaned by overEPA and
#' the number of days that we have sensor data for (not number of days that are over
#' EPA threshold), and returns a dataframe that fits in the histogram function
#' @param a dataframe of values from one sensor after being cleaned by overEPA
#' @return a dataframe that fits in the histogram function
#' @export
#'
overEPA_hist <- function(daysOver,numOfDays){

  list <- c()
  days <- c()

  #numOfDays <- 31
  # find the frequency of the days over the EPA threshold
  for (day in (1:numOfDays)){
    freq <- 0
    for (hour in (1:length(daysOver$timestamp))){
      dayF <- c()
      if(daysOver$day[hour] == day){
        freq <- freq + 1
        dayF <- append(dayF,daysOver$hour)}}
    list <- c(list,freq)
    days <- append(days,dayF)}

  df <- data.frame(day = seq(1,numOfDays,by=1),
                   freq = list)

  # find what days correspond to those frequencies
  total <- 0

  for (i in (1:length(list))) {
    df$days[i] <- list(days[total+1:list[i]])
    total <- total + list[i]}


  for (i in (1:length(list))) {
    if (list[i] != 0) {df$days1[i] <- df$days[i]}
    else{df$days1[i] <- 0}}

  # make a dataframe corresponding to these values
  df <- df[,-c(3)]
  names(df)[3] <- "days"

  df
}

#' Down Sensors?
#'
#' This reports which sensors were down and on which days that they went down. This
#'
#' @param data a dataframe of hourly PurpleAir sensor data from the CEHAT website
#' @param new a boolean specifying if you are using the new data format (in which case you put TRUE) or the old data format (in which case you put FALSE)
#' @return a list object including the logical dataframe showing when sensors went down and the corresponding numerical representation of this, which counts the days
#' @export

downSensors <- function(data, new){
  #---------------------------------------------------#
  if (new == FALSE){
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
  }

  else{
    sensors <- unique(data[,c("longitude","latitude","names")])
    sensorNum <- length(unique(data$names))
  }

  totalDays <- unique(cut(data$timestamp, breaks="day"))

  totalDays <- as.Date(totalDays, tz= "America/Los_Angeles")

  numDays <- length(totalDays)
  #---------------------------------------------------#

  downSensors <- data.frame(matrix(ncol = sensorNum+1, nrow = numDays ))

  downSensors[,1] <- totalDays


  #was sensor i down on day k? (FALSE if yes, TRUE if no)
  for(i in 1:sensorNum){
    names(downSensors)[i+1] <- sensors$names[i]
    q <- c()
    for(k in 1:numDays){
      if (sensors$names[i] %in% data$names[as.Date(data$timestamp, tz= "America/Los_Angeles") == totalDays[k]]){
        q <- c(q,TRUE)
      }
      else{
        q <- c(q,FALSE)
      } }
    downSensors[,i+1] <- q
  }
  names(downSensors)[1] <- "Date"


  #numerical representation
  offDays <- as.data.frame(apply(downSensors[,-1], 2, sum))
  offDays <- tibble::rownames_to_column(offDays, "names")
  names(offDays)[2] <- "numDownDays"
  offDays$numDownDays <- (as.vector(rep(length(totalDays),times=sensorNum)) - offDays$numDownDays)


  output <- list(downSensors,offDays)

  output
}




#' Above/Below Median
#'
#' This reports which sensors were down and on which days that they went
#'
#' @param data output of the hourlyPA function (a dataframe of hourly PurpleAir sensor data)
#' @param type a single-character string indicating whether values above or below the median should be returned
#' @param new a boolean specifying if you are using the new data format (in which case you put TRUE) or the old data format (in which case you put FALSE)
#' @return a dataframe that details how many readings are above/below the median for each sensor, along with that sensors total readings
#' @export

compareSensors <- function(data, type = c('a', 'b'), new){
  #-------------------------------------------------------------------------------------#
  if (new == FALSE){
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
  }

  else{
    sensors <- unique(data[,c("longitude","latitude","names")])
    sensorNum <- length(unique(data$names))
  }
  #-------------------------------------------------------------------------------------#

  num_readings <- aggregate(PM2.5 ~ names, data, FUN=length )
  colnames(num_readings) <- c('names', 'total_readings')

  #the percent breaks
  bks <- c(0,15,50,100, 200, Inf)
  end <- length(bks)

  #initialize the data frame
  pct_diffs <- data.frame(matrix(0L, ncol = 6, nrow = sensorNum ))

  names(pct_diffs) <- c("names", "0-15%", "15%-50%", "50%-100%", "100%-200%", "above_200%")
  pct_diffs$names <- sensors$names


  #change less than or equal to back to just less than once you check the thing
  if (type == "a"){
    readings_over <- aggregate(PM2.5 ~ names,
                               data = data[data$PM2.5> data$median_PM2.5,], FUN=length )
    colnames(readings_over) <- c("names", "count")

    #add longitude and latitude, for mapping
    readings_over <- dplyr::left_join(readings_over, sensors, by= "names")
    #add the total number of readings
    readings_over <- dplyr::left_join(readings_over, num_readings, by= "names")

    #create the pct difference values on which to separate by
    readings <- dplyr::filter(data, PM2.5> median_PM2.5)
    readings$pct_over <- (abs(readings$PM2.5 - readings$median_PM2.5)/readings$median_PM2.5)*100

    x<-1
    #counting each instance of readings under the median by percentage bin
    while (!plyr::empty(readings[readings$pct_over>=bks[x] & readings$pct_over<=bks[x+1],])){
      u <- aggregate(PM2.5 ~ names,
                     data = readings[readings$pct_over>=bks[x] & readings$pct_over<=bks[x+1],], FUN = length)

      index <- dplyr::left_join(pct_diffs, u, by ="names", keep=F)

      pct_diffs[,x+1] <- index[,7]
      x <- x+1
    }

    #aggregate doesn't count nonpresent sensors, so replace the NA values with 0's
    pct_diffs[is.na(pct_diffs)] <- 0

    readings_over <- dplyr::left_join(readings_over, pct_diffs, by = "names", keep=F)

    readings_over
  }
  else{
    readings_under <- aggregate(PM2.5 ~ names,
                                data = data[data$PM2.5< data$median_PM2.5,], FUN=length )
    colnames(readings_under) <- c("names", "count")

    #add longitude and latitude, for mapping
    readings_under <- dplyr::left_join(readings_under, sensors, by= "names")
    #add the total number of readings
    readings_under <- dplyr::left_join(readings_under, num_readings, by= "names")

    #create the pct difference values on which to separate by
    readings <- dplyr::filter(data, PM2.5< median_PM2.5)
    readings$pct_under <- (abs(readings$PM2.5 - readings$median_PM2.5)/readings$median_PM2.5)*100

    pct_names <- c("0-15%", "15%-50%", "50%-100%", "100%-200%", "above_200%")

    x<-1
    #counting each instance of readings under the median by percentage bin
    while (!plyr::empty(readings[readings$pct_under>=bks[x] & readings$pct_under<=bks[x+1],])){
      u <- aggregate(PM2.5 ~ names,
                     data = readings[readings$pct_under>=bks[x] & readings$pct_under<=bks[x+1],], FUN = length)

      index <- dplyr::left_join(pct_diffs, u, by ="names", keep=F)

      pct_diffs[,x+1] <- index[,7]
      x <- x+1
    }

    #aggregate doesn't count nonpresent sensors, so replace the NA values with 0's
    pct_diffs[is.na(pct_diffs)] <- 0

    readings_under <- dplyr::left_join(readings_under, pct_diffs, by = "names", keep=F)

    readings_under
  }

}


#' Sensitive Locations
#'
#'
#' @param the input is the data that is outputted in the PAhourly function and a
#' time a character object of forrmat "yyyy-mm-dd hh:00:00" 
#' @return a dataframe that contains sensitive locations and their locations 
#' @export

sensitiveLocations <- function(data,time){
  
  # sensitive locations
  sensitiveLocations <- data.frame(places = c("Tweedy Elementary School", "Bryson Elementary School","South Gate Middle School","San Miguel School",
                                              "Stanford Elementary School","San Gabriel Elementary School","South East High School","Liberty Boulevard Elementary School",
                                              "South Gate Park","Circle Park","El Paseo Shopping Center","Azalea Shopping Center","MC Guadalupano",
                                              "MC Tweedy","MC Clinica Maria","MC Loyola", "MC United","South Gate SC", "South Gate Senior Villas SC","South Gate Park SC"),
                                   
                                   latitude = c(33.9435,33.9455,33.9540,33.9445,33.9505,33.9565,33.9435,33.962,33.946,
                                                33.939,33.951,33.953,33.9420,33.9435,33.9625,33.9435,33.9420,33.9325,
                                                33.9435,33.941),
                                   longitude = c(-118.1890,-118.1950,-118.206,-118.2045,-118.2270,-118.2085,
                                                 -118.2255,-118.223,-118.1895,-118.1695,-118.1695,-118.1860,
                                                 -118.2100,-118.2025,-118.2065,-118.1990,-118.184,-118.182,
                                                 -118.209,-118.2),
                                   AQI = rep(0,20))

  predictions<- as.data.frame(krigePA(data,time))
  predictions$x <- round(predictions$x,4)
  predictions$y <- round(predictions$y,4)

  for (place in (1:length(sensitiveLocations$places))){
    pm <- filter(predictions,y == sensitiveLocations$latitude[place] &
                                       x == sensitiveLocations$longitude[place])
    if (length(pm$x) != 0){
      sensitiveLocations$AQI[place] <- PurpleAirCEHAT::aqiFromPm25(pm$var1.pred)
      }
  
  }
  sensitiveLocations
}


