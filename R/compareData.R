suppressPackageStartupMessages({
  library(usethis)
  library(testthat)
  library(devtools)
  devtools:: install_github("CEHAT-Clinic/analysis")
  library(PurpleAirCEHAT)
})

# reading the csv file
test1 <- read.csv("december2020_readings.csv")

#cleaned PM2.5 data
myData <- test1


Sensor1 <- dpylr::filter(myData, latitude == '33.9509')
Sensor2 <- dpylr::filter(myData, latitude == '33.9606')
Sensor3 <- dpylr::filter(myData, latitude == '33.9498')
Sensor4 <- dpylr::filter(myData, latitude == '33.9619')
Sensor5 <- dpylr::filter(myData, latitude == '33.9411')

# ************ compare the South Gate data to the AQMD data *************

otherCityData <- read.csv("long_beach_pm2.5.csv")

# --- changing the time format for the long beach data
#otherCityData <- cleanAQMD(otherCityData)
# --- filtering out the rows with no pm2.5 data
otherCityData <- dpylr::filter(otherCityData, Value != "--")

# --- making a data frame with matching dates (for Sensor1)

#filtering out the non common days

#the sensor1 days that are not in the long beach dates
nonMatchingDays <- Sensor1$datehour[!Sensor1$datehour %in% otherCityData$Date.Time]
# vice versa
nonMatchingDays1 <- otherCityData$Date.Time[!otherCityData$Date.Time %in% Sensor1$datehour]

`%notin%` <- Negate(`%in%`)

Matching1 <- dpylr::filter(otherCityData, Date.Time %notin% nonMatchingDays1)
Matching2 <- dpylr::filter(Sensor1, datehour %notin% nonMatchingDays)

#making the data frame with the PM2.5 values
Matching1$SouthGatePM<- round(Matching2$PM2.5,0)

ourData1 <- Matching1[-c(3,4)]
names(ourData1)[1] <- "datehour"
names(ourData1)[2] <- "otherCityPM"
ourData1$otherCityPM <- as.numeric(as.character(ourData1$otherCityPM))

ourData1 <- dplyr::filter(ourData1, otherCityPM < 300)
for (i in 1:length(ourData1$datehour)) {
  ourData1$difference[i] = ourData1$SouthGatePM[i] - ourData1$otherCityPM[i]
}

# ----- comparing the data

# comparing the two data sets using graphics

# boxplot
boxplot(ourData1$otherCityPM, ourData1$SouthGatePM, main = "PM2.5 in Other City vs South Gate",
        names = c("Other City","South Gate"), outline=FALSE)

# line chart
x <- strptime(ourData1$datehour, '%Y-%m-%d %H:%M:%S')
plot(x,ourData1$SouthGatePM,main = "otherCity vs South Gate", xlab= "day", ylab= "PM2.5",
     type="l",col="red")
lines(x,ourData1$otherCityPM,col="green")

# stacked barchart
counts <- table(ourData1$otherCityPM, x)

barplot(counts)

# comparing the data using t-tests


ttest = stats::t.test(ourData1$otherCityPM,ourData1$SouthGatePM)

# check if the difference is statistically different

if (ttest$p.value < (.05) && ttest$statistic > 0) {
  print("we can not accept the null hypothesis. the Other City data is statistically worse than the South Gate data.")
}
if (ttest$p.value < (.05) && ttest$statistic < 0) {
  print("we can not accept the null hypothesis. the South Gate data is statistically worse than the Other City data.")
}
if (ttest$p.value >= (.05)){
  print("we can not reject the null hypothesis")
}

