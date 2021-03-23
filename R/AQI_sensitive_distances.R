suppressPackageStartupMessages({
  library(ggplot2)
  library(data.table)
  #for the mapping
  library(dplyr)
  library(gdata)
  library(Ecdat)
  library(boot)
  library(lubridate)
  library(gstat)

  devtools:: install_github("CEHAT-Clinic/analysis")
})

test1 <- read.csv("december2020_readings.csv")
myData <- cleanPA(test1)
# *********finding AQI at distance from sensitive locations***********

# ---- GATHERING THE SENSITIVE LOCATIONS

# schools

schools <- data.frame("schools" = c("Miramonte Elementary", "Russell Elementary",
                                    "New Charles Drew Middle", "Florence Elementary",
                                    "Middleton Elementary", "Miles Elementary",
                                    "Walnut Elementary", "Corona Elementary",
                                    "Teresa Hughes Elementary", "South Gate Middle",
                                    "South East High", "Warren High"),
                      latitude = c(33.9781,33.9610,33.9608,33.9738,33.9800,33.9779,
                                   33.9680,33.9757,33.9661,33.9519, 33.9450,33.9360),
                      longitude = c(-118.2499,-118.2519,-118.2481,-118.2388,-118.2282,
                                    -118.2187,-118.2251,-118.1976,-118.1930,-118.1976,
                                    -118.2227,-118.1401))


# superfund sites

superfundSites <- data.frame("site" = c("Cooper Drum", "Jervis B Webb", "Southern Ave"),
                             latitude = c(33.9469, 33.9498,33.9467),
                             longitude = c(-118.1812, -118.1778,-118.1778))

# parks
# roosevelt park is not in SG, it's nearby

parks <- data.frame("park" = c("Roosevelt", "Clara Street", "Camp Little Bear",
                               "Stanford Avenue", "South Gate","Cudahy"),
                    latitude = c(33.9707,33.9657, 33.9763,33.9514, 33.9460,33.9594),
                    longitude = c(-118.2420,-118.1816,-118.1990,-118.2224, -118.1852,
                                  -118.1747))

# medical centers

medicalCenters <- data.frame("medical center" = c("Tweedy", "Sanchez","Clinica Maria",
                                                  "Loyola", "Guadalupano", "United"),
                             latitude = c(33.9434, 33.9543,33.9627,33.9435,33.9421,
                                          33.9421),
                             longitude = c(-118.2001,-118.2050,-118.2043,-118.1969,
                                           -118.2074,-118.1820))
# senior centers
# roosevelt park and watts senior centers are not in South Gate, just very close

seniorCenters <- data.frame("senior centers" = c("South Gate", "Watts","Roosevelt Park",
                                                 "South Gate Senior Villas",
                                                 "Vista Verenda Assisted Living"),
                            latitude = c(33.9421,33.9458,33.9700,33.9437,33.9351),
                            longitude = c(-118.1857,-118.2441,-118.2420, -118.2066,
                                          -118.2064))


# --------ADDING NEW COLUMNS TO THE DATAFRAMES

# for schools
myAQIData$MiramonteSch <- NA
myAQIData$RussellSch <- NA
myAQIData$NewCharlesSch <- NA
myAQIData$FlorenceSch <- NA
myAQIData$MiddletonSch <- NA
myAQIData$MilesSch <- NA
myAQIData$WalnutSch <- NA
myAQIData$CoronaSch <- NA
myAQIData$TeresaHughesSch <- NA
myAQIData$SouthGateSch <- NA
myAQIData$SouthEastSch <- NA
myAQIData$WarrenHighSch <- NA

#for parks
myAQIData$MiramontePark <- NA
myAQIData$RussellPark <- NA
myAQIData$NewCharlesPark <- NA
myAQIData$FlorencePark <- NA
myAQIData$MiddletonPark <- NA
myAQIData$MilesPark <- NA

#for superfund sites
myAQIData$CooperDrumSFS <- NA
myAQIData$JervisWebbSFS <- NA
myAQIData$SouthernAveSFS <- NA

#for medical centers
myAQIData$TweedyMC <- NA
myAQIData$SanchezMC <- NA
myAQIData$clinicaMariaMC <- NA
myAQIData$LoyolaMC <- NA
myAQIData$GuadalupanoMC <- NA
myAQIData$UnitedMC <- NA

#for senior centers
myAQIData$SouthGateSC <- NA
myAQIData$WattsSC <- NA
myAQIData$RooseveltParkSC <- NA
myAQIData$SouthGateVillasSC <- NA
myAQIData$VistaVerendaSC <- NA

numOfLoc <- 32
# ----- ADDING DISTANCE VALUES TO DATA FRAME -------

# for schools
for (i in (1:length(schools$schools))) {
  for (j in (1: length(myAQIData$datehour))) {
    myAQIData[j,i+5] <- distance(myAQIData$latitude[j],myAQIData$longitude[j],
                                 schools$latitude[i],schools$longitude[i])
  }}

#for parks
for (i in (1:length(parks$park))) {
  for (j in (1: length(myAQIData$datehour))) {
    myAQIData[j,i+17] <- distance(myAQIData$latitude[j],myAQIData$longitude[j],
                                  parks$latitude[i],parks$longitude[i])
  }}

# for SuperFund Sites
for (i in (1:length(superfundSites$site))) {
  for (j in (1: length(myAQIData$datehour))) {
    myAQIData[j,i+23] <- distance(myAQIData$latitude[j],myAQIData$longitude[j],
                                  superfundSites$latitude[i],superfundSites$longitude[i])
  }}

# for medical Centers
for (i in (1:length(medicalCenters$medical.center))) {
  for (j in (1: length(myAQIData$datehour))) {
    myAQIData[j,i+26] <- distance(myAQIData$latitude[j],myAQIData$longitude[j],
                                  medicalCenters$latitude[i],medicalCenters$longitude[i])
  }}

#for senior centers
for (i in (1:length(seniorCenters$senior.centers))) {
  for (j in (1: length(myAQIData$datehour))) {
    myAQIData[j,i+32] <- distance(myAQIData$latitude[j],myAQIData$longitude[j],
                                  seniorCenters$latitude[i],seniorCenters$longitude[i])
  }}



# if you want to look up the AQI (and the distance between the sensors
# and sensitive locations) at a certain date and hour,
# substitute the date here

filter(myAQIData, datehour == '2020-12-18 15:00:00') # <------ HERE

avgValuesLessthan1 <- data.frame("datehour" = myAQIData$datehour)
avgValuesLessthan1$MiramonteSch <- NA
avgValuesLessthan1$RussellSch <- NA
avgValuesLessthan1$NewCharlesSch <- NA
avgValuesLessthan1$FlorenceSch <- NA
avgValuesLessthan1$MiddletonSch <- NA
avgValuesLessthan1$MilesSch <- NA
avgValuesLessthan1$WalnutSch <- NA
avgValuesLessthan1$CoronaSch <- NA
avgValuesLessthan1$TeresaHughesSch <- NA
avgValuesLessthan1$SouthGateSch <- NA
avgValuesLessthan1$SouthEastSch <- NA
avgValuesLessthan1$WarrenHighSch <- NA

#for parks
avgValuesLessthan1$MiramontePark <- NA
avgValuesLessthan1$RussellPark <- NA
avgValuesLessthan1$NewCharlesPark <- NA
avgValuesLessthan1$FlorencePark <- NA
avgValuesLessthan1$MiddletonPark <- NA
avgValuesLessthan1$MilesPark <- NA

#for superfund sites
avgValuesLessthan1$CooperDrumSFS <- NA
avgValuesLessthan1$JervisWebbSFS <- NA
avgValuesLessthan1$SouthernAveSFS <- NA


avgValuesLessthan1$TweedyMC <- NA
avgValuesLessthan1$SanchezMC <- NA
avgValuesLessthan1$clinicaMariaMC <- NA
avgValuesLessthan1$LoyolaMC <- NA
avgValuesLessthan1$GuadalupanoMC <- NA
avgValuesLessthan1$UnitedMC <- NA

avgValuesLessthan1$SouthGateSC <- NA
avgValuesLessthan1$WattsSC <- NA
avgValuesLessthan1$RooseveltParkSC <- NA
avgValuesLessthan1$SouthGateVillasSC <- NA
avgValuesLessthan1$VistaVerendaSC <- NA


for (i in (1:numOfLoc)) {
for(j in (1:length(avgValuesLessthan1$datehour))) {
    date <- filter(myAQIData, datehour == avgValuesLessthan1[j,1])
    vector1 <- c()
    for (k in (1:length(date$datehour))) {
      if (date[k, i+5] < 2){
        vector1 <- append(vector1,date[k, 2])
      }
    }
    avg <- mean(vector1)
    avgValuesLessthan1[j,i+1] <- avg
    }
  }

avgValuesLessthan1 <- avgValuesLessthan1

