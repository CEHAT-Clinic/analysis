\name{AQIdataframe}
\alias{AQIdataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make a Dataframe with AQI values}
\description{
This function takes in data, specifically the name of a csv file with
timestamps, ChannelA, ChannelB, humidity, latitude and longitude
It then cleans the data frame and returns AQI values.
}
\usage{
AQIdataframe(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a csv of PurpleAir sensor data from the CEHAT website
}
}

\value{
a dataframe of the full PurpleAir sensor data, with corrected timezone and combined channels. Instead of PM2.5 values being displayed, like the
cleanPA function, AQI values are displayed.
}
