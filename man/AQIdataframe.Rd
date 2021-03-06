% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_dataframes.R
\name{AQIdataframe}
\alias{AQIdataframe}
\title{This function takes in data, specifically the name of a csv file with
timestamps, ChannelA, ChannelB, humidity, latitude and longitude
It then cleans the data frame and returns AQI values.}
\usage{
AQIdataframe(data)
}
\arguments{
\item{data, }{specifically the name of a csv file}
}
\value{
it returns a dataframe with hourly time, AQI, humidity, latitude and longitude
}
\description{
This function takes in data, specifically the name of a csv file with
timestamps, ChannelA, ChannelB, humidity, latitude and longitude
It then cleans the data frame and returns AQI values.
}
