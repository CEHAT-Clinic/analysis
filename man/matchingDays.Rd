% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_dataframes.R
\name{matchingDays}
\alias{matchingDays}
\title{This function takes in cleaned purple air data, a dataframe with
timestamps, PM2.5, humidity, latitude and longitude, and a AQMD csv files.
It then combines these two files to make a dataframe with matching days.}
\usage{
matchingDays(SGdata, otherCitydata)
}
\arguments{
\item{SGdata}{and otherCitydata, two csv files}
}
\value{
it returns a dataframe with matching days
}
\description{
This function takes in cleaned purple air data, a dataframe with
timestamps, PM2.5, humidity, latitude and longitude, and a AQMD csv files.
It then combines these two files to make a dataframe with matching days.
}
