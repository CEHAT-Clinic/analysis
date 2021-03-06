% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_dataframes.R
\name{overEPA_hist}
\alias{overEPA_hist}
\title{This function takes in a data frame of values that is cleaned by overEPA and
the number of days that we have sensor data for (not number of days that are over
EPA threshold), and returns a dataframe that fits in the histogram function}
\usage{
overEPA_hist(daysOver, numOfDays)
}
\arguments{
\item{a}{dataframe of values from one sensor after being cleaned by overEPA}
}
\value{
a dataframe that fits in the histogram function
}
\description{
This function takes in a data frame of values that is cleaned by overEPA and
the number of days that we have sensor data for (not number of days that are over
EPA threshold), and returns a dataframe that fits in the histogram function
}
