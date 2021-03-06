% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolation.R
\name{krigePA}
\alias{krigePA}
\title{Kriging Function}
\usage{
krigePA(data, time)
}
\arguments{
\item{data}{output of hourlyPA function, or air quality data organized by 1-hour timestamps}

\item{time}{a character object of forrmat "yyyy-mm-dd hh:00:00"}
}
\value{
the interpolation result: three objects of similar format: the input grid populated by values of predictions, variance, or standard deviation
}
\description{
This uses the automap package to conduct interpolation analysis on ONE HOUR of air quality data
It assumes that the data is the output of the function "hourlyPA". There is no alternative for this
}
