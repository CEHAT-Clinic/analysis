\name{aqiFromPm25}
\alias{aqiFromPm25}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PM2.5 to AQI}
\description{
This function takes in one parameters:pm25Concentration.
It then calculates the AQI given that specified value.}
\usage{
aqiFromPm25(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a PM2.5 value
}
}
\value{
an AQI value that corresponds to that PM2.5 value (using an EPA
correction formula)
}
