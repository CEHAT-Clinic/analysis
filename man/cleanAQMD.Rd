\name{cleanAQMD}
\alias{cleanAQMD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clean AQMD data files}
\description{
This function takes in data, specifically the name of a csv file with
timestamps, PM2.5, humidity, latitude and longitude
It then cleans the changes the timestamps so that they match the
timestamps of the purple air data.
}
\usage{
cleanAQMD(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a csv file with PM2.5 values from the AQMD site
}
}

\value{
a file with a time format that matches the time format
on PurpleAir files.
}

