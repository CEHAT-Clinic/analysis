#' This function takes in a read csv file from AQMD and returns the
#' name of the city that the data is about
#' @param a csv file
#' @return it returns the name that the data set is about.
#' @export

gettingCityName <- function(data){

  for (i in (1:100)){
    if(substr(data[1,1],i,i) != '_'){end_index <- i}
    else {break}}

  nameOfCity <- substr(data[1,1],1,end_index)

  nameOfCity
}
