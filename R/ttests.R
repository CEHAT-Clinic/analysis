#' This function takes in two arrays of data (with corresponding
#' days) and the city name (the non-south gate city name) and
#' returns the results after conducting t-tests on them
#' @param two arrays of data with matching days and the name of the non-South Gate city
#' @return it returns the t test results of the data sets
#' @export

ttests<- function(otherCityData,SGdata,cityName){

  ttest = stats::t.test(otherCityData,SGdata)

  # null hypothesis: there is no significant difference between
  # the two data sets
  # check if the difference is statistically different

  if (ttest$p.value < (.05) && ttest$statistic > 0) {
    result <- paste("the",cityName,"data is statistically worse than the South Gate data.",sep=" ")
  }
  if (ttest$p.value < (.05) && ttest$statistic < 0) {
    result <- paste("the South Gate data is statistically worse than the",cityName, "data.",sep = " ")
  }
  if (ttest$p.value >= (.05)){
    result <- paste("the difference in the data is not statistically significant.")
  }
  paste("The p value is ", ttest$p.value,". This means that", result, sep="")
}
