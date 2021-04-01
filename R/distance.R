#' This function takes in one parameter:degrees.
#'  It then calculates the radian of that specified value.
#' @param degrees
#' @return it returns the radian value of that degree value
#' @export

toRad <- function(degrees) {
  return ((pi*degrees)/180)
}



#' This function takes in four parameters: lat_1,long_1,lat_2,long_2.
#' It then calculates the distance between these two locations using
#' the Haversine formula.
#' @param latitude of the first location, longitude of the first location, latitude
#' of the second location, longitude of the second location
#' @return it returns the distance between these locations in miles
#' @export
#'

distance <- function(lat_1,long_1,lat_2,long_2) {
  # converting all degrees to radians
  lat1 <- toRad(lat_1)
  long1 <- toRad(long_1)
  lat2 <- toRad(lat_2)
  long2 <- toRad(long_2)

  # calculating dist using the Haversine Formula
  answer <- 2 * asin(sqrt((sin((lat2 - lat1)/2))^2 +
                            cos(lat1) * cos(lat2)*
                            (sin((long2 - long1)/2))^2))

  # use 6371 instead of 3956 to calculate in kilometers
  return (answer*3956)
}

