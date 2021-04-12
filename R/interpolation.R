#' Kriging Function
#'
#' This uses the automap package to conduct interpolation analysis on ONE HOUR of air quality data
#' It assumes that the data is the output of the function "hourlyPA". There is no alternative for this
#'
#'
#' @param data output of hourlyPA function, or air quality data organized by 1-hour timestamps
#' @param time a character object of forrmat "yyyy-mm-dd hh:00:00"
#' @return the interpolation result: three objects of similar format: the input grid populated by values of predictions, variance, or standard deviation
#' @export

krigePA <- function(data, time){

  slicePA <- data[data$timestamp == time,]

  slicecoords <- slicePA[,c("longitude","latitude")]

  sliceSP <- sp::SpatialPointsDataFrame(data = slicePA, coords = slicecoords,
                                    proj4string = rgdal::CRS("+proj=longlat +zone=19 +ellps=WGS84 +datum=WGS84"))

  sg.grid <- southgate_grid()


  variogram <- automap::autofitVariogram(PM2.5~1, sliceSP, model="Gau", miscFitOptions = list(min.np.bin = 3, merge.small.bins = TRUE) )


  #If the grid and SpatialPointDF have different projections, an error is raised.
  #If one of both has a non-projected system (i.e. latitude-longitude), an error is raised.
  #This error is raised because 'gstat does use spherical distances when data are in geographical coordinates
  #These lines are not reprojections, they simply change the coordinate systems
  sg.grid@proj4string <- sp::CRS("+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84")
  sliceSP@proj4string <- sp::CRS("+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84")

  kriging_result <- automap::autoKrige(PM2.5~1, sliceSP, sg.grid, model="Gau", miscFitOptions = list(min.np.bin = 3, merge.small.bins = TRUE))

  kriging_result$krige_output

}
