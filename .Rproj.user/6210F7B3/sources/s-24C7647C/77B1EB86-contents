#' Add a polygon map of South Gate
#'
#' This function produces a line drawing of the city of South Gate, classed as a Spatial Polygons Data Frame
#'
#' @return an easily plot-able polygon of South Gate, of the class Spatial Polygons DF
#' @export

southgate <- function(){
  cities <- tigris::places(state = "CA", cb = TRUE, year=2019)
  sg <- dplyr::filter(cities, NAME == "South Gate")

  sg.city <- as(sg, "Spatial")
  sg.city <- sp::spTransform(sg.city, CRSobj = sp::CRS("+proj=longlat +zone=19 +ellps=WGS84 +datum=WGS84"))
  sg.city
}


#' Create a grid map of South Gate
#'
#' This function produces a grid of the city of South Gate, classed as a Spatial Pixels Data Frame
#'
#' @return an easily plot-able grid of South Gate, of the class Spatial Pixels DF; useful for kriging
#' @export

southgate_grid <- function() {

  cities <- tigris::places(state = "CA", cb = TRUE, year=2019)
  sg <- dplyr::filter(cities, NAME == "South Gate")

  sg.city <- as(sg, "Spatial")
  sg.city <- sp::spTransform(sg.city, CRSobj = sp::CRS("+proj=longlat +zone=19 +ellps=WGS84 +datum=WGS84"))

  long.range <- as.numeric(range(sg.city@bbox[1,]))
  lat.range <- as.numeric(range(sg.city@bbox[2,]))

  test.grid <- expand.grid(x = seq(from = long.range[1], to = long.range[2], by = .0005), y = seq(from = lat.range[1], to = lat.range[2], by = .0005))

  sg.grid <- sp::SpatialPoints(test.grid, proj4string = sp::CRS(sp::proj4string(sg.city)))

  sg.grid <- sp::SpatialPixelsDataFrame(points= sg.grid[sg.city,], data = as.data.frame(sg.grid[sg.city,]@coords))

  sg.grid
}
