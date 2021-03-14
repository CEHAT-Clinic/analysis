#' Lollipop Chart
#'
#' This function creates a lollipop chart visualization for PurpleAir high/low sensor data.
#'
#' @param data a .csv of PurpleAir high/low sensor data from the CEHAT website
#' @param sensor which sensor you want to view
#' @return a lollipop ggplot2 chart of daily high and low values per sensor for a given time period
#' @export

lollipop <- function(data, sensor) {
  wessy_pal <- c("high"="#C93312","low"="#899DA4")

  print(tidyverse::ggplot(data=data[data$longitude == sensor,], tidyverse::aes(x=day, y=PM2.5, group = day)) +
          tidyverse::geom_line(lwd=1) +
          tidyverse::geom_point(data=data[data$type == "high" & data$PM2.5<=300 & data$names == sensor, ],
                                tidyverse::aes(x=day, y=PM2.5, group = type, col="high"), size=5)+
          tidyverse::geom_point(data=data[data$type == "low" & data$names == sensor,],
                                tidyverse::aes(x=day, y=PM2.5, group = type, col="low"), size=5)+
          tidyverse::scale_color_brewer(palette="Dark2") +
          tidyverse::labs(x = "Day", y = "PM2.5 (µg/m³)") +
          tidyverse::scale_colour_manual(name="Type",values=wessy_pal, guide = guide_legend(override.aes=aes(fill=NA)) ) +
          tidyverse::scale_fill_manual(name="Type",values=wessy_pal) +
          tidyverse::ggtitle(sensor) +
          tidyverse::theme_minimal())
}


