#' This function takes in five parameters: highIndexBreakpoint,lowIndexBreakpoint,
#' highConcentrationBreakpoint,lowConcentrationBreakpoint,pm25Concentration, in
#' that order. It then calculates the AQI guven the specified indices.
#' @param highIndexBreakpoint,lowIndexBreakpoint, highConcentrationBreakpoint,lowConcentrationBreakpoint,pm25Concentration
#' @return it returns the AQI that corresponds to the given PM2.5 measurement
#' @export


indexCalculation <-function(highIndexBreakpoint,lowIndexBreakpoint,
                            highConcentrationBreakpoint,lowConcentrationBreakpoint,pm25Concentration){

  indexRange <- highIndexBreakpoint - lowIndexBreakpoint

  concentrationRange <- highConcentrationBreakpoint - lowConcentrationBreakpoint
  rangeRelativeConcentration <- pm25Concentration - lowConcentrationBreakpoint

  return (
    (indexRange / concentrationRange) * rangeRelativeConcentration +
      lowIndexBreakpoint)
}



#' This function takes in one parameters:pm25Concentration.
#'  It then calculates the AQI given that specified value.
#' @param pm25Concentration
#' @return it returns the AQI that corresponds to the given PM2.5 measurement
#' @export



aqiFromPm25 <- function(pm25Concentration) {
  # Source of bound values is Table 6 of the paper at
  # https://www.airnow.gov/sites/default/files/2018-05/aqi-technical-assistance-document-may2016.pdf
  # EPA formulas require PM 2.5 to be truncated to one decimal place

  truncatedPm25 <- floor(10 * pm25Concentration) / 10

  aqi <-  0
  highAqiBound <-  0
  lowAqiBound <-  0
  highPmBound <-  0
  lowPmBound <-  0

  # Assign appropriate bounds
  if (truncatedPm25 < 12.1) {
    highAqiBound <-  50
    lowAqiBound <-  0
    highPmBound <-  12
    lowPmBound <-  0
  } else if (truncatedPm25 < 35.5) {
    highAqiBound <-  100
    lowAqiBound <-  51
    highPmBound <-  35.4
    lowPmBound <-  12.1

  } else if (truncatedPm25 < 55.5) {
    highAqiBound <-  150
    lowAqiBound <-  101
    highPmBound <-  55.4
    lowPmBound <-  35.5

  } else if (truncatedPm25 < 150.5) {
    highAqiBound <-  200
    lowAqiBound <-  151
    highPmBound <-  150.4
    lowPmBound <-  55.5

  } else if (truncatedPm25 < 250.5) {
    highAqiBound <-  300
    lowAqiBound <-  201
    highPmBound <-  250.4
    lowPmBound <-  150.5

  } else if (truncatedPm25 < 350.5) {
    highAqiBound <-  400
    lowAqiBound <-  301
    highPmBound <-  350.4
    lowPmBound <-  250.5

  } else if (truncatedPm25 < 500.5) {
    highAqiBound <-  500
    lowAqiBound <-  401
    highPmBound <-  500.4
    lowPmBound <-  350.5
  }

  # Values beyond the range are indicated by infinite values
  if (truncatedPm25 < 0) {
    aqi <-  -Inf
  } else if (truncatedPm25 >= 500.5) {
    aqi <-  Inf
  } else {
    aqi <-  indexCalculation(
      highAqiBound,
      lowAqiBound,
      highPmBound,
      lowPmBound,
      truncatedPm25
    )
  }
  # /* eslint-enable no-magic-numbers */
  return (round(aqi, digits = 0))
}
