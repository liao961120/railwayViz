#' Return the month and day of the lunar calendar
#' @export
tolunar <- function(year, month, day) {
  python_script <- system.file('LunarSolarConverter.py', package = 'railwayViz')
  reticulate::source_python(python_script)

  lunar_date <- tolunar(year, month, day)
  names(lunar_date) <- c('month', 'day')
  return(lunar_date)
}

