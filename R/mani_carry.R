#' Given the data \code{carry}, retrieve the filtered data.
#' @import dplyr
#' @export
filter_stations <- function(carry, sta = '台北', years = 2005, months = 1:12) {
  data %>% filter(station %in% sta) %>%
    filter(year(date) %in% years) %>%
    filter(month(date) %in% months)
}

#' Retrieve the date of Chinese New Year
#' @export
nye_date <- function(year) {
  pat <- paste0('^', as.character(year))
  new_year <- seasonal::cny[grepl(pat, seasonal::cny)]
  return(as.Date(new_year - lubridate::days(1), '%Y-%m-%d'))
}

#' Set Date by weekday
#'
#' @return The closest weekday's date to the given date.
cl_wday_date <- function(year, month, day, weekday=3) {
  date <- lubridate::ymd(paste(year, month, day, sep = '-'))
  lubridate::wday(date, week_start = 1) <- weekday
  return(date)
}

#' Set Date by weekday
#'
#' @return The date of the closest Wednesday to the given date.
#' @export
cl_wed_date <- function(year, month, day) {
  cl_wday_date(year, month, day)
}

