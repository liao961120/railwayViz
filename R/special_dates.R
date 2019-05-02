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


#' Is the date given 'normal' (Not holidays, not weekends)
#' @importFrom lubridate ymd wday days year month day
#' @export
is_working_day_atom <- function(year, month, day) {
  date <- ymd(paste(year, month, day, sep = '-'))

  holidays <- c('01-01', '02-28', '04-04', '04-05', '04-06', '05-01',
                '10-10')
  lunar_holidays <- c('12-29', '12-30', '01-01', '01-02', '01-03', '01-04', '01-05', '01-06', '05-05', '08-15')

  # Weekends?
  message('Weekends')
  if (wday(date, week_start = 1) %in% c(5, 6, 7)) return(FALSE)

  # Holidays?
  message('Holiday')
  tmp <- sprintf("%02d", c(month, day))
  if (paste(tmp, collapse = '-') %in% holidays) return(FALSE)

  # Lunar Holidays?
  message('Lunar holiday')
  tmp <- sprintf("%02d", tolunar(year, month, day))
  if (paste(tmp, collapse = '-') %in% lunar_holidays) return(FALSE)

  # Typhoons?
  message('typhoons')
  if (date %in% ymd(typhoon_dates)) return(FALSE)

  # Monday off (彈性放假)
  message('Tue. is holiday')
  if (wday(date, week_start = 1) == 1) {
    tomorrow <- date + days(1)
    yyyy <- year(tomorrow)
    mm <- month(tomorrow)
    dd <- day(tomorrow)

    solar_date <- paste(sprintf("%02d", c(mm, dd)), collapse = '-')
    lunar_date <- paste(sprintf("%02d", tolunar(yyyy, mm, dd)), collapse = '-')

    # Tue. is Holiday?
    if (solar_date %in% holidays || lunar_date %in% lunar_holidays) return(FALSE)
  }

  return(TRUE)
}

#' Is the given dates a working day?
#' @export
is_working_day <- function(dates = c("2019-02-05")) {
  out_vec <- vector('logical', length = length(dates))
  for (i in seq_along(dates)) {
    date <- unlist(strsplit(dates[i], '-'))
    date <- as.integer(date)
    out_vec[i] <- suppressMessages(
      is_working_day_atom(date[1], date[2], date[3])
    )
  }
  return(out_vec)
}
