#' Get New Year's Eve & normal Wednesdays stats of a given year
#' @import dplyr
#' @export
compare_nye2normal <- function(carry, year) {
  # Date of New Years Eve & 'normal' Wednesdays
  relevant_days <- list(new_year_eve = nye_date(year),
                        wednesdays = c(cl_wed_date(year, 3, 7),
                                       cl_wed_date(year, 3, 15),
                                       cl_wed_date(year, 3, 23),
                                       cl_wed_date(year, 4, 15),
                                       cl_wed_date(year, 4, 23),
                                       cl_wed_date(year, 5, 7),
                                       cl_wed_date(year, 5, 15),
                                       cl_wed_date(year, 5, 23),
                                       cl_wed_date(year, 11, 7),
                                       cl_wed_date(year, 11, 15),
                                       cl_wed_date(year, 11, 23),
                                       cl_wed_date(year, 12, 7),
                                       cl_wed_date(year, 12, 15),
                                       cl_wed_date(year, 12, 23)))

  # Filter data to only relevant days
  carry <- carry %>%
    filter(date %in% unlist(relevant_days))

  # New Year's Eve stats
  carry_nye <- carry %>%
    filter(date == nye_date(year)) %>%
    select(-date) %>%
    rename(num_in_nye = num_in, num_out_nye = num_out)

  # Normal Wednesdays avg. stats
  carry_ndays_avg <- carry %>%
    filter(date != nye_date(year)) %>%
    group_by(station) %>%
    summarise(num_in_avg = mean(num_in),
              num_out_avg = mean(num_out))

  # Combine New Year's Eve data & Normal Wednesdays data
  carry_nye_v_normal <- left_join(carry_nye,
                                  carry_ndays_avg,
                                  by = c('station' = 'station'))
}

#' Convert dataframe to sf spatial data frame
#'
#' Merge data returned from \code{compare_nye2normal} with
#' sf datafrme \code{stations}.
#' @import dplyr
#' @export
to_sf <- function(carry_nye_normal, stations) {
  sf <- dplyr::left_join(carry_nye_normal, stations) %>%
    sf::st_as_sf(sf_column_name = "geometry") %>%
    filter(!is.na(landmarkid)) %>%
    select(station, num_in_nye, num_out_nye,
           num_in_avg, num_out_avg, address, geometry)
}
