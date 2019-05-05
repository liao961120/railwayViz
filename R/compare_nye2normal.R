#' Get New Year's Eve & normal Wednesdays stats of a given year
#'
#' Three columns, \code{date} (class: Date), \code{num_in},
#' and \code{num_out} must be present in \code{carry}.
#'
#' @import dplyr
#' @importFrom lubridate ymd
#' @import rlang
#' @export
compare_nye2wdays <- function(carry, year, groupby = 'station') {
  # Date of New Years Eve & 'normal' Wednesdays
  all_dates <- seq(ymd(paste0(year, '-01-01')), ymd(paste0(year, '-12-31')),
                   by = 'days')
  working_days <- all_dates[is_working_day(as.character(all_dates))]

  relevant_days <- list(new_year_eve = nye_date(year),
                        working_days = working_days)

  # Filter data to only relevant days
  carry <- carry %>%
    filter(date %in% unlist(relevant_days))

  # Deal with cases where num_in & num_out are both zeros
  for (i in 1:nrow(carry)) {
    if (carry$num_in[i] == 0 && carry$num_out[i] == 0) {
      carry$num_in[i] <- 1
      carry$num_out[i] <- 1
    }
  }

  # New Year's Eve stats
  carry_nye <- carry %>%
    filter(date == nye_date(year)) %>%
    select(-date) %>%
    rename(num_in_nye = num_in, num_out_nye = num_out) %>%
    mutate(nye_idx = (num_out_nye - num_in_nye) / (num_in_nye + num_out_nye))

  # Working Days avg. stats
  carry_wdays_avg <- carry %>%
    filter(date != nye_date(year)) %>%
    mutate(wday_idx = (num_out - num_in) / (num_out + num_in)) %>%
    group_by(!!sym(groupby)) %>%
    summarise(avg_wday_idx = mean(wday_idx))

  # Combine New Year's Eve data & Working Days data
  groupby <- enquo(groupby)
  by <- set_names(quo_name(groupby), quo_name(groupby))
  carry_nye_v_normal <- left_join(carry_nye,
                                  carry_wdays_avg,
                                  by = by) %>%
    mutate(urban_idx = avg_wday_idx - nye_idx)
}

#' Convert data frame to sf spatial data frame
#'
#' Merge data returned from \code{compare_nye2wdays} with
#' sf datafrme.
#' @import dplyr
#' @export
to_sf <- function(carry_nye_wdays, sf, ...) {
  sf <- dplyr::left_join(carry_nye_wdays, sf, ...) %>%
    sf::st_as_sf(sf_column_name = "geometry")
}
