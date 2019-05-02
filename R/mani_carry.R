#' Given the data \code{carry}, retrieve the filtered data.
#' @import dplyr
#' @export
filter_stations <- function(carry, sta = '台北', years = 2005, months = 1:12) {
  data %>% filter(station %in% sta) %>%
    filter(year(date) %in% years) %>%
    filter(month(date) %in% months)
}
