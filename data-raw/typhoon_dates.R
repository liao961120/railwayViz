library(dplyr)
library(lubridate)

typhoon <- readr::read_csv('data-raw/typhoon.csv') %>%
  rename(date = `警報期間`) %>%
  mutate(date = gsub(' \\d{2}:\\d{2}', '', date)) %>%
  mutate(date = strsplit(date, '\n'))

# Convert duration to dates
for (i in 1:nrow(typhoon)) {
  from <- typhoon$date[[i]][1]
  to <- typhoon$date[[i]][2]
  typhoon_dates <- seq(ymd(from), ymd(to), by = 'days')
  typhoon$date[[i]] <- as.character(typhoon_dates)
}

# Save dates since 2005
typhoon_dates <- unlist(typhoon$date)[1:243]

# Save to package
usethis::use_data(typhoon_dates, internal = TRUE, overwrite = TRUE)
