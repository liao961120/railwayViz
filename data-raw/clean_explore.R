library(dplyr)
library(lubridate)
library(ggplot2)

#### Get Date of Chinese New Year ####
library(seasonal)
data(holiday) # cny: date of chinese New year
new_year <- function(year) {
  pat <- paste0('^', as.character(year))
  new_year <- seasonal::cny[grepl(pat, seasonal::cny)]
  return(as.Date(new_year, '%Y-%m-%d'))
}

#### Data Import ####
data_2018 <- readr::read_csv('2018.csv')
data_2019 <- readr::read_csv('2019.csv')
data <- readr::read_csv('2005-2017.csv') %>%
  bind_rows(data_2018, data_2019)
colnames(data) <- c('date', 'id', 'station', 'num_in', 'num_out')

data <- data %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d'))

#### Plotting ####

## Function: Filter years and stations
filter_sta_y <- function(data, sta = '台北', years = 2005, months = 1:12) {
  data %>% filter(station %in% sta) %>%
    filter(year(date) %in% years) %>%
    filter(month(date) %in% months)
}

## Function: Plot time series data of 1 year (daily) for many stations
plot_daily <- function(data, print = TRUE) {
  new_year_date <- new_year(lubridate::year(data$date[1]))
  
  pl <- ggplot(data = data) +
    geom_line(aes(
      x = date, 
      y = (num_out - num_in)/(num_in + num_out),
      color = station), 
      size = 0.3) +
    geom_vline(xintercept = as.numeric(new_year_date), 
               linetype = 4, size = 0.2) +
    geom_vline(xintercept = as.numeric(new_year_date - days(1)), 
               linetype = 3, size = 0.2) +
    labs(y = '(出站 - 進站) / 總人數',
         x = lubridate::year(new_year_date)) +
    theme(axis.title.y = element_text(size = 10))
  if (print) print(pl)
  return(pl)
}

