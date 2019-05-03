#setwd('data-raw/')
library(dplyr)
library(sf)
library(railwayViz)

#### Read attribute data: Daily carry ####
carry_2018 <- readr::read_csv('2018.csv')
carry_2019 <- readr::read_csv('2019.csv')
carry <- readr::read_csv('2005-2017.csv') %>%
  bind_rows(carry_2018, carry_2019)
colnames(carry) <- c('date', 'id', 'station', 'num_in', 'num_out')
carry <- carry %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  filter(num_in >=0 & num_out >= 0) %>%
  select(date, station, num_in, num_out)

## Filter Discarded stations
invalid_stations <- readr::read_csv('discarded_new_stations.csv')
invalid_stations <- invalid_stations[!is.na(invalid_stations$`廢站日期`),]$STOP_NAME

carry <- carry %>%
  filter(!(station %in% invalid_stations))


####### Combining Spatial data & calculated stats #######

## Read shape file: Station location
stations <- sf::st_read('station_loc_shp/VP0264V02.shp',
                        stringsAsFactors = FALSE) %>%
  mutate(station = gsub('站$|站\\(.+\\)$', '', landmarkna)) %>%
  mutate(station = gsub('站.+$', '', station)) %>%
  mutate(station = gsub('臺', '台', station)) %>%
  filter(landmarkna != '基隆新站南站')

## Calculate New Year's Eve Stats, Working days stats, & Urbanization index
carry_nye_wdays <- compare_nye2wdays(carry, 2018)

## Convert carry_nye_wdays to spatial data frame
carry_nye_wdays <- to_sf(carry_nye_wdays, stations)


######## plot sf ##########
library(ggplot2)
palette <- tmaptools::get_brewer_pal("RdBu", n = 4)
sqrt4 <- scales::trans_new('sqrt4',
                           function(x) if_else(x > 0, x^(0.25), -(-x)^(0.25)),
                           function(y) if_else(y > 0, y^4, -(-y)^4))

railway <- sf::st_read('railway_shp/VL0305V02.shp',
                       stringsAsFactors = FALSE)
taiwan <- sf::st_read('taiwan_country/TWN_county.shp',
                      stringsAsFactors = FALSE) %>%
  filter(!COUNTYNAME %in% c("澎湖縣", "連江縣", "金門縣"))

tweak <- theme(axis.title.x = element_text(size = 0),
               axis.title.y = element_text(size = 0),
               axis.text.x = element_text(size = 8))

base_map <- ggplot(data = carry_nye_wdays) +
  geom_sf(data = taiwan, colour = "#404040", size = 0.1) +
  scale_x_continuous(limits = c(119.6, 122.3)) + #c(119.7,122.25),
  scale_y_continuous(limits = c(21.65, 25.45))#c(21.8, 25.4),




# New Year's Eve
pl_nye <- base_map +
  geom_sf(aes(color = nye_idx)) +
    scale_colour_gradientn(colours = rev(palette),
                           limits=c(-1, 1),
                           trans = sqrt4) +
  tweak


# Working Days
pl_avg <- base_map +
  geom_sf(aes(color = avg_wday_idx)) +
    scale_colour_gradientn(colours = rev(palette),
                           limits=c(-1, 1),
                           trans = sqrt4) +
  tweak


# Urban Index
#color_rng <- round(max(abs(range(carry_nye_wdays$urban_idx))), 1)
color_rng <- 1

pl_urban <- base_map +
  geom_sf(aes(color = urban_idx)) +
    scale_colour_gradientn(colours = rev(palette),
                           limits = c(-color_rng, color_rng),
                           trans = sqrt4) +
  geom_sf_text(aes(label = if_else(urban_idx >= 0.75,
                                   station, NULL)), color = 'red') +
  geom_sf_text(aes(label = if_else(urban_idx <= -0.75,
                                   station, NULL)), color = 'blue') +
  tweak

