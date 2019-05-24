# This script converts
# `county_urban_index-by_year.RDS` (cache from data-raw/choropleth.R) &
# `station_urban_index-2018.RDS` (cache from data-raw/shp_read.R) to shapefiles.
# These RDS files each store a sf dataframe
source('funcs.R')
library(dplyr)
library(sf)

# county_urban_index-by_year.RDS
county_urban <- readRDS('county_urban_index-by_year.RDS') %>%
  filter(year == 2018) %>%
  select(-year, -num_in_nye, -num_out_nye) %>%
  rename(avg_idx = avg_wday_idx) # Rename because shapefile truncates field names

sf::st_write(county_urban, 'county_urban_idx/county_urban_idx_2018.shp',
             layer_options = 'ENCODING=UTF-8', delete_layer = TRUE)
#View(st_read('county_urban_idx/county_urban_idx_2018.shp'))

# station_urban_index-2018.RDS
station_urban <- readRDS('station_urban_index-2018.RDS') %>%
  select(-address) %>%
  rename(avg_idx = avg_wday_idx)
sf::st_write(station_urban, 'station_urban_idx/station_urban_idx_2018.shp',
             layer_options = 'ENCODING=UTF-8', delete_layer = TRUE)
#View(st_read('station_urban_idx/station_urban_idx_2018.shp'))

# Save Taiwan sf RDS to shapefile.
fp <- system.file('railway-data', 'twcounty-shp.RDS', package = 'railwayViz')
taiwan <- readRDS(fp)
sf::st_write(taiwan, 'taiwan/taiwan_county.shp',
             layer_options = 'ENCODING=UTF-8', delete_layer = TRUE)
#View(st_read('taiwan/taiwan_county.shp'))
