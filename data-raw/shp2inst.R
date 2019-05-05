library(dplyr)

railway <- sf::st_read('railway_shp/VL0305V02.shp',
                       stringsAsFactors = FALSE)
saveRDS(railway, 'railway-shp.RDS')
saveRDS(railway, '../inst/railway-data/railway-shp.RDS')


taiwan <- sf::st_read('taiwan_county/TWN_county.shp',
                      stringsAsFactors = FALSE) %>%
  filter(!COUNTYNAME %in% c("澎湖縣", "連江縣", "金門縣")) %>%
  rename(county = COUNTYNAME)
saveRDS(taiwan, 'twcounty-shp.RDS')
saveRDS(taiwan, '../inst/railway-data/twcounty-shp.RDS')

stations <- sf::st_read('station_loc_shp/VP0264V02.shp',
                        stringsAsFactors = FALSE) %>%
  mutate(station = gsub('站$|站\\(.+\\)$', '', landmarkna)) %>%
  mutate(station = gsub('站.+$', '', station)) %>%
  mutate(station = gsub('臺', '台', station)) %>%
  filter(landmarkna != '基隆新站南站')
saveRDS(stations, 'station-points-shp.RDS')
saveRDS(stations, '../inst/railway-data/station-points-shp.RDS')
