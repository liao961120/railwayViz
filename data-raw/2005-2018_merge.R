library(dplyr)
library(sf)
library(railwayViz)
library(jsonlite)

#### Read attribute data: Daily carry ####
carry_2018 <- readr::read_csv('2018.csv')
carry <- readr::read_csv('2005-2017.csv') %>%
  bind_rows(carry_2018)
colnames(carry) <- c('date', 'id', 'station', 'num_in', 'num_out')
carry <- carry %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  filter(num_in >=0 & num_out >= 0) %>%
  select(date, station, num_in, num_out) %>%
  mutate(station = gsub('站$','', station))

## Filter Discarded stations

invalid_stations <- readr::read_csv('discarded_new_stations.csv')
invalid_stations <- invalid_stations[!is.na(invalid_stations$`廢站日期`),]$STOP_NAME

carry <- carry %>%
  filter(!(station %in% invalid_stations))


#### Add county/city location ####
stations <- sf::st_read('station_loc_shp/VP0264V02.shp',
                        stringsAsFactors = FALSE) %>%
  mutate(station = gsub('站$|站\\(.+\\)$', '', landmarkna)) %>%
  mutate(station = gsub('站.+$', '', station)) %>%
  mutate(station = gsub('臺', '台', station)) %>%
  filter(landmarkna != '基隆新站南站')

st_geometry(stations) <- NULL

# Extract county/city from address
stations <- stations %>%
  mutate(county = substr(address, 1, 3)) %>%
  select(station, county)
stations$county[which(stations$county == '中巿后')] <- '臺中市'
stations <- rbind(stations, c('猴硐', '新北市'))

# Second source
stations2 <- fromJSON('station_info.json') %>%
  rename(station = stationName, address = stationAddrTw) %>%
  select(station, address) %>%
  mutate(station = gsub('臺', '台', station))
# Extract county/city from address
for (i in 1:nrow(stations2)) {
  stations2$county[i] <- unlist(
    strsplit(stations2$address, ' ')[[i]][1]
  )
}

stations2 <- stations2[,-2]

s <- c("佳冬","永春","長榮","林榮新","美術館","鼓山","三塊厝","民族","科工")
loc <- c('屏東縣','宜蘭縣','臺南市','花蓮縣','高雄市','高雄市','高雄市','高雄市','高雄市')
for (i in seq_along(s)){
  idx <- which(stations2$station == s[i])
  if (length(idx) != 0) {
    stations2$county[idx] <- loc[i]
  } else {
    stations2 <- rbind(stations2, c(s[i], loc[i]))
  }
}


# Join two sources
stations <- bind_rows(stations2, stations) %>%
  distinct(station, .keep_all = TRUE)

# Manually convert non-county level to county level
non_county <- c('彰化市')
county <- c('彰化縣')
for (i in seq_along(non_county)) {
  stations$county[which(stations$county == non_county[i])] <- county[i]
}

# Left join address to carry
carry <- left_join(carry, stations, by = c('station' = 'station'))


# Save binary data
saveRDS(object = carry, file = '2005-2018_carry.RDS')
saveRDS(object = carry, file = '../inst/railway-data/2005-2018_carry.RDS')
