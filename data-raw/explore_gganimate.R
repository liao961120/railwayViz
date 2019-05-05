library(dplyr)
library(railwayViz)
fp <- system.file('railway-data', '2005-2018_carry.RDS', package = 'railwayViz')
carry <- readRDS(fp) %>%
  group_by(date, county) %>%
  summarise(num_in = sum(num_in), num_out = sum(num_out)) %>%
  ungroup()

# Calculate Urban index
carry_nye_wdays <- NULL
for (y in 2005:2018) {
  year_data <- compare_nye2wdays(carry, y, groupby = 'county') %>%
    mutate(year = y)
  carry_nye_wdays <- bind_rows(carry_nye_wdays, year_data)
}

# Join spatial data frame
fp <- system.file('railway-data', 'twcounty-shp.RDS', package = 'railwayViz')
taiwan <- readRDS(fp)
carry_nye_wdays <- to_sf(carry_nye_wdays, taiwan)



### Plot ###
library(ggplot2)
library(gganimate)
library(rlang)
palette <- tmaptools::get_brewer_pal("Spectral", n = 4, plot = F)



plot <- pl_tw_idx(data2, urban_idx, 'Spectral')


pl <- ggplot(data = carry_nye_wdays) +
    geom_sf(aes(fill = urban_idx, frame = year),
            colour = "#404040", size = 0.1) +
    scale_fill_gradientn(colours = rev(palette),
                             limits=c(-1, 1),
                             trans = railwayViz:::sqrt4) +
    scale_x_continuous(limits = c(119.6, 122.3)) +
    scale_y_continuous(limits = c(21.65, 25.45)) +
    labs(title = "都市化指標：",x = "", y = "")


gganimate::gganimate(pl, ani.width=400,
          ani.height=500, interval = 1.2,
          filename="animated_urban_idx.gif")

