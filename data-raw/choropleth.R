#setwd('data-raw/')
library(dplyr)
library(sf)
library(railwayViz)

#### Data Import ####

# Group stations together to get county level data
fp <- system.file('railway-data', '2005-2018_carry.RDS', package = 'railwayViz')
carry <- readRDS(fp) %>%
  group_by(date, county) %>%
  summarise(num_in = sum(num_in), num_out = sum(num_out)) %>%
  ungroup()

# Calculate Urban index (2005, 2010, 2015, 2018)
carry_nye_wdays <- NULL
for (y in c(2005, 2010, 2015, 2018)) {
  year_data <- compare_nye2wdays(carry, y, groupby = 'county') %>%
    mutate(year = y)
  carry_nye_wdays <- bind_rows(carry_nye_wdays, year_data)
}

# Join spatial data frame
fp <- system.file('railway-data', 'twcounty-shp.RDS', package = 'railwayViz')
taiwan <- readRDS(fp)
carry_nye_wdays <- to_sf(carry_nye_wdays, taiwan)
#saveRDS(carry_nye_wdays, 'final-project/county_urban_index-by_year.RDS')

#### Plot ####
library(ggplot2)
library(sf)
library(rlang)

tweak <- theme(axis.title.x = element_text(size = 0),
               axis.title.y = element_text(size = 0),
               axis.text.x = element_text(size = 5),
               axis.text.y = element_text(size = 5))


# New Year's Eve
pl_nye <- carry_nye_wdays %>% filter(year == 2018) %>%
  pl_choropleth(nye_idx, theme = tweak, print = F)

# Working Days
pl_wdays <- carry_nye_wdays %>% filter(year == 2018) %>%
  pl_choropleth(avg_wday_idx, theme = tweak, print = F)

# Urban Index
pl_urban_idx <- carry_nye_wdays %>% filter(year == 2018) %>%
  pl_choropleth(urban_idx, 'Spectral', tweak, print = F)




#### Facet map: all years ####
pl_facet_urban_idx <- pl_choropleth(carry_nye_wdays, urban_idx,
                              'Spectral', tweak, print = F) +
    facet_wrap( ~ year ,ncol=2, strip.position="top")

  # ggplot(carry_nye_wdays) +
  #   geom_sf(aes(fill = urban_idx),
  #           colour = "#404040", size = 0.1) +
  #   scale_fill_gradientn(colours = rev(palette),
  #                            limits=c(-1, 1),
  #                            trans = railwayViz:::sqrt4) +
  #   scale_x_continuous(limits = c(119.6, 122.3)) +
  #   scale_y_continuous(limits = c(21.65, 25.45)) +
  #   facet_wrap( ~ year ,ncol=2, strip.position="top")
  #   labs(title = "都市化指標：",x = "", y = "")
