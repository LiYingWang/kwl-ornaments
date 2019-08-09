library(sf)
library(cartography)
# read in the spatial data
burial <- invisible(st_read(here("analysis", "data", "raw_data", "AD_burial.shp"), quiet = TRUE))
AD <- invisible(st_read(here("analysis", "data", "raw_data", "AD_zone.shp"), quiet = TRUE))
AD_data <- invisible(st_read(here("analysis", "data", "raw_data", "AD_withdata.shp"), quiet = TRUE))
location <- invisible(st_read(here("analysis", "data", "raw_data", "location.shp"), quiet = TRUE))
units_40 <- invisible(read.csv(here("analysis", "data", "raw_data", "kwl-list-of-sampling-squares.csv")))

# tidy spatial data
# 40 sampling units
sample_units <-
  AD_data %>%
  semi_join(units_40, by = c (`坑號` = "the_sq"))

# join two dataset to get coordinate
ornaments_period_join <-
  ornaments_period %>%
  mutate(period = as.factor(period)) %>% # seems need this line first
  group_by(Pit_No, period) %>%
  count(Categories) %>%
  inner_join(AD_data, by = c("Pit_No" = "坑號"))

# mapping sampling area
ggplot(sample_units) +
  geom_sf(data = AD_data, fill = NA) +
  geom_sf(color = 'red') +
  geom_sf_text(aes(label = `坑號`), size = 2.5, alpha = 0.9) +
  theme_minimal() +
  theme_void()

#1. Need to say that this is a map of excavation areas at Kiwulan, because the reader cannot see the site name anywhere in this figure.
#2. Need to add in here that each square is 4 x 4 m
#3. and say which way north is. Or add to the map and scale and north arrow.

ggsave("analysis/figures/KWL-excavation-map.png")
