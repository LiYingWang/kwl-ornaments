library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("here")
library("ggspatial")
#devtools::install_github('3wen/legendMap')
library(legendMap)

world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# read Taiwan administrative areas
TW_adm <- st_read(here("analysis", "data", "raw_data","TWN_adm2.shp"))
TW_map <- fortify(TW_adm)

# add site location
site_location <-
  data.frame(location = c("Kiwulan", "Keeling", "Tamsui", "Hualien"),
             lon = c(121.7809, 121.7621, 121.4349, 121.6084),
             lat = c(24.8066, 25.1523, 25.1764, 24.0228))

TW_SE_Asia <-
ggplot(data = world) +
  geom_sf() +
  geom_rect(xmin = 120.9, xmax = 122.2, ymin = 24.4, ymax = 25.5,
            fill = NA, colour = "red", size = 0.5) +
  coord_sf(xlim = c(107, 135), ylim = c(5, 45), expand = FALSE, datum = NA) +
  theme(panel.background = element_rect(fill = "azure"))

# Topographic map
library(ggmap)
library(tmaptools)
library(shadowtext)
# we don't want to download every time, so let's save the map locally
# from https://stackoverflow.com/a/52710855/1036500
tw_map <- ggmap(get_stamenmap(rbind(as.numeric(c(120.7, 24.2,
                                                 122.1, 25.6))), zoom = 10))
#saveRDS(tw_map, here("analysis", "data", "raw_data", "tw_map.rds"))
#tw_map <- readRDS(here("analysis", "data", "raw_data", "tw_map.rds"))
pg <- ggplot_build(tw_map)

TW_map_with_site <-
tw_map +
  geom_point(data = site_location,
             aes(x = lon,
                 y = lat),
             size = 2,
             color = "red") +
  geom_shadowtext(data = site_location,
            aes(x = lon,
                y = lat,
                label = location),
            size = 5,
            position = position_nudge(y = 0.05)) +
  coord_sf(xlim = c(120.9, 122.7),
           ylim = c(24.4, 25.5),
           expand = FALSE) +
  scale_x_continuous(breaks = c(121.0, 121.5, 122.0),
                     limits = c(120.9, 122.7)) +
  legendMap::scale_bar(
    # edit these numbers to select a suitable location
    # for the scale bar where it does not cover
    # important details on the map
    lon = 121.0,
    lat = 24.45,
    # distance of one section of scale bar, in km
    distance_lon = 20,
    # height of the scale bar, in km
    distance_lat = 1,
    # distance between scale bar and units, in km
    distance_legend = 5,
    # units of scale bar
    dist_unit = "km",
    # add the north arrow
    orientation = TRUE,
    # length of N arrow, in km
    arrow_length = 5,
    # distance between scale bar & base of N arrow, in km
    arrow_distance = 8,
    # size of letter 'N' on N arrow, in km
    arrow_north_size = 5)

TW_map_with_site +
annotation_custom(
grob = ggplotGrob(TW_SE_Asia),
xmin = 122.1,
xmax = 122.7,
ymin = 24.5,
ymax = 25.4)

ggsave("analysis/figures/kiwulan-location-map.png")

# add the location of Keelung, Tamsui, and Heping dau
