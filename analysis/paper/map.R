library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("here")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# read Taiwan administrative areas
TW_adm <- st_read(here("analysis", "data", "raw_data","TWN_adm2.shp"))
TW_map <- fortify(TW_adm)

# add site location
Kiwulan <- data.frame(lon = c(121.7809), lat = c(24.8066))

TW_SE_Asia <-
ggplot(data = world) +
  geom_sf() +
  geom_rect(xmin = 119.8, xmax = 122.2, ymin = 21.7, ymax = 25.5,
            fill = NA, colour = "red", size = 0.5) +
  coord_sf(xlim = c(107, 135), ylim = c(13, 35), expand = FALSE, datum = NA)

TW_SE_Asia_void <-
  TW_SE_Asia +
  theme_void() +
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA))

library("ggspatial")
TW_map_with_site <-
ggplot(data = world) +
  geom_sf(data = TW_map) +
  geom_point(data = Kiwulan, aes(x = lon, y = lat),
             size = 3, color = "red") +
  geom_text(aes(x = lon, y = lat, label = "Kiwulan"),
            data = Kiwulan, position = position_nudge(y = -0.1)) +
  coord_sf(xlim = c(118, 122.2), ylim = c(21.9, 25.4), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.45, "in"),
                         style = north_arrow_fancy_orienteering)

TW_map_with_site +
  annotation_custom(
    grob = ggplotGrob(TW_SE_Asia),
    xmin = 118,
    xmax = 120,
    ymin = 23.5,
    ymax = 25.5)
