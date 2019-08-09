# plot the centroid
map(ornaments_period_join_cen,
    ~ggplot() +
      geom_sf(data = sample_units, fill = NA) +
      geom_point(data = .x,
                 aes(x = lon,
                     y = lat))) %>%
  plot_grid(plotlist  = .)


# test gganimate for measurement of beads by period
# devtools::install_github('thomasp85/gganimate')
library(gganimate)

anim_gold_bead <-
  ggplot(ornaments_intact_gold,
         aes(Length, Width)) +
  geom_point() +
  transition_states(period, 3, 1) +
  labs(title = 'Period: {closest_state}',
       x ='Length (mm) of golden bead',
       y ='Width (mm) of golden bead')

# does not work
anim_gold_bead_spatial <-
  ggplot(ornaments_period_join_gold) +
  geom_sf(data = sample_units, fill = NA) +
  geom_sf(data = ornaments_period_join_gold, aes(fill = n), alpha = 0.9) +
  transition_states(period, 3, 1) +
  scale_fill_gradient(high = "black", low = "grey") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 11))

# categories of ornaments by period
ornaments_count <-
  ornaments_period %>%
  filter(!is.na(`6-layers`),
         Categories != "Unknown Metal") %>%
  ungroup() %>%
  mutate(Categories = fct_lump(Categories,
                               n = 5)) %>%
  mutate(Categories = fct_infreq(Categories)) %>%
  mutate(Categories = fct_rev(Categories)) %>%
  group_by(period, Categories) %>%
  count()

# histogram facetting for three periods
orna_periods_facet <-
  ggplot(ornaments_count) +
  geom_col(aes(Categories, n)) +
  facet_wrap(~ period) +
  coord_flip() +
  labs(y = "Frequency") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# stack histogram
orna_all_viridis <-
  ggplot(ornaments_count) +
  geom_col(aes(Categories, n, fill = period)) +
  labs(y = "Frequency", x = "Categories") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  scale_fill_viridis_d()

library(cowplot)
library(viridis)
# add two plots together
ggdraw() +
  draw_plot(orna_all_viridis +
              theme(legend.justification = "top"), 0, 0, 1, 1) +
  draw_plot(orna_periods_facet + scale_color_viridis(discrete = TRUE) +
              theme(legend.justification = "bottom"), 0.5, 0.15, 0.5, 0.5) +
  draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.7), size = 15)


# plot for intact golden bead by period
ornaments_intact_gold <-
  ornaments_period %>%
  filter(Condition %in% c('complete', 'near-complete'),
         Categories == 'Golden bead')

ggplot(ornaments_intact_gold) +
  geom_point(aes(Length, Width, col = Perforation_average)) +
  facet_wrap(~ period) +
  labs(x = "Length (mm)",
       y = "Width (mm)",
       title = "Golden beads") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

# plot for intact glass bead by period, need to deal with the original data, haven't done yet

# plot for intact agate bead by period
ornaments_intact_agate <-
  ornaments_period %>%
  filter(Condition %in% c('complete','near-complete'),
         Categories == 'Agate bead')

ggplot(ornaments_intact_agate,
       aes(Length, Width, col = period))+
  geom_point() +
  labs(x = "Length (mm)",
       y = "Width (mm)",
       title = "Agate beads") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

# scatter plot for intact bell by period
ornaments_intact_bell <-
  ornaments_period %>%
  filter(Condition %in% c('complete', 'near-complete'),
         Categories == 'Bell',
         Type != 'unclassified') %>%
  mutate(Length = as.numeric(gsub("\\+", "", `Length(mm)`))) #remove +

ggplot(ornaments_intact_bell) +
  geom_point(aes(Length, Width, col = Type)) +
  facet_wrap(~ period) +
  labs(x = "Length (mm)",
       y = "Width (mm)",
       title = "Bell") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

# spatial pattern
# filter metal ring
ornaments_period_join_metal <-
  ornaments_period_join %>%
  filter(Categories %in% 'Metal ring')

# plot by period
dis_metal <-
  ggplot(ornaments_period_join_metal) +
  geom_sf(data = sample_units, fill = NA) +
  geom_sf(data = ornaments_period_join_metal,
          aes(geometry = geometry,
              fill = n), alpha = 0.9) +
  facet_wrap(~ period) +
  labs(title = "Metal Ring",
       fill = "frequency") +
  scale_fill_gradient(high = "black", low = "grey") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 11))

# filter bell
ornaments_period_join_bell <-
  ornaments_period_join %>%
  filter(Categories %in% 'Bell')

# plot by period
dis_bell <-
  ggplot(ornaments_period_join_bell) +
  geom_sf(data = sample_units, fill = NA) +
  geom_sf(data = ornaments_period_join_bell,
          aes(geometry = geometry,
              fill = n), alpha = 0.9) +
  facet_wrap(~ period) +
  labs(title = "Copper Bell",
       fill = "frequency") +
  scale_fill_gradient(high = "black", low = "grey") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 11))
