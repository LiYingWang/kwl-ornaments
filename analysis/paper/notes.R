# plot the centroid
map(ornaments_period_join_cen,
    ~ggplot() +
      geom_sf(data = sample_units, fill = NA) +
      geom_point(data = .x,
                 aes(x = lon,
                     y = lat))) %>%
  plot_grid(plotlist  = .)

```{r-gganimate, eval= FALSE}
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

```
