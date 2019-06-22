# plot the centroid
map(ornaments_period_join_cen,
    ~ggplot() +
      geom_sf(data = sample_units, fill = NA) +
      geom_point(data = .x,
                 aes(x = lon,
                     y = lat))) %>%
  plot_grid(plotlist  = .)

