## Get lowest and highest value to use in the legend of the map later
combined_densities <- all_densities %>% 
  map(as.data.frame) %>% 
  bind_rows() %>% 
  filter(!is.na(v))

lowest_value <- combined_densities %>% 
  pull(v) %>% 
  quantile(0.5, na.rm = TRUE)

middle_value <- combined_densities %>% 
  pull(v) %>% 
  quantile(0.85, na.rm = TRUE)

highest_value <- combined_densities %>% 
  pull(v) %>% 
  quantile(0.99, na.rm = TRUE)

## Define density-map function
make_maps <- function(density_obj, species){
  
  ## Extract info to show on the map
  obs_counts <- observations %>% 
    filter(Animal == species) %>% nrow()
  ct_counts <- ct_subset %>% 
    filter(Animal == species) %>% nrow()
  
  ## Create the map
  ggplot() +
    geom_stars(data = density_obj, 
               aes(x = x, y = y, fill = v, alpha = v)) +
    scale_fill_viridis_c("Density", option = "inferno", 
                         limits = c(lowest_value, highest_value), na.value = NA, 
                         breaks = c(lowest_value, middle_value, highest_value),
                         labels = c("low", "med", "high")) +
    scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
    scale_x_continuous(expand = c(0.1,0.1)) +
    scale_y_continuous(expand = c(0.1,0.1)) +
    geom_sf(data = st_boundary(ownr_utm)) +
    labs(title = paste("Density map of", species, "in OWNR"),
         subtitle = paste("Based on", obs_counts, "reported sightings and",
                          ct_counts, "camera trap observations")) +
    annotation_scale(location = "bl", height = unit(0.5, "cm"),
                     text_cex = 1.5, text_face = "bold") +
    annotation_north_arrow(location = "tr",
                           height = unit(2, "cm"), width = unit(2, "cm")) +
    theme_void() +
    theme(
      legend.position = "bottom",
      title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12),
      panel.background = element_rect(fill = "white", color = "black", size = 2),
      plot.background = element_rect(fill = "white", color = "white"),
      plot.margin = unit(c(0,0,0,0), "cm")
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top", title.hjust = 0.5, ticks = FALSE
      )
    )
}
## Create the maps
density_maps <- map2(all_densities, names(all_densities), make_maps)
