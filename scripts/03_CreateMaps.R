## Define density-map function
make_maps <- function(density_obj, species){
  
  ## Extract info to show on the map
  obs_counts <- data() %>% 
    filter(Animal == species & 
             str_detect(Identifier, pattern = "camera", negate = TRUE)) %>% 
    nrow()
  
  ct_counts <- data() %>% 
    filter(Animal == species & str_detect(Identifier, pattern = "camera")) %>%
    nrow()
  
  ## Create the map
  ggplot() +
    layer_spatial(data = ownr_utm, fill = "#90ee90", alpha = 0.2) +
    geom_stars(data = density_obj, 
               aes(x = x, y = y, fill = v_km, alpha = v_km)) +
    scale_fill_viridis_c("Density", option = input$colourScheme,
                         limits = c(0, highest_value()),
                         na.value = "transparent") +
    scale_alpha_continuous(range = c(0, 1), guide = "none") +
    labs(title = paste("Distribution of", species, "in OWNR"),
         subtitle = paste("Based on", obs_counts, "reported sightings and",
                          ct_counts, "camera trap observations"),
         caption = paste("map produced on", format(Sys.Date(), "%d %b %Y"))) +
    annotation_scale(location = "bl", height = unit(0.5, "cm"),
                     text_cex = 1) +
    annotation_north_arrow(location = "tr",
                           height = unit(1, "cm"), width = unit(1, "cm")) +
    theme_void() +
    theme(
      legend.position = c(0.125, 0.15),
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(),
      plot.caption = element_text(),
      plot.title.position = "panel",
      plot.caption.position = "panel",
      plot.margin = unit(c(0,0,0,0),"mm"),
      panel.background = element_rect(fill = "white", color = "black", size = 2)
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top", title.hjust = 0.5, ticks = TRUE
      )
    )
}
