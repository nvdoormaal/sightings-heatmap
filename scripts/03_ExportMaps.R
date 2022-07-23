## Export maps in a loop
formatted_date <- format(Sys.Date(), format = "%y%m%d")

for (i in 1:length(density_maps)) {
  export_name <- gsub(pattern = " ", replacement = "", names(density_maps)[i])
  
  ggsave(plot = density_maps[[i]], 
         filename = here("output", paste(formatted_date, export_name, "densitymap.png", sep = "_")),
         device = "png", dpi = 400, type = "cairo",
         width = 14, height = 10, units = "cm", scale = 2
  )
}