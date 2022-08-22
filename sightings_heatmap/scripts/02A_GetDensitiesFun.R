# Define density function
get_densities <- function(.data, x){
  
  data_matrix <- as.matrix(.data)
  
  ppp_test <- ppp(
    x = data_matrix[,1],
    y = data_matrix[,2],
    window = ownr_window, check = TRUE
  )
  
  density_spatstat <- density.ppp(
    ppp_test, kernel = "quartic", diggle = TRUE,
    leaveoneout = TRUE, sigma = 1000, edge = TRUE, dimyx = 400)
  
  density_stars <- st_as_stars(density_spatstat) %>%
    st_set_crs(32736) %>%
    st_crop(y = ownr_utm) %>% 
    mutate(v_km = v * 1000000)
  
  return(density_stars)
}