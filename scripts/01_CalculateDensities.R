## Get Densities function

## Add transformed coordinates as columns
sightings_all <- sightings_all %>% 
  add_column(
    X = st_coordinates(sightings_sf)[,1],
    Y = st_coordinates(sightings_sf)[,2]
  )

## split the data into seperate sets; one for each species
sightings_split <- sightings_all %>% 
  group_split(Animal, .keep = TRUE) %>% 
  map(.f = list(. %>% select(X, Y)))

## name the list so that each dataset is easier to find
names(sightings_split) <- unique(sightings_all$Animal)

## set the window for the spatial density
ownr_utm <- st_read(
  here("data", "background", "OWNRBufferzone.shp")
) %>% 
  st_transform(crs = 32736)

ownr_buffer <- ownr_utm %>% 
  st_union() %>% 
  st_buffer(dist = 2000)

ownr_window <- as.owin(ownr_buffer)

## Define density function
get_densities <- function(.data, x){
  data_matrix <- as.matrix(.data)
  
  ppp_test <- ppp(
    x = data_matrix[,1],
    y = data_matrix[,2],
    window = ownr_window, check = TRUE
  )
  
  density_spatstat <- density(
    ppp_test, kernel = "quartic", diggle = TRUE,
    leaveoneout = TRUE, sigma = 1000, edge = TRUE, dimyx = 400)
  
  density_stars <- st_as_stars(density_spatstat) %>% 
    st_set_crs(32736) %>%
    st_crop(y = ownr_utm)
}
