## Define Densities function
calculate_densities <- function(data){

## Add transformed coordinates as columns
sightings_sf <- 
  data %>%
  filter( !is.na(longitude) | !is.na(latitude) ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = 32736) %>% 
  st_jitter(amount = 5)

sightings_all <- data %>%
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

return(sightings_split)
}
