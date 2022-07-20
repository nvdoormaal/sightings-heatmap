###################################
## Creating heatmap of sightings ##
###################################

## Add explanation


## Load packages
library(tidyverse)
library(here)
library(sf)
library(lubridate)
library(spatstat)
library(stars)


## Load data
sightings_file <- list.files(
  path = here("data"), pattern = ".csv", full.names = TRUE
)
sightings_raw <- read_delim(sightings_file, delim = ",") %>% 
  mutate(
    Identifier = str_to_lower(Identifier),
    Animal = str_to_lower(Animal)
  ) %>% 
  filter(
    !is.na(longitude) | !is.na(latitude),
    Animal == "black rhino" | Animal == "white rhino"
  )

## split camera trap data and in-person observations
ct_data <- sightings_raw %>% 
  filter(
    str_detect(Identifier, pattern = "camera")
  )
observations <- sightings_raw %>% 
  filter(
    str_detect(Identifier, pattern = "camera", negate = TRUE) |
      is.na(Identifier)
  )

## Create distance matrix to group nearby cameras
distance_value <- 400

ct_sf <- ct_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = 32736)

ct_locations <- ct_sf %>% 
  st_is_within_distance(y = ct_sf, dist = distance_value) %>% 
  unique()

## Assing groups to locations that are close to each other
ct_groups <- list()

for (i in 1:length(ct_locations)) {
ct_groups[[i]] <- ct_sf %>% 
    slice(., ct_locations[[i]]) %>% 
  mutate(
    group = LETTERS[i]
  )
}
ct_groups <- bind_rows(ct_groups)  

## Select only one (random) observation per species per CT location per week
ct_subset <- ct_groups %>% 
  st_drop_geometry() %>% 
  slice_sample(n = nrow(ct_groups), replace = FALSE) %>% 
  mutate(
    weeknumber = week(dateOccurred)
  ) %>% 
  distinct(Animal, group, weeknumber, .keep_all = TRUE) %>% 
  select(-c(group, weeknumber))

## Combine the ct subset with the other locations
sightings_all <- observations %>% 
  bind_rows(ct_subset)

## Convert to spatial object
sightings_sf <- sightings_all %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = 32736) %>% 
  st_jitter(amount = 5)

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
ownr_utm <- ownr_utm[-1,]

ownr_buffer <- ownr_utm %>% 
  st_union() %>% 
  st_buffer(dist = 2000)

ownr_window <- as.owin(ownr_buffer)

A <- function(.data, x){
  data_matrix <- as.matrix(.data)
  
  ppp_test <- ppp(
    x = data_matrix[,1],
    y = data_matrix[,2],
    window = ownr_window, check = TRUE
  )
  
  density_spatstat <- density(
    ppp_test, kernel = "quartic", diggle = TRUE,
    leaveoneout = TRUE, sigma = 500, edge = TRUE, dimyx = 200)
  
  density_stars <- st_as_stars(density_spatstat)
  density_sf <- st_as_sf(density_stars)
}
  

ppp_test <- ppp(
  x = A[,1],
  y = A[,2],
  window = ownr_window, check = TRUE
)

sightings_ppp <- (sightings_sf)

Window(sightings_ppp) <- as.owin(ownr_utm)

density_spatstat <- density(ppp_test, kernel = "quartic", diggle = TRUE,
                            leaveoneout = TRUE, sigma = 500, edge = TRUE,
                            dimyx = 200)

density_stars <- stars::st_as_stars(density_spatstat)
density_sf <- st_as_sf(density_stars) %>%
  st_set_crs(32736) %>% 
  st_intersection(y = ownr_utm)


quartic_sigma <- ggplot() +
  geom_sf(data = density_sf %>% filter(v>0.0000001), 
          aes(fill = v), col = NA) +
  scale_fill_viridis_c() + 
  geom_sf(data = st_boundary(ownr_utm)) +
  labs(title = "quartic - Sima") +
  theme_void() +
  theme(
    legend.position = "bottom"
  )

quartic_sigma



