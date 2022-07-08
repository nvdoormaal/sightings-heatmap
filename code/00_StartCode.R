###################################
## Creating heatmap of sightings ##
###################################

## Add explanation


## Load packages
library(dplyr)
library(readr)
library(here)
library(stringr)
library(sf)
library(lubridate)

## Load data
sightings_file <- list.files(
  path = here("data"), full.names = TRUE
)
sightings_raw <- read_delim(sightings_file, delim = ";") %>% 
  mutate(
    Identifier = str_to_lower(Identifier)
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

## Assing groups to nearby camera traps
ct_groups <- list()

for (i in 1:length(ct_locations)) {
ct_groups[[i]] <- ct_sf %>% 
    slice(., ct_locations[[i]]) %>% 
  mutate(
    group = LETTERS[i]
  )
}
ct_groups <- bind_rows(ct_groups)  

ct_group_time <- ct_groups %>% 
  st_drop_geometry() %>% 
  slice_sample(n = nrow(ct_groups), replace = FALSE) %>% 
  mutate(
    dateOccurred = ymd_hm(dateOccurred),
    weeknumber = week(dateOccurred)
  ) %>% 
  distinct(Animal, group, weeknumber, .keep_all = TRUE)






