## Define data cleaning for CT and direct sightings
data_cleaning <- function(data){
  
## split camera trap data and in-person observations
ct_data <- data %>% 
  filter(
    str_detect(Identifier, pattern = "camera")
  )
observations <- data %>% 
  filter(
    str_detect(Identifier, pattern = "camera", negate = TRUE) |
      is.na(Identifier)
  )

if (nrow(ct_data) == 0) {
  return(observations)
}

## Run the following code if there is at least one CT location
if (nrow(ct_data) > 0) {
  
## Create distance matrix to group nearby cameras
distance_value <- 400

ct_sf <- ct_data %>% 
  filter(!is.na(longitude) | !is.na(latitude)) %>% 
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
observations %>% 
  bind_rows(ct_subset)
}

}
