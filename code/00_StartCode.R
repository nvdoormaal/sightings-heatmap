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
library(ggspatial)

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

## Run the 'get_density' for each species in the list
all_densities <- map(sightings_split, get_densities)

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