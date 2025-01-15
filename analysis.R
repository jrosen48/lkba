library(tidyverse)
library(sf)
library(geosphere) # For distance calculations

# Define the folder containing the .gpx files
gpx_folder <- "gpx"

# Function to process a .gpx file and return trail details
process_gpx_file <- function(file_path) {
  print(paste("Processing file:", file_path)) # Debugging output
  
  # Read the route_points layer
  gpx_data <- tryCatch(
    st_read(file_path, layer = "route_points", quiet = TRUE),
    error = function(e) NULL
  )
  
  # Check if data was read successfully and has coordinates
  if (!is.null(gpx_data) && nrow(gpx_data) > 1) {
    # Extract coordinates and attributes (elevation might be in attributes)
    coords <- gpx_data %>% 
      st_coordinates() %>% 
      as_tibble() %>% 
      rename(lon = X, lat = Y)
    
    # Get the starting point
    start_point <- coords %>% slice(1)
    
    # Calculate distances between consecutive points
    distances <- geosphere::distHaversine(coords[, c("lon", "lat")])
    total_distance_m <- sum(distances, na.rm = TRUE) # Distance in meters
    
    # Convert meters to miles
    total_distance_miles <- total_distance_m * 0.000621371
    
    return(list(
      start_lat = start_point$lat,
      start_lon = start_point$lon,
      trail_distance_miles = total_distance_miles
    ))
  }
  
  # Return NA values if no valid data was found
  return(list(
    start_lat = NA,
    start_lon = NA,
    trail_distance_miles = NA
  ))
}

# Process all .gpx files in the folder
gpx_results <- tibble(file_name = list.files(gpx_folder, pattern = "\\.gpx$", full.names = TRUE)) %>%
  mutate(
    name = basename(file_name) %>% str_remove("\\.gpx$"),
    results = map(file_name, process_gpx_file),
    start_lat = map_dbl(results, "start_lat"),
    start_lon = map_dbl(results, "start_lon"),
    trail_distance_miles = map_dbl(results, "trail_distance_miles"),
  ) %>%
  select(-results) # Remove intermediate list column

# View the results
gpx_processed <- gpx_results %>% 
  arrange(desc(name)) %>% 
  select(-file_name)

overall_data <- readxl::read_excel("lkba-trails-data.xlsx") %>% 
  rename(name = 1)

selected_data <- overall_data %>% 
  select(1, 2, 3, 4, 7:16)

gpx_processed %>% 
  left_join(selected_data) %>% 
  select(name, start_lat, start_lon) %>% 
  write_csv("lkba-trails-data.csv")
