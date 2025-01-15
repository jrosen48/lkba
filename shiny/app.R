## loading packages

library(shiny)
library(sf)
library(mapboxer)
library(shinythemes)
library(sfheaders)
library(tidyverse)

source("token.R")

## loading data

gpx_folder <- "gpx"

process_gpx_file <- function(file_path) {
  
  print(paste("Processing file:", file_path)) # Debugging output
  
  # Read the route_points layer
  gpx_data <- st_read(file_path, layer = "route_points", quiet = TRUE) %>% 
    st_combine() %>%
    st_cast("LINESTRING") %>% 
    pluck(1)
  
}

get_start_points <- function(file_path) {
  
  print(paste("Processing file:", file_path)) # Debugging output
  
  # Read the route_points layer
  gpx_data <- st_read(file_path, layer = "route_points", quiet = TRUE) %>% 
    st_combine() %>%
    st_cast("LINESTRING") %>% 
    pluck(1)
  
  start_point <- gpx_data %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    rename(lon = X, lat = Y) %>% 
    slice(1)
  
}

file_name <- list.files(gpx_folder, pattern = "\\.gpx$", full.names = TRUE)

results <- map(file_name, process_gpx_file)
start_points <- map(file_name, get_start_points)

start_points <- start_points %>% 
  map_df(~.) %>% 
  mutate(L1 = file_name %>% basename() %>% gsub("\\.gpx$", "", .))

start_points <- st_as_sf(
  start_points,
  coords = c("lon", "lat"),   # Specify longitude and latitude columns
  crs = 4326,                 # Set CRS to WGS84 (EPSG:4326)
  remove = FALSE              # Keep the original lat/lon columns
)

linestrings <- st_sfc(results)

sf_linestrings <- st_sf(geometry = linestrings)

sf_linestrings$trail_name <- file_name %>% 
  basename() %>% 
  gsub("\\.gpx$", "", .)


overall_data <- readxl::read_excel("lkba-trails-data.xlsx") %>% 
  rename(trail_name = 1) %>% 
  rename(typed_name = 2) %>% 
  mutate(`Trail Distance (Miles)` = round(`Trail Distance (Miles)`, 1))

sf_linestrings <- sf_linestrings %>% 
  left_join(overall_data)

## app

## UI
ui <- navbarPage(
  title = "Little Kids, Big Adventures Around Knoxville, TN",
  theme = shinytheme("cerulean"), # Updated theme to "cerulean"
  
  # Tab: About
  tabPanel(
    title = "About",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          h2("Little Kids, Big Adventures Around Knoxville, Tennessee"),
          h4("Hiking with Children in the Tennessee Valley, Cumberland Plateau, and the Great Smoky Mountains"),
          h5("A book to be published by the University of Tennessee Press (Expected 2025)"),
          hr(),
          p("Created by Katie Rosenberg and Joshua Rosenberg"),
          p("Explore the map to find your next trail, and happy hiking!"),
          tags$figure(
            class = "centerFigure",
            tags$img(
              src = "arm in arm.jpg",
              width = 300,
            )
          )
        )
      )
    )
  ),
  
  # Tab: Map
  tabPanel(
    title = "Overview",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          h4("The 30 Hikes in the Book"),
          tableOutput("hikes")
        )
      )
    )
  ),
  
  # Tab: Map
  tabPanel(
    title = "Map",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          mapboxerOutput("map", width = "100%", height = "800px") # Full-width map
        )
      )
    )
  )
  
)


## Server
server <- function(input, output) {
  
  output$hikes <- renderTable({
    readxl::read_xlsx("lkba-trails-data.xlsx") %>% 
      select(-1) %>% 
      select(1, 2, 3, `Trailhead Address`, 7:14) %>% 
      mutate(`Trail #` = as.integer(`Trail #`)) %>% 
      select(`Trail #`, everything())
  })
  
  output$map <- renderMapboxer({
    # Setup a map with the default source above
    as_mapbox_source(sf_linestrings) %>%
      mapboxer(
        style = "mapbox://styles/mapbox/outdoors-v11", # Outdoor style for hiking
        center = c(-83.9207, 35.9606), # Longitude and Latitude for Knoxville, TN
        zoom = 8, # Appropriate zoom for Knoxville region
        token = mapbox_token
      ) %>%
      # Add a navigation control
      add_navigation_control() %>%
      # Add a line layer for the routes with names
      add_line_layer(
        line_color = "#8A2BE2", # Purple trail color
        line_width = 5,
        popup = paste0(
          "<b>Trail Name:</b> {{typed_name}}<br>",
          "<b>Trailhead Address:</b> {{Trailhead Address}}<br>",
          "<b>Region:</b> {{Region}}<br>",
          "<b>Trail Distance:</b> {{Trail Distance (Miles)}} miles<br>",
          "<b>Elevation Change:</b> {{Elevation Change}}<br>",
          "<b>Pets:</b> {{Pets}}<br>",
          "<b>Time Estimate (Fast):</b> {{Time Estimate - Hiking Fast (Hours)}} hours<br>",
          "<b>Time Estimate (Slow):</b> {{Time Estimate - Hiking Slowly (Hours)}} hours<br>",
          "<b>Parking Pass/Entrance Fee:</b> {{Parking Pass/Entrance Fee}}<br>",
          "<b>Restroom(s):</b> {{Restroom(s)}}<br>",
          "<b>Terrain:</b> {{Terrain}}<br>"
        )
      ) %>%
      add_circle_layer(
        source = as_mapbox_source(start_points),
        circle_color = "rgba(128, 128, 128, 0.8)", # Light gray with transparency
        circle_radius = 3,
        circle_stroke_color = "rgba(0, 0, 0, 0.5)", # Subtle black border
        circle_stroke_width = 1
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
