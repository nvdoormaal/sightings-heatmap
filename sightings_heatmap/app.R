#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)
library(DT)
library(tidyverse)
library(here)
library(sf)
library(lubridate)
library(spatstat)
library(stars)
library(ggspatial)
library(patchwork)
library(shinybusy)
library(grid)

gifs <- c(
  "https://media4.giphy.com/media/HnrKxE23lWCbu/giphy.gif",
  "https://c.tenor.com/_kEbS7KpN6oAAAAC/lol-omg.gif",
  "https://c.tenor.com/_TV6qVC4toAAAAAd/panda-dancing.gif",
  "https://c.tenor.com/_TV6qVC4toAAAAAd/panda-dancing.gif",
  "https://c.tenor.com/_TV6qVC4toAAAAAd/panda-dancing.gif",
  "https://c.tenor.com/7bJFeYCeQSgAAAAM/fat-bouncy-funny.gif"
)

# Define UI for application
ui <- fluidPage(

  
  add_busy_gif(src = sample(gifs, 1), height = 70, width = 70),

    # Application title
    titlePanel("Heat map maker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          # Input: Select a file ----
          fileInput("upload", "Choose CSV File",
                    multiple = FALSE,
                    accept = c(".csv")),
          
          # Input: Select date range
          dateRangeInput('dateRange',
                         label = "Time range",
                         start = Sys.Date() - months(1), end = Sys.Date()
          ),
          # Input: Colours
          selectInput('colourScheme', choices = c("magma", "inferno", "plasma",
                                                "viridis", "cividis", "rocket",
                                                "mako", "turbo"), 
                      label = "Colour scheme",
                      selected = "magma", multiple = FALSE
        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel( type = "tabs",
                       tabPanel(title = "Heat map", 
                                plotOutput("density_plot", width = "110%", height = "1000px")),
                       tabPanel(title = "Data", 
                                DTOutput("data_preview")),
                       tabPanel(title = "Plot 01", 
                                plotOutput("test1", width = "100%")
                                )
          )
        )
    )
)

# Define server logic required
server <- function(input, output, session) {
  
  ## Load the functions
  source(here("scripts", "00_LoadData.R"), local = TRUE)
  source(here("scripts", "01_DataCleaning.R"), local = TRUE)
  source(here("scripts", "02_CalculateDensities.R"), local = TRUE)
  source(here("scripts", "02A_GetDensitiesFun.R"), local = TRUE)
  source(here("scripts", "03_CreateMaps.R"), local = TRUE)
  
  ## Read in the uploaded data
  data <- reactive({
    req(input$upload)
 
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = data.table::fread(input$upload$datapath),
           validate("Invalid file; Please upload a .csv")
    ) %>% 
      load_data_fun() %>% 
      data_cleaning()
  })

  ## Create data table
  output$data_preview <- DT::renderDT({
    data()
  })
  
  ## Load region shapefile
  ownr_utm <- read_sf(
    here("data", "background", "OWNRBufferzone.shp")
  ) %>%
    st_transform(crs = 32736)
  
  ## Create buffer around area
  ownr_buffer <- ownr_utm %>% 
    st_union() %>% 
    st_buffer(dist = 2000)
  
  ## Set window for density estimation
  ownr_window <- as.owin(ownr_buffer)
  
  ## Density estimation
  density_data <- reactive({
    req(data())
    
    sightings_split <- data() %>%
      calculate_densities()
    
    all_densities <-
      map(sightings_split, .f = get_densities)
  })
  ## Density estimation
  highest_value <- reactive({
    req(density_data())
    
    highest_value <- density_data() %>%
      map(as.data.frame) %>%
      bind_rows() %>%
      filter(!is.na(v_km)) %>% 
      pull(v_km) %>% 
      max()
  })
  
  density_maps <- reactive({
    req(density_data())
    
    density_maps <- map2(density_data(), names(density_data()), make_maps)
  })
    
  output$density_plot <- renderPlot({
    
    ## Create the maps
    wrap_plots(density_maps()) +
      plot_layout(guides = "auto", heights = c(2,2), ncol = 1)
  }, execOnResize = TRUE, bg = "transparent", height = "auto")
  
  output$test1 <- renderPlot({
    density_maps()[1]
  }, execOnResize = TRUE, bg = "transparent"
)
}

# Run the application 
shinyApp(ui = ui, server = server)
