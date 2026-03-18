# Load packages
library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load the GeoPackage file once at startup
engwales_sf <- st_read("EW_bngsimple.gpkg", quiet = TRUE)

# UI
ui <- fluidPage(
  titlePanel("Cat Densities by County"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_county", "Select a County:",
                  choices = sort(unique(engwales_sf$County_max)),
                  selected = NULL)
    ),
    mainPanel(
      leafletOutput("map", height = "600px"),
      textOutput("area_info")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter to selected county
  filtered_data <- reactive({
    req(input$selected_county)
    engwales_sf %>% filter(County_max == input$selected_county)
  })
  
  # Render the map
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    # Handle edge case where all densities might be NA
    if (all(is.na(data$Densities_Total))) {
      pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 1), na.color = "lightgrey")
    } else {
      pal <- colorNumeric(palette = "YlOrRd", domain = data$Densities_Total, na.color = "lightgrey")
    }
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        layerId = ~SiteCode,  # make sure this column exists
        fillColor = ~pal(Densities_Total),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0(SiteName, ": ", Densities_Total),
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = data$Densities_Total,
        title = "Density",
        opacity = 0.7,
        na.label = "No data"
      )
  })
  
  # Show clicked area info
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click)) {
      clicked_row <- filtered_data() %>% filter(SiteCode == click$id)
      output$area_info <- renderText({
        paste0("Local Area: ", clicked_row$SiteName,
               "\nDensity: ", ifelse(is.na(clicked_row$Densities_Total), "Unknown", clicked_row$Densities_Total))
      })
    }
  })
}

# Run the app
shinyApp(ui, server)
