# Green Infrastructure
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)

# Data Source: https://data.cityofnewyork.us/Environment/DEP-Green-Infrastructure/spjh-pz7h
greenInf.load <- readOGR("https://data.cityofnewyork.us/api/geospatial/spjh-pz7h?method=export&format=GeoJSON")
greenInf.load@data <- cbind(greenInf.load@data, coordinates(greenInf.load))
names(greenInf.load@data)[c(30,31)] <- c("longitude", "latitude")
boros.load <- readOGR("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON")
# Add Boro centroids to dataframe
boros.load@data <- cbind(boros.load@data, rgeos::gCentroid(boros.load, byid = TRUE)@coords)

icons <- awesomeIconList(
  MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
  Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
  `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
  `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
)

# Define UI for application
ui <- navbarPage("NYC Green Infrastructure",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Select Sewer Type
                              selectInput("sewerSelect",
                                          "Sewer Type",
                                          levels(greenInf.load$sewer_type),
                                          selected = c("MS4", "Non-combined"),
                                          selectize = T,
                                          multiple = T),
                              # Select NYC Borough
                              radioButtons("boroSelect",
                                           "Borough Filter:",
                                           choices = levels(greenInf.load$borough),
                                           selected = "Bronx"),
                              # Number of projects
                              textOutput("text"),
                              tags$br(),
                              # Remove a Project
                              disabled(actionButton("delete", "Remove Project", icon = icon("times"))),
                              # Select a Project
                              tags$br(), tags$br(),
                              # Restore projects
                              disabled(actionButton("restore", "Restore removed Projects", icon = icon("refresh")))
                            ),
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              )
                            )
                          ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
   values <- reactiveValues(removed = c())
   # Basic Map
   output$leaflet <- renderLeaflet({
     leaflet() %>%
       addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
       addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
       setView(-74.0060, 40.7128, 9) %>%
       addLayersControl(baseGroups = c("Google", "Wiki"))
   })
   # Green Infrastructure Filtered data
   greenInfInputs <- reactive({
     greenInf <- greenInf.load 
     
     req(input$boroSelect)
     # Boros
     greenInf <- subset(greenInf, borough == input$boroSelect)
     
     # Sewer type
     if (length(input$sewerSelect) > 0) {
       greenInf <- subset(greenInf, sewer_type %in% input$sewerSelect)
     }
      # Filter out removed projects
      if (length(values$removed) > 0) {
       greenInf <- subset(greenInf, !(asset_id %in% values$removed))
     }
      
     return(greenInf)
   })
   
   # Replace layer with filtered greenInfrastructure
   observe({
     greenInf <- greenInfInputs()
     # Data is greenInf
     leafletProxy("leaflet", data = greenInf) %>%
       # In this case either lines 92 or 93 will work
       # clearMarkers() %>%
       clearGroup(group = "greenInf") %>%
       addAwesomeMarkers(icon = ~icons[sewer_type], popup = ~paste0("<b>", project_na, "</b>: ", sewer_type), group = "greenInf", layerId = ~asset_id)
   })
   # Borough Filter
   boroInputs <- reactive({
     boros <- subset(boros.load, boro_name == input$boroSelect)

     return(boros)
   })
   observe({
     boros <- boroInputs()
     
     leafletProxy("leaflet", data = boros) %>%
       # In this case either lines 107 or 108 will work
       # clearShapes() %>%
       clearGroup(group = "boros") %>%
       addPolygons(popup = ~paste0("<b>", boro_name, "</b>"), group = "boros", layerId = ~boro_code, fill = FALSE, color = "green") %>%
       setView(lng = boros$x[1], lat = boros$y[1], zoom = 9)
   })
   output$table <- DT::renderDataTable(greenInfInputs()@data, options = list(scrollX = T))
   # Enable button once a marker has been selected
   observeEvent(input$leaflet_marker_click$id, {
     enable("delete")
   })
   # Add layerID to list of removed projects
   observeEvent(input$delete, {
     enable("restore")
     isolate({
       values$removed <- c(values$removed, input$leaflet_marker_click$id)
     })
   })
   # Reset removed Projects
   observeEvent(input$restore, {
     values$removed <- c()
     disable("restore")
   })
   # Subset to data Only on screen
   onScreen <- reactive({
      req(input$leaflet_bounds)
      bounds <- input$leaflet_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(greenInfInputs()@data, latitude >= latRng[1] & latitude <= latRng[2] & longitude >= lngRng[1] & longitude <= lngRng[2])
   })
   # Print Projects
   output$text <- renderText({
      paste("You are viewing", nrow(onScreen()), "projects")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

