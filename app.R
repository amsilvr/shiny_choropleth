# Setup

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(stringr)
library(sf)
library(leaflet)

# Download Shapefiles
 
if (!exists("alert_tally")) {
  source("CMAS_Clean_shiny.R", echo = TRUE)
}
countyshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
if (!dir.exists("data")) {dir.create("data")}
if (!file.exists("data/county_shape_file.zip")) {
  download.file(countyshapes_url
                , destfile = "data/county_shape_file.zip")}

  c_shp <- unzip("data/county_shape_file.zip", exdir = "data")

# Read the file with sf and add the proper crs code for this projection

counties_sf <- read_sf(c_shp[grep("shp$", c_shp)]) %>% #pulls the shp file from the zip
  left_join(state_iso) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  inner_join(lsad_lookup())
counties_sf$NAME <- str_replace_all(counties_sf$NAME, pattern = "Ã±",replacement = "ñ") %>%
  str_replace_all("Ã¡",replacement = "á") %>%
  str_replace_all("Ã¼",replacement = "ñ") %>%
  str_replace_all("Ã³",replacement = "ó") %>%
  str_replace_all("Ã",replacement = "í")

bins <- c(0, 1, 3, 5, 10, 20, 30, 40, 80, 205)
pal <- colorBin("YlOrRd", domain = NULL, bins = bins, pretty = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = shinytheme('darkly'),

   # Application title
  tags$style(type = "text/css",
          "html,
             body {width:100%;height:100%;text-align:center;} 
             .selectize-input { font-size: 32px; background-color:aqua; }
             .selectize-dropdown { font-size: 25px; line-height: 30px; }
             .control-label { font-size: 32px; color: black!important;} 
             .shiny-input-container {background-color:white;}
             .content::-webkit-scrollbar {display: none;"),

  h1("WARN Alerts by County"),
  h3("Mouse over map for more info"),

  # Show choropleth of selected alerts

  leafletOutput("map", width = "100%", height = "100%"),

   # Dropdown menu
   absolutePanel( top = "20px"
                 , left = "10%"
                 , width = "20%"
                 , draggable = TRUE
        ,selectInput(inputId = "alertType", label = "Which Alert Type?"
                     ,choices = c("Total"
                                  ,"AMBER"
                                  ,"Flash Flood" = "FlashFlood"
                                  ,"Tornado"
                                  ,"Tsunami"
                                  ,"Other")
              )
      )

)

allCounties <- left_join(counties_sf, alert_tally)
allCounties[is.na(allCounties)] <- 0


# Define server logic required to draw a choropleth

server <- function(input, output, session) {
  type <- reactive(renderText(input$alertType))
<<<<<<< HEAD
# Reactive variable fd containing (f)iltered (d)ata
fd <- reactive({
  allCounties %>%
            mutate_(inst = input$alertType)
      })
# Reactive variable containing click_data
  click_data <- reactiveValues(clickedMarker = NULL)
  
output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
<<<<<<< HEAD
    setView(lng = -93.85, lat = 37.45, zoom = 4) #%>%
=======
    setView(lng = -93.85, lat = 37.45, zoom = 5) #%>%
>>>>>>> ce9a78b976b077c7b70a008f2ce13b27938a77ce
# Layer Controls
    # addLayersControl(
    #   alertTypeGroups = c("Total"
    #                       ,"AMBER"
    #                       ,"FlashFlood"
    #                       ,"Tornado"
    #                       ,"Tsunami"
    #                       ,"Other")
    #   , options = layersControlOptions(collapsed = FALSE)
    # )
})
  
observeEvent(input$alertType, {
    leafletProxy('map') %>%
    clearShapes() %>%
    addPolygons(data = fd()
<<<<<<< HEAD
                , group = input$alertType
=======
>>>>>>> ce9a78b976b077c7b70a008f2ce13b27938a77ce
                , layerId = ~GEOID
                , stroke = FALSE
                , label = ~paste0("<strong>"
                                  ,NAME
                                  ," "
                                  ,description #lookup table for lsad
                                  ,", "
                                  ,iso_3166_2
                                  ,":</strong><br />"
                                  ,inst
                                  ," "
                                  ,input$alertType
                                  ," WEA Messages") %>% 
                  lapply(htmltools::HTML)
                , labelOptions = labelOptions(style = list(
=======

  # Reactive variable fd containing (f)iltered (d)ata
  fd <- reactive({
    allCounties %>%
      mutate_(inst = input$alertType)
  })
  # Reactive variable containing click_data
  click_data <- reactiveValues(clickedMarker = NULL)
  
  # Base Map 
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 5)
  })
    
    observeEvent(input$alertType, {
      leafletProxy('map') %>%
        clearShapes() %>%
        addPolygons(data = fd()
                    , group = input$alertType
                    , layerId = ~GEOID
                    , stroke = FALSE
                    , label = ~paste0("<strong>"
                                      ,NAME
                                      ," "
                                      ,description #lookup table for lsad
                                      ,", "
                                      ,iso_3166_2
                                      ,":</strong><br />"
                                      ,inst
                                      ," "
                                      ,input$alertType
                                      ," WEA Messages") %>% 
                      lapply(htmltools::HTML)
                    , labelOptions = labelOptions(style = list(


>>>>>>> d7b00a9a51b15d679c16563c0c5621789d7b3a72
                      "color" = "#2b3e50",
                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                      "text-align" = "left",
                      "font-size" = "17px",
                      "border-color" = "rgba(0,0,0,0.5)"))

                    , fillOpacity = .6
                    , smoothFactor = .5
                    , fillColor = ~pal(inst)
                    , highlight = highlightOptions(


                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 1,
                      bringToFront = FALSE)
        )

    })
    # Store the Map Boundaries on screen
    observeEvent(input$map1_bounds, {
      proxy <- leafletProxy("map") %>%
        setView(input$map1_bounds)
    })
    
    # Re-title the legend
    observeEvent(input$alertType, { 

      proxy <- leafletProxy("map", data = fd()) %>%
        clearControls() %>%
        addLegend(pal = pal
                  , values = ~inst
                  , opacity = .5
                  , title = paste0("Number of ",input$alertType," WEAs")
                  , position = "topleft")
    })

 # store the clicked county
 observeEvent(input$map_shape_click, {
   click_data$clickedShape <- input$map_shape_click
 })
 
 #Create a table with all the events in that geoid
   output$events <- renderTable({
     county_events = click_data$clickedShape$id %>%
     print()
     filter(fips_msg, GEOID == county_events) %>%
       left_join(msg2) %>%
       filter(type == input$alertType) %>%
       transmute(`Alert Received` = 
                   paste(month(rec_time,abbr = TRUE)
                         , day(rec_time)
                         , year(rec_time) )
                         , wea)
     }
     , striped = TRUE
     , hover = TRUE
     , bordered = TRUE)
 
 }

# Run the application
shinyApp(ui = ui, server = server)

