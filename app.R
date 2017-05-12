#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(stringr)
library(sf)
library(leaflet)

# Download Shapefiles
# 
if (!exists("alert_tally")) {
  source("CMAS_Clean_shiny.R", echo = TRUE)
}
#countyshapes_url <- "https://www2.census.gov/geo/tiger/TIGER2016/COUNTY/tl_2016_us_county.zip"
countyshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
stateshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
if (!dir.exists("data")) {dir.create("data")}
if (!file.exists("data/county_shape_file.zip")) {
  download.file(countyshapes_url
                , destfile = "data/county_shape_file.zip")}

if (!file.exists("data/state_shape_file.zip")) {
  download.file(stateshapes_url
                , destfile = "data/state_shape_file.zip")}
  
  c_shp <- unzip("data/county_shape_file.zip", exdir = "data")
  s_shp <- unzip("data/state_shape_file.zip", exdir = "data")

# Read the file with sf and add the proper crs code for this projection

counties_sf <- read_sf(c_shp[5]) %>%
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
ui <- bootstrapPage(
  theme = shinytheme('darkly'),

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
  leafletOutput("map", width = "100%", height = "85%"),

   # Dropdown menu
   absolutePanel( top = "20px"
                 , left = "10%"
                 , width = "20%"
                 , draggable = TRUE
        ,selectInput(inputId = "alertType", label = "Which Alert Type?"
                     ,choices = c("Total"
                                  ,"AMBER"
                                  ,"FlashFlood"
                                  ,"Tornado"
                                  ,"Tsunami"
                                  ,"Other")
              )
      ),
  absolutePanel( top = "20px"
                 , right = "10%"
                 , width = "20%"
                 , draggable = TRUE
                 , tableOutput("events")
                 )

)

allCounties <- left_join(counties_sf, alert_tally)
allCounties[is.na(allCounties)] <- 0


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  type <- reactive(renderText(input$alertType))
  # map data
  fd <- reactive({
    allCounties %>%
              mutate_(inst = input$alertType)
        })
  v <- reactiveValues(msg = '')
  
output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    setView(lng = -93.85, lat = 37.45, zoom = 5) 
})
  
observeEvent(input$alertType, {
    leafletProxy('map') %>%
    clearShapes() %>%
    addPolygons(data = fd()
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

 observeEvent(input$map1_bounds, {
   proxy <- leafletProxy("map") %>%
     setView(input$map1_bounds)
 })
 
 observeEvent(input$alertType, { # redraw legend
      proxy <- leafletProxy("map", data = fd()) %>%
        clearControls() %>%
        addLegend(pal = pal
                , values = ~inst
                , opacity = .5
                , title = paste0("Number of ",input$alertType," WEAs")
                , position = "topleft")
    })

 #Create Event List for County
 observeEvent(input$map1_shape_click, {
   v$eventList <- filter_(fips_msg, GEOID == input$map_shape_click$GEOID) %>%
     left_join(msg2) %>%
     select(rec_time, wea, type)
 })
 
 output$events <- renderTable(v$eventList)
 
 }

# Run the application
shinyApp(ui = ui, server = server)

