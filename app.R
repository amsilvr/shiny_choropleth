#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



require(shiny)
require(shinythemes)
require(tidyverse)
require(lubridate)
require(stringr)
require(sf)
require(leaflet)

# Download Shapefiles
# 
if (!exists("alert_tally")) {
  source("CMAS_Clean_shiny.R", echo = TRUE)
}

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

states_sf <- read_sf(s_shp[4]) %>% 
  st_transform('+proj=longlat +datum=WGS84')


bins <- c(0, 1, 3, 5, 10, 20, 30, 40, 80, 205)
pal <- colorBin("Reds", domain = NULL, bins = bins, pretty = TRUE)


# Define UI for application that draws a histogram
ui <- bootstrapPage(
  theme = shinytheme('superhero'),

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
                                  ,"FlashFlood"
                                  ,"Tornado"
                                  ,"Tsunami"
                                  ,"Other")
              )
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


output$map <- renderLeaflet({
    leaflet(data = fd()) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 5) %>%
    
      addPolygons(data = states_sf
                , stroke = TRUE
                , weight = 2
                , opacity = 1
                , color = "grey"
                , fill = FALSE
    ) %>%
    addPolygons(data = fd()
                #, layerId = input$alertType
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
                      "color" = "steelblue",
                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                      "text-align" = "left",
                      "font-size" = "15px",
                      "border-color" = "rgba(0,0,0,0.5)"))
                , fillOpacity = .5
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
  
  # A reactive expression that returns the set of counties that are
  # in bounds right now - adapted from https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
  # # countiesInBounds <- reactive({
  # #   if (is.null(input$map_bounds))
  # #     return(allCounties[FALSE,])
  # #   bounds <- input$map_bounds
  # #   latRng <- range(bounds$north, bounds$south)
  # #   lngRng <- range(bounds$east, bounds$west)
  # #   
  # #   subset(allCounties,
  # #          latitude >= latRng[1] & latitude <= latRng[2] &
  # #            longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  
    

 
 # observe({# new polygon layer
 #   proxy <- leafletProxy("map") 
 #   })

 observe({ # redraw legend
      proxy <- leafletProxy("map", data = fd()) %>%
        clearControls() %>%
        addLegend(pal = pal
                , values = ~inst
                , opacity = .5
                , title = paste0("Number of ",input$alertType," WEAs")
                , position = "topleft")
    })

 }

# Run the application
shinyApp(ui = ui, server = server)

