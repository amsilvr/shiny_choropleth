#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



require(shiny)
require(tidyverse)
require(lubridate)
require(stringr)
require(sf)
require(leaflet)
require(albersusa)

source('CMAS_Clean_shiny.R', echo = TRUE)


alert_tally <- mutate(alert_tally
                      , Total = AMBER + FlashFlood
                              + Other + Tornado
                              + Tsunami)
# EPSG Definition from
# setView( 39.50 , -98.35, zoom = 5) %>%
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=38.393632 +lon_0=-101.8458245 +x_0=0 +y_0=0
              +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))

state_sf <- usa_sf()
counties_sf <- counties_sf()

bins <- c(0, 1, 5, 10, 15, 20, 25, 30, 300)
pal <- colorBin("Blues", domain = NULL, bins = bins, pretty = TRUE)

# Define UI for application that draws a histogram
ui <- bootstrapPage(

   # Application title
  tags$style(type="text/css", "html, body {width:100%;height:100%;text-align:center;}"),

  h1("WARN Alerts by County: mouse over map for more info"),

  # Show choropleth of selected alerts
  leafletOutput("map", width = "100%", height = "100%"),

   # Sidebar with a radio buttons input for types of alerts
   absolutePanel(top = 10, right = 10, draggable = TRUE,
        selectInput(inputId = "alertType", label = "Which Alert Type?"
                     ,choices = c("Total"
                                  ,"AMBER"
                                  ,"FlashFlood"
                                  ,"Tornado"
                                  ,"Tsunami"
                                  ,"Other")
              )
      )



)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  type <- reactive(renderText(input$alertType))
  # map data
  fd <- reactive({
    inner_join(counties_sf
              ,select(alert_tally
                    , inst = matches(input$alertType))
                    )
        })

output$map <- renderLeaflet({
    leaflet(data = fd()
            , options = leafletOptions(crs = epsg2163)) %>%


      addPolygons(stroke = FALSE
                , label = ~paste0(name
                                  ," "
                                  ,lsad
                                  ,", "
                                  ,iso_3166_2
                                  ,": "
                                  ,inst)
                , labelOptions = labelOptions(style = list(
                        "color" = "steelblue",
                        "font-style" = "bold",
                        "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                        "font-size" = "15px",
                        "border-color" = "rgba(0,0,0,0.5)"))
                , fillOpacity = .5
                , smoothFactor = 1
                , fillColor = ~pal(inst)
                , highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 1,
                      bringToFront = FALSE)
                ) %>%
            addPolygons(data = state_sf
                        ,stroke = TRUE
                        , weight = 2
                        , opacity = 1
                        , color = "grey"
                        , fill = FALSE
            )
    })

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

