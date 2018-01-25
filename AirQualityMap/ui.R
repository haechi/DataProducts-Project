library(shiny)
library(leaflet)

bootstrapPage(
  
  # Filling the windows with the map
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),

  # Adding controls
  absolutePanel(top=10, right=10,
    h2("Air Quality Map"),
    selectInput("location", "Location Selection",
                c("IP Based","Beijing","Shanghai","Hong Kong","Seoul","Tokyo")),
    checkboxInput("legend", "Show legend", TRUE),
    checkboxInput("help", "Show help", FALSE),
    
    # Help display
    conditionalPanel(condition = "input.help", 
      textAreaInput("helptext", "Short Desription", value = includeText("help.txt"), 
                                   cols = 3, rows = 8)),
    
    # Data source
    hr(),
    helpText("Data from ",a(href="http://aqicn.org/", "World Air Quality Index")),
    style = "opacity: 0.8; background: #ffffff;")
)


