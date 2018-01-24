library(shiny)
library(leaflet)

loc.list <- c("IP Based Location","Shanghai","Beijng","Seoul","Tokyo")

# Define UI for application that draws a histogram
bootstrapPage(

  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(top=10, right=10,
    h2("Air Quality Map"),
    selectInput("location", "Location Selection",
                c("IP Based","Beijing","Shanghai","Hong Kong","Seoul","Tokyo")),
    checkboxInput("legend", "Show legend", TRUE),
    hr(),
    helpText("Data from ",a(href="http://aqicn.org/", "World Air Quality Index")),
    style = "opacity: 0.8; background: #ffffff;"
  )
)


