# Project: Developing Data Products Course

The goal of this project was to develop a dynamic shiny application displaying the current Air Quality Index (AQI) for the current location. The index scale is not linear. Instruction of how to calculate the index can be found <a href='https://en.wikipedia.org/wiki/Air_quality_index#Computing_the_AQI'>here</a>. The AQI divides the air pollution into six levels

1. Good 
2. Moderate
3. Unhealthy for Sensitive Groups
4. Unhealthy
5. Very Unhealthy
6. Hazardous

Project links:
* <a href='https://haechi.github.io/DataProducts-Project'>Github Page</a>
* <a href='http://haechi.shinyapps.io/air_quality_map'>Shiny Application</a>

## Functionality

The Application will determine the visitor's current IP location and set a rectangle boundary around the approximate location. The sensor station data for station within the boundary is then pull as <a href='https://json.org'>JSON</a> object from the <a href='http://aqicn.org/'>World Air Quality Index</a> API. 

The sensor station data is then overlaid on the map. The marker color is changed accordingly  to the AQI level.

Additionally, the user has the following menu options
* Change the location by selecting a city from the drop-down list
* Displaying a short help text
* Hiding the legend

## User Interface

The shiny application on displays the map with the overlay data. This can be done by using a `bootstrapPage` layout and rendering the leaflet map with the following options

```
tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
leafletOutput("map", width = "100%", height = "100%"),
```
The controls were implemented inside an `absolutePanel` using the following controls
```
selectInput("location", "Location Selection",
  c("IP Based","Beijing","Shanghai","Hong Kong","Seoul","Tokyo")),
checkboxInput("legend", "Show legend", TRUE),
checkboxInput("help", "Show help", FALSE),
```

## Server 

For the data processing function the server will first assemble a formatted URL directed to the World Air Quality Index <a href='http://aqicn.org/api/'>API</a>. The API token was generated before on the API homepage. 
```
token <- "3e0dd90175924cf5dd95296e75f93e13f9dc1cb3"
id.loc <- "here"
url.loc <- paste("http://api.waqi.info/feed/",id.loc,"/?token=",token,sep="")
json.loc <- as.data.frame(fromJSON(url.loc))
```
The geo location obtained from this query is then used to again to obtain measurement stations in close proximity. The stations AQI level is determined and the resulting `dataframe` is passed to `leavelet` for rendering.
```
aqi.map %>% leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerLite, 
                   options = providerTileOptions(noWrap = TRUE)) %>% ...
```
