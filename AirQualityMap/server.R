library(shiny)
library(jsonlite)
library(leaflet)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  observe({
    proxy <- leafletProxy("map")
    
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend("bottomright", 
        colors= c("darkgreen", "gold","darkorange","red","purple","saddlebrown"), 
        labels = c("0-50 Good", "51-100 Moderate","101-150 Unhealthy for Sensitive Groups",
                   "151-200 Unhealthy","201-300 Very Unhealthy","300+ Hazardous"), 
        title="Air Quality Level")
    }
  })
   
  output$map <- renderLeaflet({
    # World Air Quality Index
    # API Homepage: http://aqicn.org/api/
    
    token <- "3e0dd90175924cf5dd95296e75f93e13f9dc1cb3"
    name.loc <- input$location
    
    if ((is.null(name.loc)) || (identical(name.loc,"IP Based"))) id.loc <- "here"
    if (identical(name.loc,"Beijing")) id.loc <- "@3303"
    if (identical(name.loc,"Shanghai")) id.loc <- "@487"
    if (identical(name.loc,"Hong Kong")) id.loc <- "@3308"
    if (identical(name.loc,"Seoul")) id.loc <- "@5508"
    if (identical(name.loc,"Tokyo")) id.loc <- "@2290"
    
    url.loc <- paste("http://api.waqi.info/feed/",id.loc,"/?token=",token,sep="")
    json.loc <- as.data.frame(fromJSON(url.loc))
    
    if (json.loc$status[1] != "ok") {
      # Switch to Beijing as default
      loc <- "@885"
      url.loc <- paste("http://api.waqi.info/feed/",loc,"/?token=",token,sep="")
      json.loc <- as.data.frame(fromJSON(url.loc))
    }
    
    delta.lat <- 0.15
    delta.long <- 0.3
    
    url.map <- paste("http://api.waqi.info/map/bounds/?latlng=",
                 as.numeric(json.loc$data.city.geo[1])-delta.lat/2,",",
                 as.numeric(json.loc$data.city.geo[2])-delta.long/2,",",
                 as.numeric(json.loc$data.city.geo[1])+delta.lat/2,",",
                 as.numeric(json.loc$data.city.geo[2])+delta.long/2,"&",
                 "token=",token,sep="")
    
    if (length(fromJSON(url.map)$data) == 0) {
      
      # Hack that switches the latitude and longitude respose
      url.map <- paste("http://api.waqi.info/map/bounds/?latlng=",
                   as.numeric(json.loc$data.city.geo[2])-delta.lat/2,",",
                   as.numeric(json.loc$data.city.geo[1])-delta.long/2,",",
                   as.numeric(json.loc$data.city.geo[2])+delta.lat/2,",",
                   as.numeric(json.loc$data.city.geo[1])+delta.long/2,"&",
                   "token=",token,sep="")
    }
    
    
    json.map <- as.data.frame(fromJSON(url.map))

    aqi.map <- data.frame(idx = as.numeric(json.map$data.aqi),
                          lat = as.numeric(json.map$data.lat),
                          lng = as.numeric(json.map$data.lon),
                          status = as.character(json.map$status),
                          apl = sapply(1:nrow(json.map), 
                            function(x) {
                              if (as.numeric(json.map$data.aqi[x]) <= 50 ) {
                                "L1"
                              }  else if (as.numeric(json.map$data.aqi[x]) <= 100 ) {
                                "L2"
                              } else if (as.numeric(json.map$data.aqi[x]) <= 150 ) {
                                "L3"
                              } else if (as.numeric(json.map$data.aqi[x]) <= 200 ) {
                                "L4"
                              } else if (as.numeric(json.map$data.aqi[x]) <= 300 ) {
                                "L5"
                              } else {
                                "L6"
                              }
                            }))

    pal <- colorFactor(c("darkgreen", "gold","darkorange","red","purple","saddlebrown"), 
                       domain = c("L1", "L2","L3","L4","L5","L6"))
    
    aqi.map %>% leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLite, 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(lng = aqi.map$lng, lat=aqi.map$lat, 
                       label = as.character(aqi.map$idx), color = ~pal(aqi.map$apl), 
                       labelOptions = lapply(1:nrow(aqi.map), 
                          function(x) { 
                              labelOptions(opacity=0.7, noHide = T, direction = 'auto', 
                              textsize='11px', offset=c(20,-15))
                          })) 
  })
  
})
