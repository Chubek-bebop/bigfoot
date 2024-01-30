#big_foot_map.R by Bart Czubak

#Load in an libraries.
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(mapview)



df <- read.csv('bfro_reports_geocoded.csv')
df <- 


map <- df%>%
leaflet() %>%
  addProviderTiles("USGS.USImagery") %>% 
  setView(lat = 35, lng = -95, zoom = 4) %>%
  addHeatmap(
    lng= na.omit(df$longitude),lat = na.omit(df$latitude), max = .6,radius = 20, intensity = .05, gradient = "Spectral")


map <- map %>%
  addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri") %>%
  addLayersControl(baseGroups = c("USGS.USImagery", "Carto", "Esri"))

saveWidget(map, file="bigfoot_map_sightings_heatmap.html")
