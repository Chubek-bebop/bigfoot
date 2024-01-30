#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)

df <- read.csv('bfro_reports_geocoded.csv')

#Define color palette for the map to use based on seasons.
pal_types <- colorFactor(
  palette = c('red', 'green', 'blue'),levels = c("Class A", "Class B", "Class C"))

#Plot the data using leaflet by piping in the data.frame
map <- df%>%
  leaflet() %>%
  #Set provider tiles (i.e. the way the map looks)
  addProviderTiles("USGS.USImagery") %>% 
  #Manual set the view to the center of the US (roughly)
  setView(lat = 35, lng = -95, zoom = 4) %>%
  #Add a search option if a specific location is needed to be found.
  addSearchOSM()%>%
  addResetMapButton

#Split the data based on the season. 
classifications = levels(factor(df$classification))
#Plot each of the observations by season.
for (c in classifications) {
  map <- map %>%
    #Add circle markers with popups per season.
    addCircleMarkers(data = filter(df,classification == c),~longitude, color = ~pal_types(classification), ~latitude,popup = ~title, group = c)
}

map <- map %>% addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri")

# By adding the layerControl after the initial map setup, we get the generation of the overlayGroups to match the actual groups created above.
map <- map %>% addLayersControl(baseGroups = c("USGS.USImagery", "Carto", "Esri"),overlayGroups = classifications, options = layersControlOptions(collapsed = FALSE)) 
map
saveWidget(map, file="bigfoot_map_types.html")