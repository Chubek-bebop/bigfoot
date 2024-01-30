#big_foot_map.R by Bart Czubak

#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)



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

#Read the data in as a data.frame (df)
df <- read.csv('bfro_reports_geocoded.csv')

#Define color palette for the map to use based on seasons.
pal_seasons <- colorFactor(
  palette = c('brown', 'blue', 'green', 'red','black'),levels = c("Fall", "Winter", "Spring","Summer","Unknown"))

#Split the data based on the season. 
seasons = levels(factor(df$season))
#Plot each of the observations by season.
for (s in seasons) {
  map <- map %>%
    #Add circle markers with popups per season.
    addCircleMarkers(data = filter(df,season == s),~longitude,~latitude,popup = ~title,color = ~pal_seasons(season), group = s)
}

map <- map %>% addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri")

# By adding the layerControl after the initial map setup, we get the generation of the overlayGroups to match the actual groups created above.
map <- map %>% addLayersControl(baseGroups = c("USGS.USImagery", "Carto", "Esri"),overlayGroups = seasons, options = layersControlOptions(collapsed = FALSE)) 
map  


#'Print' the map
map
#Save map as a HTML file.
saveWidget(map, file="bigfoot_map_seasons.html")
