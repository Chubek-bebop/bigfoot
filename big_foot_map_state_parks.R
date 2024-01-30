#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(sf)
library(sp)

#df <- read.csv('bfro_reports_geocoded.csv')

# Good God keep this line of code safe because it fixes everything '%>% sf::st_transform('+proj=longlat +datum=WGS84')
shp_parks <- read_sf('shapefiles\\stateparks\\nps_boundary.shp') %>%
  sf::st_transform('+proj=longlat +datum=WGS84')
shp_states <- read_sf('shapefiles\\states\\cb_2022_us_state_500k.shp') %>%
  sf::st_transform('+proj=longlat +datum=WGS84')


map <- shp_parks %>%
  leaflet() %>%
  addProviderTiles("USGS.USImagery")%>%
  addPolygons(
    data = shp_parks,
    weight = 2,
    opacity = 1,
    color = "red",
    dashArray = "3",
    fillOpacity = 0,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.5),
    label = ~paste("Park Name","<b>",UNIT_NAME,"</b>") %>% lapply(htmltools::HTML),
    group = "Parks")#%>%
  
  # addPolygons(
  #   data = shp_states,
  #   weight = 2,
  #   opacity = 1,
  #   color = "blue",
  #   dashArray = "3",
  #   fillOpacity = 0,
  #   highlightOptions = highlightOptions(
  #     weight = 5,
  #     color = "#999",
  #     dashArray = "",
  #     fillOpacity = 0.5),
  #   label = ~paste("State","<b>",shp_states$NAME,"</b> <br>") %>% lapply(htmltools::HTML),
  #   group = "States")

#Read the data in as a data.frame (df)
df <- read.csv('bfro_reports_geocoded.csv')

map <- map %>% addCircleMarkers(
  data = df
  ,~longitude
  ,~latitude,
  popup = ~title,
  color = "red",
  group = "Sightings",
  stroke = 0)

map <- map %>% addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri")

# By adding the layerControl after the initial map setup, we get the generation of the overlayGroups to match the actual groups created above.
map <- map %>% addLayersControl(
  baseGroups = c("USGS.USImagery", "Carto", "Esri"),
  overlayGroups = c("Parks","Sightings"),#, "States"),
  options = layersControlOptions(collapsed = FALSE)) 


map
saveWidget(map, file="bigfoot_map_by_state_park.html")