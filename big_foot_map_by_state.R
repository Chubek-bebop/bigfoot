#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(sf)
library(sp)


df <- read.csv('bfro_reports_geocoded.csv')

shp <- read_sf('shapefiles\\s_08mr23.shp') %>% sf::st_transform('+proj=longlat +datum=WGS84')
#shp <- raster::getData("GADM", country = "United States", level = 1)

total_per_state <- df %>% count(state)
shp@data <- shp@data %>% left_join(total_per_state, by = c("NAME_1" = "state"))

pal <- colorNumeric(
  palette = "BuGn",
  domain = shp@data$n)


#Plot the data using leaflet by piping in the data.frame
map <- shp %>%
  leaflet() %>%
  #Set provider tiles (i.e. the way the map looks)
  addProviderTiles("USGS.USImagery") %>% 
  #Manual set the view to the center of the US (roughly)
  setView(lat = 35, lng = -95, zoom = 4) %>%
  #Add a search option if a specific location is needed to be found.
  addSearchOSM()%>%
  addResetMapButton %>%
  addPolygons(
    data = shp,
    weight = 2,
    opacity = 1,
    color = ~pal(n),
    dashArray = "3",
    fillOpacity = 0,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.5,
      bringToFront = TRUE),
    label = ~paste("State","<b>",state.name,"</b> <br>", "Total sightings: ", "<b>",n,"</b> <br>") %>% lapply(htmltools::HTML))

map <- map %>% addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri")

# By adding the layerControl after the initial map setup, we get the generation of the overlayGroups to match the actual groups created above.
map <- map %>% addLayersControl(baseGroups = c("USGS.USImagery", "Carto", "Esri"),options = layersControlOptions(collapsed = FALSE)) 
map
#saveWidget(map, file="bigfoot_map_by_state.html")