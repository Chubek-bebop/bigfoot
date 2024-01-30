#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)

df <- read.csv('bfro_reports_geocoded.csv')

#Extract the year from the date.
year_df <- df %>%
  mutate(year = as.numeric(format(as.Date(df$date, format = "%Y-%m-%d"), "%Y")))

#Get decades.
decade_df <- year_df %>%
  mutate(decade  = year - year %% 10)

#Define color palette.
pal_decades <- colorNumeric(
  palette = "Reds",
  domain = decade_df$decade)

#Plot the data using leaflet by piping in the data.frame
map <- decade_df%>%
  leaflet() %>%
  #Set provider tiles (i.e. the way the map looks)
  addProviderTiles("USGS.USImagery") %>% 
  #Manual set the view to the center of the US (roughly)
  setView(lat = 35, lng = -95, zoom = 4) %>%
  #Add a search option if a specific location is needed to be found.
  addSearchOSM()%>%
  addResetMapButton

#Split the data based on the season. 
decades = levels(factor(decade_df$decade))
#Plot each of the observations by season.
for (d in decades) {
  map <- map %>%
    #Add circle markers with popups per season.
    addCircleMarkers(data = filter(decade_df,decade == d),~longitude,~latitude,popup = ~title, group = d)
    #addHeatmap(data = filter(decade_df,decade == d),na.omit(~longitude),na.omit(~latitude), group = d,max = .6,radius = 20, intensity = .05, gradient = "Spectral")
}

map <- map %>% addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri")

# By adding the layerControl after the initial map setup, we get the generation of the overlayGroups to match the actual groups created above.
map <- map %>% addLayersControl(baseGroups  = decades, options = layersControlOptions(collapsed = F)) 
map
map
saveWidget(map, file="bigfoot_map_decades.html")