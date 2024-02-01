#big_foot_map.R by Bart Czubak

#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(sf)
library(sp)


date2season <- function(date) {
  season_start <- c("0101", "0321", "0621", "0923", "1221")
  season_name <- c("Winter", "Spring", "Summer", "Fall", "Winter")
  mmdd <- format(date, "%m%d")
  season_name[findInterval(mmdd, season_start)]
}

df <- read.csv('bfro_reports_geocoded.csv')
df <- df %>%
  mutate(year = as.numeric(format(as.Date(date, format = "%Y-%m-%d"), "%Y"))) %>%
  mutate (season_actual = date2season(as.Date(date, format = "%Y-%m-%d"))) %>%
  arrange(desc(year), desc(classification))


shp <-
  read_sf('shapefiles/states/cb_2022_us_state_500k.shp') %>%   sf::st_transform('+proj=longlat +datum=WGS84')

map <- df %>%
  leaflet() %>%
  addProviderTiles("USGS.USImagery") %>%
  setView(lat = 35, lng = -95, zoom = 4) %>%
  addSearchOSM() %>%
  addResetMapButton


pal_seasons <- colorFactor(
  palette = c('brown', 'blue', 'green', 'red', 'black'),
  levels = c("Fall", "Winter", "Spring", "Summer", "Unknown")
)

seasons = levels(factor(df$season_actual))
for (s in seasons) {
  map <- map %>%
    addHeatmap(
      data = filter(df, season == s) %>% na.omit(),
      ~ longitude,
      ~ latitude,
      group = s,
      gradient ="RdYlGn"
    )
}

map <-
  map %>% addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri")

map <-
  map %>% addLayersControl(
    baseGroups = c("USGS.USImagery", "Carto", "Esri"),
    overlayGroups = seasons,
    options = layersControlOptions(collapsed = FALSE)
  )
map


map
saveWidget(map, file = "bigfoot_map_seasons.html")
