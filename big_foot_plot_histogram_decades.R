#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(ggplot2)

df <- read.csv('bfro_reports_geocoded.csv')

#Extract the year from the date.
year_df <- df %>%
  mutate(year = as.numeric(format(as.Date(df$date, format = "%Y-%m-%d"), "%Y")))

#Get decades.
decade_df <- year_df %>%
  mutate(decade  = year - year %% 10) %>%
  group_by(decade)

#Plot histogram
graph <- ggplot(decade_df, aes(x = decade)) + geom_histogram(bins = length(levels(factor(decade_df$decade)))) + scale_x_log10() + xlab("Decade") + ylab("Number of sightings") + ggtitle("Number of Bigfoot sightings in the US by decade.")
graph
ggsave("bigfoot_decade_histogram.png")
