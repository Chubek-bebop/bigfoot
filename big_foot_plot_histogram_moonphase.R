#Load in an libraries.
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(ggplot2)

df <- read.csv('bfro_reports_geocoded.csv')

graph <- ggplot(df,aes(x = round(df$moon_phase, digits = 1)))+ geom_histogram(bins = length(levels(factor(round(df$moon_phase, digits = 1)))))
graph <- graph + scale_x_continuous(name = "Moon Phase")

graph
ggsave("bigfoot_moonphase_histogram.png")