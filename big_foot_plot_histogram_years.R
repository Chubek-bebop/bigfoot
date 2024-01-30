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

#Plot histogram
graph <- ggplot(year_df, aes(x = year)) + geom_histogram(bins = length(levels(factor(year_df$year)))) + scale_x_log10() + ylab("Number of sightings") + ggtitle("Number of Bigfoot sightings in the US by year")
graph <- graph + scale_x_continuous(name = "Year", limits = c(1920,2030))
graph
ggsave("bigfoot_year_histogram.png")
