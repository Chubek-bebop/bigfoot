library(ggplot2)
library(plotly)
library(dplyr)
library(scales) ## For the pretty_breaks()
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(shinythemes)
library(wordcloud2)
library(tidytext)
library(tidyverse)
library(textdata)
library(sf)



df <- read.csv('data/bigfoot.csv')
states_population <- read.csv('data/states_population.csv')
states_forests <- read.csv('data/states_forest.csv')
states_size <- read.csv('data/states_size.csv')
bears <- read.csv('data/bears_pop.csv') %>%
  mutate(total_bear_poulation = sum(est_population))

df_bears <- df %>%
  count(state) %>%
  mutate(total = sum(n)) %>%
  left_join(bears, by = c('state' = 'state')) %>%
  left_join(states_population, by = c('state' = 'state'))

df_forest <- df %>%
  count(state) %>%
  left_join(states_forests, by = c('state' = 'state')) %>%
  left_join(states_population, by = c('state' = 'state')) %>%
  left_join(states_size, by = ('state' = 'state'))



welcomeMainPanelHTML <- HTML(
  "
<center> <h3> Welcome to the Bigfoot Tracking Shiny App! </h3><center>
This Shiny App covers locations of sightings of Bigfoot across the US, reported between 1860 and 2021.
<center><img src = \"bigfoot_1.jpg\" width = 90%><figcaption></center>"
)

welcomeBigfootBackgroundHTML <- HTML(
  "
<center> <h3> Bigfoot - What is it?</h3></center>

Bigfoot, also commonly referred to as Sasquatch, is an ape-like cryptid commonly spotted in the continental US and Canada.
While no Bigfoot was ever captured, alive or otherwise, the reports of its sightings go back to as far back as approx. 500 BC.
The local population at the time carved pictures of a 'large wooly' creature in stones. <br><br>
<center><img src = \"bigfoot.jpg\" width = 90%><figcaption> The famous Bigfoot image captured by Roger Patterson and Robert Gimlin in 1967.</center> <br>

Nowdays, Bigfoot sightings are commonly reported and the BRFO (Bigfoot Field Researchers Organization) gathers all verified reports of sightings across the US.
The data contains among other things, location, date and conditions of the reported sightings. This data is used through out this app to help identify where Bigfoot might be native to in order to help the
deployed task force in finding the creature."
)

bearsSidePanelDescription <- HTML(
  "
<center><h3> Are Bigfoot sightings actually Bear sightings? </h3></center>
Black and Grizzly bears are indigenous to many US states. While they are quadrupedal,
they often stand on their hind legs to get a better view of the area and when tracking a smell.<br>
<center><img src='bear.jpg' width = 50%>
<figcaption> Fig. 1 - Black bear chilling on a rock, looking all human.</figcaption><br>
</center>This makes them appear remarkably humanlike, especially from a distance in a forest.
Could the reports of Bigfoot sigthings in states with high bear population actually be bear sightings instead?"
)

yearsSidePanelDescription <- HTML(
  "
<h3> Sightings over the year </h3>
Plot and table of Bigfoot sightings across the US between 1920 and 2020. <br>
Use the variables below to plot the number of sightings of selected Class and between the years.

"
)

heatmapHTMLSide <- HTML(
  "
<h3> Bigfoot sightings across the decades.</h3>
Bigfoot is believed to live in Pacific Northwest region of continental US. This however is a historic believe not supported by data.<br> <br>\
In order to determin Bigfoot's native habitat, the sightings were plotted onto a map. Furthermore, to check if its habitate is changing over time, sightings were group by decade and plotted as heatmaps over US.<br>

                        "
)
forestSidePanelDescription <- HTML(
  "<div class = \"forest\">
<center><h3> Bigfoot and forests </h3> </center><br>
Historically, Bigfoot has been associated with deep forest. Most of the initial sighitngs were reported in heavily forrested states such as Washington, Oregon and Montana.<br><br>
</div>

                                   "
)



date2season <- function(date) {
  season_start <- c("0101", "0321", "0621", "0923", "1221")
  season_name <- c("Winter", "Spring", "Summer", "Fall", "Winter")
  mmdd <- format(date, "%m%d")
  season_name[findInterval(mmdd, season_start)]
}

df <- df %>%
  filter(!is.na(date)) %>%
  mutate(observed = as.character(observed)) %>%
  mutate(year = as.numeric(format(as.Date(date, format = "%d/%m/%Y"), "%Y"))) %>%
  mutate (season_actual = date2season(as.Date(date, format = "%d/%m/%Y"))) %>%
  arrange(desc(year), desc(classification))

ui <- fluidPage(
  theme = shinytheme('sandstone'),
  HTML("<title> Bigfoot</title>"),
  
  titlePanel((
    HTML("<center>Tracking Bigfoot</center>")
  )),
  tags$head(tags$title("Bigfoot")),
  navlistPanel(
    tabPanel("Welcome!",
             tabsetPanel(
               tabPanel("Intro",
                        mainPanel(welcomeMainPanelHTML)),
               tabPanel(
                 "Bigfoot Background",
                 mainPanel(welcomeBigfootBackgroundHTML)
               )
             )),
    tabPanel(
      "Sightings over the years",
      titlePanel("Bigfoot sightings between 1920 and 2020"),
      tabsetPanel(
        tabPanel("Plot",
                 sidebarLayout(
                   sidebarPanel(
                     yearsSidePanelDescription,
                     checkboxGroupInput(
                       "class",
                       label = h3("Sighting classifications"),
                       choices = unique(df$classification),
                       selected = unique(df$classification)[1:3]
                     ),
                     sliderInput(
                       'year_range',
                       h3('Year range'),
                       value = c(1920, 2020),
                       min = 1920,
                       max = 2023
                     )
                   ),
                   mainPanel(plotlyOutput('plot'))
                 )),
        tabPanel("Table",
                 mainPanel(DTOutput('year_table'))),
        tabPanel("State Map",
                 mainPanel(leafletOutput('stateMap'))),
        tabPanel("Season Map",
                 mainPanel(
                   leafletOutput('seasonsMap', width = "200%")
                 ))
      )
    ),
    tabPanel (
      "Sightings across time",
      titlePanel("Bigfoot sightings heatmap."),
      sidebarPanel(heatmapHTMLSide),
      mainPanel(leafletOutput('heatmap_sightings'))
    ),
    tabPanel("Is it a bear?",
             sidebarLayout(
               sidebarPanel(bearsSidePanelDescription),
               mainPanel(plotlyOutput('bearsPlot'))
             )),
    tabPanel(
      "Forests and Bigfoot",
      sidebarLayout(
        sidebarPanel(forestSidePanelDescription),
        mainPanel(plotlyOutput('forestPlot'))
      )
    ),
    
    tabPanel(
      "Sightings by year and classification.",
      titlePanel("Bigfoot sightings by year and classification."),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            'year',
            'Select the year',
            selected = 2023,
            choices = unique(na.omit(df$year))
          ),
          checkboxGroupInput(
            "class2",
            label = h3("Sighting classifications"),
            choices = unique(df$classification),
            selected = unique(df$classification)[1:3]
          )
        ),
        mainPanel(DT::DTOutput("table"))
      )
    ),
    
    tabPanel(
      "Wordcloud",
      titlePanel("Wordcloud of the text descriptions of the sightings."),
      sidebarLayout(
        sidebarPanel(
          numericInput(
            'wordcloud_words',
            h3('Number of words'),
            value = 20,
            max = 50
          ),
          sliderInput(
            'wordcloud_year_range',
            h3('Year range'),
            value = c(1920, 2023),
            min = 1920,
            max = 2023
          ),
          actionButton('go', 'Update wordcloud')
        ),
        mainPanel(wordcloud2Output('wordcloud'))
      )
    ),
    tabPanel(
      "Sentiment analysis",
      sidebarLayout(sidebarPanel(
        selectInput(
          'dict',
          'Select the sentiment dict',
          selected = 'nrc',
          choices = c('nrc', 'bing')
        )
      ),
      
      mainPanel(plotlyOutput(
        "sentimentAnalysis"
      )))
      
    )
    ,
    widths = c(2, 8)
  )
)



server <- function(input, output, session) {
  output$plot <- generatePlot(input)
  output$year_table <- generateYearTable(input)
  output$seasonsMap <- generateMapSeasons(input)
  output$heatmap_sightings <-
    generateHeatMap_sightings(generateHeatMap_sightings)
  observeEvent(input$go,
               {
                 ivals <- c(input$wordcloud_year_range, input$wordcloud_words)
                 output$wordcloud <- generateWordcloud(ivals)
               })
  output$sentimentAnalysis <- generateSentimentAnalysis(input)
  output$bears <- generateBears(input)
  output$bearsPlot <- generateBearsPlot(input)
  output$forestPlot <- generateForestPlot(input)
  output$stateMap <- generateStateMap(input)
  
}

generateYearTable <- function(input)
{
  renderDT(
    data_by_state <- df %>%
      filter(classification %in% input$class) %>%
      group_by(year) %>%
      count() %>%
      na.omit()
    
  )
}

generatePlot <- function(input) {
  renderPlotly({
    data_by_state <- df %>%
      filter(classification %in% input$class) %>%
      group_by(year) %>%
      count()
    
    
    ggplotly(
      ggplot(data_by_state, aes(
        x = year, y = n, text =  year
      ))  +
        geom_line (color = 'red') +
        scale_x_continuous(breaks = pretty_breaks(), limits = input$year_range)      +
        scale_y_continuous(breaks = pretty_breaks(), limits = c(0, 300))      +
        labs(title = "Bigfoot sightings by year and classification.",
             x = "Year",
             y = "Number of sightings") + theme_classic() + geom_area(fill = 'red', alpha = 0.1)
      + annotate(
        "segment",
        x = 1967,
        xend = 1967,
        y = 0,
        yend = 250,
        color = 'black'
      )
      + geom_text(
        aes(
          x = 1967,
          y = 275,
          label = "1967 Picture of Bigfoot",
          text = HTML(" "),
          angle = 30
        )
      ),
      tooltip = "text"
    )
  })
}

generateWordcloud <- function(vals) {
  renderWordcloud2({
    custom_stop_words <-   tribble(~ word, ~ lexicon,
                                   "HERE GOES A CUSTOM STOP WORD", "CUSTOM")
    
    stop_words_complete <-
      stop_words %>% bind_rows(custom_stop_words)
    
    df_words <- unnest_tokens(df, word, observed) %>%
      anti_join(stop_words_complete) %>%
      select(year, word) %>%
      filter(year > vals[1] & year < vals[2]) %>%
      count(word) %>%
      arrange(desc(n))
    
    
    wordcloud2(
      df_words[1:vals[3],],
      size = 0.5,
      shape = 'diamond',
      backgroundColor = "white"
    )
  })
}

generateHeatMap_sightings <- function(input) {
  renderLeaflet({
    #Extract the year from the date.
    year_df <- df %>%
      mutate(year = as.numeric(format(
        as.Date(df$date, format = "%d/%m/%Y"), "%Y"
      )))
    
    #Get decades.
    decade_df <- year_df %>%
      mutate(decade  = year - year %% 10)
    
    #Define color palette.
    pal_decades <- colorNumeric(palette = "Reds",
                                domain = decade_df$decade)
    
    #Plot the data using leaflet by piping in the data.frame
    map <- decade_df %>%
      leaflet() %>%
      #Set provider tiles (i.e. the way the map looks)
      addTiles() %>%
      #Manual set the view to the center of the US (roughly)
      setView(lat = 35,
              lng = -95,
              zoom = 3) %>%
      #Add a search option if a specific location is needed to be found.
      addSearchOSM() %>%
      addResetMapButton
    
    #Split the data based on the season.
    decades = levels(factor(decade_df$decade))
    #Plot each of the observations by season.
    for (d in decades) {
      map <- map %>%
        #Add circle markers with popups per season.
        addHeatmap(
          data = filter(decade_df, decade == d),
          ~ longitude,
          ~ latitude,
          group = d,
          gradient = 'Spectral',
          radius = 10,
          intensity = .5
        )
      #addHeatmap(data = filter(decade_df,decade == d),na.omit(~longitude),na.omit(~latitude), group = d,max = .6,radius = 20, intensity = .05, gradient = "Spectral")
    }
    
    map <-
      map %>% addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri")
    
    # By adding the layerControl after the initial map setup, we get the generation of the overlayGroups to match the actual groups created above.
    map <-
      map %>% addLayersControl(baseGroups  = decades, options = layersControlOptions(collapsed = F))
  })
}


generateStateMap <- function(input)
{
  renderLeaflet({
    shp <-
      read_sf('data/shp/500k/cb_2022_us_state_500k.shp') %>% sf::st_transform('+proj=longlat +datum=WGS84')
    total_per_state <- df %>% count(state)
    shp <-
      shp %>% left_join(total_per_state, by = c("NAME" = "state"))
    pal <- colorFactor(palette = "greens",
                       domain = shp$n)
    
    #Plot the data using leaflet by piping in the data.frame
    map <- shp %>%
      leaflet() %>%
      #Set provider tiles (i.e. the way the map looks)
      addTiles() %>%
      #Manual set the view to the center of the US (roughly)
      setView(lat = 35,
              lng = -95,
              zoom = 4) %>%
      #Add a search option if a specific location is needed to be found.
      addSearchOSM() %>%
      addResetMapButton %>%
      addPolygons(
        weight = 1,
        opacity = 0.5,
        dashArray = "3",
        fillColor = ~ pal(n),
        fillOpacity = 0.2,
        highlightOptions = highlightOptions(
          weight = 5,
          dashArray = "",
          fillOpacity = 0.5,
          bringToFront = TRUE
        ),
        label = ~ paste(
          "State",
          "<b>",
          NAME,
          "</b> <br>",
          "Total sightings: ",
          "<b>",
          n,
          "</b> <br>"
        ) %>% lapply(htmltools::HTML)
      )
  })
  
}

generateMapSeasons <- function(input) {
  renderLeaflet({
    map <- df %>%
      leaflet() %>%
      addTiles() %>%
      setView(lat = 35,
              lng = -95,
              zoom = 4) %>%
      addSearchOSM() %>%
      addResetMapButton()
    
    
    pal_seasons <- colorFactor(
      palette = c('brown', 'blue', 'green', 'red', 'black'),
      levels = c("Fall", "Winter", "Spring", "Summer", "Unknown")
    )
    
    seasons = levels(factor(df$season_actual))
    
    for (s in seasons) {
      map <- map %>%
        addHeatmap(
          data = filter(df, season_actual == s) %>% na.omit(),
          ~ longitude,
          ~ latitude,
          group = s,
          intensity = 0.1,
          gradient = "RdYlGn"
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
    
  })
}



generateSentimentAnalysis <- function(input)
{
  renderPlotly({
    df_sentiment_plot <- df %>%
      mutate(year = as.numeric(format(
        as.Date(date, format = "%Y-%m-%d"), "%Y"
      ))) %>%
      unnest_tokens(word, observed) %>%
      na.omit() %>%
      select(year, word) %>%
      group_by(year) %>%
      inner_join(get_sentiments('nrc'), by = "word") %>%
      count(sentiment) %>%
      group_by(year) %>%
      mutate(total_per_year = sum(n)) %>%
      ungroup() %>%
      mutate(fraction = n / total_per_year)
    
    ggplotly(ggplot(
      df_sentiment_plot,
      aes(
        x = year,
        y = fraction,
        text = paste0(
          "Sentiment: ",
          sentiment,
          "\nFreaction: ",
          fraction,
          "\nYear: ",
          year
        ),
        group = 1
      )
    ) + geom_line(),
  tooltip =  "text")
  })
}

generateBears <- function(input)
{
  DT::renderDataTable(df_bears)
}
generateBearsPlot <- function(input)
{
  renderPlotly(
    ggplotly(
      ggplot(
        df_bears,
        aes(
          x = est_population,
          y = n,
          size = population,
          color = population,
          text = paste(
            "State:",
            state,
            "\nEstimated bear population: ",
            est_population,
            "\nNumber of bigfoot sightings: ",
            n
          )
        ),
        show.legend = F
      )
      + geom_point() +
        scale_x_log10() +
        scale_y_log10() +
        labs(title = 'Bear population and Bigfoot sightings per state',
             x = 'Estimated Bear population',
             y = 'Number of Bigfoot sightings between 1860 and 2023') +
        theme_classic() + guides(color = FALSE),
      tooltip = "text"
    )
  )
}

generateForestPlot <- function(input)
{
  renderPlotly(
    ggplotly(
      ggplot(
        df_forest,
        aes(
          x = fraction_forest,
          y = n,
          size = state_area,
          color = state_area,
          text = paste(
            "State:",
            state,
            "\nForrested area: ",
            fraction_forest,
            "\nNumber of bigfoot sightings: ",
            n
          )
        ),
        show.legend = F
      )
      + geom_point() +
        scale_x_log10() +
        scale_y_log10() +
        labs(title = 'Bigfoot sightings as a function of forest cover',
             x = '% of forested area',
             y = 'Number of Bigfoot sightings between 1860 and 2023') +
        theme_classic() + guides(color = FALSE),
      tooltip = "text"
    )
  )
}

shinyApp(ui = ui, server = server)
