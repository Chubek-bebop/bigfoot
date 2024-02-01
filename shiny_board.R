library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(shinythemes)
library(scales)
library(wordcloud2)
library(tidytext)
library(tidyverse)
library(textdata)



df <- read.csv('bfro_reports_geocoded.csv')

date2season <- function(date) {
  season_start <- c("0101", "0321", "0621", "0923", "1221")
  season_name <- c("Winter", "Spring", "Summer", "Fall", "Winter")
  mmdd <- format(date, "%m%d")
  season_name[findInterval(mmdd, season_start)]}

df <- df %>%
  filter(!is.na(date)) %>%
  mutate(year = as.numeric(format(as.Date(date, format="%Y-%m-%d"),"%Y"))) %>%
  mutate (season_actual = date2season(as.Date(date,format="%Y-%m-%d"))) %>%
  arrange(desc(year), desc(classification))

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel(("Bigfoot Sightings Tracking App")),
  tabsetPanel(
            tabPanel("Start",
                     titlePanel("Welcome to the Bigfoot sightings tracking app!"),
                     img(src='bigfoot.jpg', align = "right")),
            
            tabPanel("Sightings by state and classification",
                     titlePanel("Bigfoot sightings by state and classification."),
                        sidebarLayout(sidebarPanel(
                          selectInput('state', h3('Select the state'), selected = 'Alabama', choices = unique(df$state)),
                          checkboxGroupInput("class", label = h3("Sighting classifications"), 
                                             choices = unique(df$classification),
                                             selected = unique(df$classification)[1:3]),
                          sliderInput('year_range', h3('Year range'), value = c(1990, 2020), min = 1860, max = 2023)
                          ),
                        mainPanel(plotOutput('plot')))),
  
            tabPanel("Sightings by year and classification.", 
                     titlePanel("Bigfoot sightings by year and classification."),
                        sidebarLayout(sidebarPanel(
                          selectInput('year', 'Select the year', selected = 2023, choices = unique(na.omit(df$year))),
                          checkboxGroupInput("class2", label = h3("Sighting classifications"), 
                                             choices = unique(df$classification),
                                             selected = unique(df$classification)[1:3])),
                        mainPanel(DT::DTOutput("table")))),
            
            tabPanel ("Heatmap of sightings per decade",
                      titlePanel("Bigfoot sightings heatmap."),
                      sidebarPanel(textOutput('mapTextInfo')),
                      mainPanel(leafletOutput('heatmap_sightings'))),
            tabPanel("Wordcloud",
                     titlePanel("Wordcloud of the text descriptions of the sightings."),
                     sidebarLayout(sidebarPanel(
                       numericInput('wordcloud_words',h3('Number of words'), value = 20, max = 50),
                       sliderInput('wordcloud_year_range', h3('Year range'), value = c(1990, 2020), min = 1860, max = 2023),
                       actionButton('go', 'Update wordcloud')),
                     mainPanel(wordcloud2Output('wordcloud')))),
            tabPanel("Sentiment analysis",
                     
                     selectInput('dict', 'Select the sentiment dict', selected = 'nrc', choices = c('nrc', 'bing','loughran')),
  
                     mainPanel(plotlyOutput("sentimentAnalysis"))
            
                     )
            )
  )
  
server <- function(input, output, session) {
  
  output$plot <- generatePlot(input)
  output$table <- generateTable(input)
  output$heatmap_sightings <- generateHeatMap_sightings(generateHeatMap_sightings)
  observeEvent(input$go,
               { ivals <- c(input$wordcloud_year_range, input$wordcloud_words)
                 output$wordcloud <-generateWordcloud(ivals)})
  output$sentimentAnalysis <- generateSentimentAnalysis(input)
  
  
  output$mapTextInfo <- renderText("Heatmap of all of geotagged sightings of Bigfoot between 1860 and 2023 grouped by decade.")
}

generatePlot <- function(input){
  renderPlot({
    data_by_state <- df %>%
      filter(state == input$state, classification %in% input$class) %>%
      group_by(year) %>%
      count()
    
    
      ggplot(data_by_state,aes(x = year,y = n))+
       geom_line (color = 'blue')+
       scale_x_continuous(breaks = pretty_breaks())+
       scale_y_continuous(breaks = pretty_breaks())+
       xlim(input$year_range)+
       labs(
        title = "Bigfoot sightings by year and classification.",
        x = "Year",
        y = "Number of sightings"
      )+        theme_classic()
    })
} 

generateWordcloud <- function(vals){
  renderWordcloud2({
    custom_stop_words <-   tribble(~word, ~lexicon,
                                 "HERE GOES A CUSTOM STOP WORD", "CUSTOM")
    
    stop_words_complete <- stop_words %>% bind_rows(custom_stop_words)
    
    df_words <- unnest_tokens(df,word,observed) %>%
      anti_join(stop_words_complete) %>%
      select(year, word)%>%
      filter(year > vals[1] & year < vals[2]) %>%
      count(word) %>% 
      arrange(desc(n))

    
    wordcloud2(df_words[1:vals[3],],size=0.5, shape = 'diamond',backgroundColor = "gold")
  })
}

generateHeatMap_sightings <- function(input){
  renderLeaflet({
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
      setView(lat = 35, lng = -95, zoom = 3) %>%
      #Add a search option if a specific location is needed to be found.
      addSearchOSM()%>%
      addResetMapButton
    
    #Split the data based on the season. 
    decades = levels(factor(decade_df$decade))
    #Plot each of the observations by season.
    for (d in decades) {
      map <- map %>%
        #Add circle markers with popups per season.
        addHeatmap(data = filter(decade_df,decade == d),~longitude,~latitude, group = d, gradient = 'Spectral', radius =10, intensity = .5)
      #addHeatmap(data = filter(decade_df,decade == d),na.omit(~longitude),na.omit(~latitude), group = d,max = .6,radius = 20, intensity = .05, gradient = "Spectral")
    }
    
    map <- map %>% addProviderTiles("CartoDB", group = "Carto") %>% addProviderTiles("Esri", group = "Esri")
    
    # By adding the layerControl after the initial map setup, we get the generation of the overlayGroups to match the actual groups created above.
    map <- map %>% addLayersControl(baseGroups  = decades, options = layersControlOptions(collapsed = F)) 
  })
}

generateSentimentAnalysis <- function(input)
{
  renderPlotly({
    
df_sentiment_plot <- df %>%
  mutate(year = as.numeric(format(as.Date(date, format = "%Y-%m-%d"), "%Y"))) %>%
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

ggplotly(ggplot(df_sentiment_plot, aes(x = year,
                        y = fraction)) + geom_line(aes(
                          text = paste(
                            "Sentiment: ",
                            sentiment,
                            "\nFreaction: ",
                            fraction,
                            "\nYear: ",
                            year
                          ),
                          color = sentiment,
                          group = 1 #### THIS LINE FIXES THE INVISIBLE LINES BUG
                        )) + xlim(2010, 2020),
         tooltip =  c("text"))
  }
  )
}

generateTable <- function(input){
  DT::renderDT(
  data_by_state <- df %>%
    filter(year == input$year, classification %in% input$class2) %>%
    count(state)%>%
    arrange(desc(n)),

  colnames = c("State","Number of sightings"),
  options = list(order = list(2, 'desc'))
  
  )
    
} 

shinyApp(ui = ui, server = server)
