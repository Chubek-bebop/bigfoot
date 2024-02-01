df <- read.csv('bfro_reports_geocoded.csv')

df_sentiment_plot <- df %>%
  mutate(year = as.numeric(format(as.Date(date, format = "%Y-%m-%d"), "%Y"))) %>%
  unnest_tokens(word, observed) %>%
  na.omit() %>%
  select(year, word) %>%
  group_by(year) %>%
  inner_join(get_sentiments('nrc'), by = "word", relationship = "many-to-many") %>%
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
                          group = 0#### THIS LINE FIXES THE INVISIBLE
                        )) + xlim(2010, 2020),
         tooltip =  c("text"))
