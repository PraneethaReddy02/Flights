library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(syuzhet)
library(DT)
library(RColorBrewer)

server <- function(input, output) {
  
  tweets_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    df$airline <- str_extract(df$tweet_text, "@\\w+")
    df$clean_text <- tolower(df$tweet_text)
    df$clean_text <- gsub("[^a-z\\s]", "", df$clean_text)
    df$sentiment_score <- get_sentiment(df$clean_text, method = "syuzhet")
    df$sentiment_label <- case_when(
      df$sentiment_score > 0 ~ "Positive",
      df$sentiment_score < 0 ~ "Negative",
      TRUE ~ "Neutral"
    )
    df
  })
  
  output$avg_sentiment <- renderValueBox({
    avg <- round(mean(tweets_data()$sentiment_score), 2)
    valueBox(avg, "Avg Sentiment Score", icon = icon("smile"), color = "light-blue")
  })
  
  output$pct_positive <- renderValueBox({
    pct <- round(mean(tweets_data()$sentiment_label == "Positive") * 100, 1)
    valueBox(paste0(pct, "%"), "Positive Tweets", icon = icon("thumbs-up"), color = "green")
  })
  
  output$pct_negative <- renderValueBox({
    pct <- round(mean(tweets_data()$sentiment_label == "Negative") * 100, 1)
    valueBox(paste0(pct, "%"), "Negative Tweets", icon = icon("thumbs-down"), color = "red")
  })
  
  output$sentiment_hist <- renderPlot({
    ggplot(tweets_data(), aes(x = sentiment_score)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(title = "Distribution of Sentiment Scores", x = "Score", y = "Tweet Count") +
      theme_minimal()
  })
  
  output$airline_sentiment <- renderPlot({
    tweets_data() %>%
      group_by(airline) %>%
      summarise(avg_sentiment = mean(sentiment_score)) %>%
      ggplot(aes(x = reorder(airline, avg_sentiment), y = avg_sentiment, fill = avg_sentiment)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
      labs(title = "Average Sentiment by Airline", x = "Airline", y = "Avg Sentiment") +
      theme_minimal()
  })
  
  output$pos_cloud <- renderPlot({
    pos_words <- tweets_data() %>%
      filter(sentiment_label == "Positive") %>%
      unnest_tokens(word, clean_text) %>%
      anti_join(get_stopwords()) %>%
      count(word, sort = TRUE)
    
    wordcloud(words = pos_words$word, freq = pos_words$n,
              min.freq = 5, colors = brewer.pal(8, "Dark2"))
  })
  
  output$neg_cloud <- renderPlot({
    neg_words <- tweets_data() %>%
      filter(sentiment_label == "Negative") %>%
      unnest_tokens(word, clean_text) %>%
      anti_join(get_stopwords()) %>%
      count(word, sort = TRUE)
    
    wordcloud(words = neg_words$word, freq = neg_words$n,
              min.freq = 5, colors = brewer.pal(8, "Set1"))
  })
  
  output$tweet_table <- renderDT({
    tweets_data() %>%
      select(tweet_id, tweet_text, airline, sentiment_label, sentiment_score)
  })
}
