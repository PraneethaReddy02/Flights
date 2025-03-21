# server.R
library(shiny)
library(tidytext)
library(dplyr)
library(stringr)
library(wordcloud)
library(ggplot2)
library(plotly)
library(DT)
library(textdata)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    updateSelectInput(session, "airlineFilter", choices = c("All", unique(df$airline)))
    return(df)
  })

  filteredData <- reactive({
    df <- data()
    if (input$airlineFilter != "All") {
      df <- df %>% filter(airline == input$airlineFilter)
    }
    return(df)
  })

  sentimentData <- reactive({
    df <- filteredData()
    df %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(airline, index = 1:nrow(df), sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
      mutate(sentiment = positive - negative)
  })

  output$sentimentPlot <- renderPlotly({
    sdata <- sentimentData()
    plot_ly(sdata, x = ~index, y = ~sentiment, type = "scatter", mode = "lines") %>%
      layout(title = "Sentiment Score Over Time", xaxis = list(title = "Tweet Index"), yaxis = list(title = "Sentiment Score"))
  })

  output$wordcloud <- renderPlot({
    df <- filteredData()
    df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      with(wordcloud(words = word, freq = n, max.words = input$numWords))
  })

  output$airlineSentimentPlot <- renderPlotly({
    sdata <- sentimentData() %>%
      group_by(airline) %>%
      summarise(avg_sentiment = mean(sentiment))
    plot_ly(sdata, x = ~airline, y = ~avg_sentiment, type = "bar") %>%
      layout(title = "Average Sentiment by Airline", xaxis = list(title = "Airline"), yaxis = list(title = "Average Sentiment"))
  })

  output$dataTable <- renderDataTable({
    datatable(filteredData())
  })
}
