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
    print("Filtered Data:")
    print(head(df))
    processed_data <- df %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("afinn"), by = "word")
    print("After unnesting and joining:")
    print(head(processed_data))
    final_data <- processed_data %>%
      group_by(airline, index = 1:nrow(df)) %>%
      summarise(sentiment = sum(value))
    print("Final Data:")
    print(head(final_data))
    return(final_data)
  })

  output$sentimentPlot <- renderPlotly({
    sdata <- sentimentData()
    print("Sentiment Plot Data:")
    print(head(sdata))
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
      summarise(avg_sentiment = mean(sentiment, na.rm = TRUE)) #Added na.rm = TRUE
    print("Airline Sentiment Plot Data:")
    print(head(sdata))
    plot_ly(sdata, x = ~airline, y = ~avg_sentiment, type = "bar") %>%
      layout(title = "Average Sentiment by Airline", xaxis = list(title = "Airline"), yaxis = list(title = "Average Sentiment"))
  })

  output$dataTable <- renderDataTable({
    datatable(filteredData())
  })
}
