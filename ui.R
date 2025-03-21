# ui.R
library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Airline Tweet Sentiment Analysis"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      selectInput("airlineFilter", "Filter by Airline", choices = c("All")),
      sliderInput("numWords", "Number of Top Words", min = 5, max = 50, value = 20)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Score Distribution", plotlyOutput("sentimentPlot")),
        tabPanel("Top Words", plotOutput("wordcloud")),
        tabPanel("Sentiment by Airline", plotlyOutput("airlineSentimentPlot")),
        tabPanel("Raw Data", dataTableOutput("dataTable"))
      )
    )
  )
)
