library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Airline Sentiment Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("By Airline", tabName = "airline", icon = icon("plane")),
      menuItem("Word Clouds", tabName = "wordclouds", icon = icon("cloud")),
      menuItem("Tweet Explorer", tabName = "explorer", icon = icon("table")),
      fileInput("file", "Upload Tweets CSV", accept = ".csv")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("avg_sentiment"),
                valueBoxOutput("pct_positive"),
                valueBoxOutput("pct_negative")
              ),
              fluidRow(
                box(title = "Sentiment Distribution", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("sentiment_hist"))
              )
      ),
      
      tabItem(tabName = "airline",
              box(title = "Average Sentiment by Airline", width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("airline_sentiment"))
      ),
      
      tabItem(tabName = "wordclouds",
              fluidRow(
                box(title = "Positive Tweets Word Cloud", width = 6, plotOutput("pos_cloud")),
                box(title = "Negative Tweets Word Cloud", width = 6, plotOutput("neg_cloud"))
              )
      ),
      
      tabItem(tabName = "explorer",
              box(title = "Tweet Table", width = 12, DT::DTOutput("tweet_table"))
      )
    )
  )
)
