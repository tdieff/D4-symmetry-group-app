# The user interface
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

header <- dashboardHeader(title = "D4 According to Taylor Dieffenbach")

sidebar <- dashboardSidebar(
  width = 100,
  actionButton("btninit", "Initialize"),
  actionButton("btni", "Apply i"),
  actionButton("btnr", "Apply r"),
  actionButton("btns", "Apply s"),
  actionButton("btnt", "Apply t"),
  actionButton("btnw", "Apply w"),
  actionButton("btnx", "Apply x"),
  actionButton("btny", "Apply y"),
  actionButton("btnz", "Apply z")
)

# Build diagrams of the squares and group multiplication table
body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      plotOutput("configurations", height =200)
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("square", height = 300)
    ),
    column(
      width = 6,
      dataTableOutput("multiplicationTable")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

# Functions that implement the algebra
source("d4calc.R")

# Variables shared among server functions
D4DF <- D4.makeDataFrame()
vertexConfiguration <- "ABCD"

# Respond to user input, change state, generate visible output
server <- function(session, input, output) {
  output$configurations <- renderPlot(D4.showConfigs(D4DF))
  output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  table <- outer(D4DF$name, D4DF$name, vD4.multiply, DF = D4DF)
  colnames(table) <- D4DF$name
  rownames(table) <- D4DF$name 
  output$multiplicationTable <- renderDataTable(table, 
                                                options = list(dom = "t"))

  observeEvent(input$btninit,{
    vertexConfiguration <<- "ABCD"
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btni,{
    vertexConfiguration <<- D4.apply("i", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btnr,{
    vertexConfiguration <<- D4.apply("r", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btns,{
    vertexConfiguration <<- D4.apply("s", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btnt,{
    vertexConfiguration <<- D4.apply("t", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btnw,{
    vertexConfiguration <<- D4.apply("w", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btnx,{
    vertexConfiguration <<- D4.apply("x", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btny,{
    vertexConfiguration <<- D4.apply("y", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
  observeEvent(input$btnz,{
    vertexConfiguration <<- D4.apply("z", vertexConfiguration)
    output$square <- renderPlot(D4.showSquare(vertexConfiguration))
  })
}

shinyApp(ui = ui, server = server)