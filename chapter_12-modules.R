# Chapter 12 - Shiny Modules

library(shiny)

moduleServer <- function(id, module) {
  callModule(module, id)
}

library(shiny)

ui <- fluidPage(
  selectInput("var", "Variable",names(mtcars)),
  numericInput("bins", "bins", 10, min = 1),
  plotOutput("hist")
)

server <- function(input, output, session) {
  data <- reactive(mtcars[[input$var]])
  output$hist <- renderPlot({
    hist(data(), breaks = input$bins, main = input$var)
  })
}

shinyApp(ui, server)

### 12.2.1 - Module UI

histogramUI <- function(id) {
  list(
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

### 12.2.2 -  Module Server

histogramServer <- function(id) {
  moduleServer(id, function(input, output, server) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    })
  })
}

# 12.2.3 - Updated App

histogramModule <- function() {
  ui <- fluidPage(
    histogramUI("hist1")
  )
  
  server <- function(input, output, session) {
    histogramServer("hist1")
  }
shinyApp(ui)
}

#============================================================

# Book version

histogramUI <- function(id) {
  list(
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

histogramServer <- function(id) {
  moduleServer(id, function(input, output, server) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    })
  })
}

histogramModule <- function() {
  ui <- fluidPage(
    histogramUI("hist1")
  )
  server <- function(input, output, session) {
    histogramServer("hist1")
  }
  shinyApp(ui, server)  
}

#============================================================