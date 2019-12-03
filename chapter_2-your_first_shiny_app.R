# Chapter 2 - Your First Shiny App

library(shiny)

## 2.2 Create App Directory and File

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
}
shinyApp(ui, server)

## 2.8 - Exercises

### 1.

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello, ", input$name)
  })
}

shinyApp(ui, server)

### 2.

ui <- fluidPage(
  sliderInput("x", "If x is", 1, 50, step = 5, value = 1),
  textOutput("number")
)

server <- function(input, output, session) {
  output$number <- renderText({
    input$x * 5
  })
}

shinyApp(ui, server)

### 3.

ui <- fluidPage(
  sliderInput("x", "If x is", 1, 50, step = 5, value = 1),
  sliderInput("y", "and y is", 1, 50, step = 5, value = 1),
  textOutput("number")
)

server <- function(input, output, session) {
  output$number <- renderText({
    input$x * input$y
  })
}

shinyApp(ui, server)

### 4.

#### !!Not Complete!!

