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

library(shiny)
ui <- fluidPage(
  sliderInput("x", "If x is", 1, 50, step = 5, value = 30),
  sliderInput("y", "and y is", 1, 50, step = 5, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
 output$product <- renderText({
   product <- input$x * input$y
   product
 })
 output$product_plus5 <- renderText({
   product <- input$x * input$y
   product + 5
 }) 
 output$product_plus10 <- renderText({
   product <- input$x * input$y
   product + 10
 })
}

shinyApp(ui, server)

#=======================================

library(shiny)
ui <- fluidPage(
  sliderInput("x", "If x is", 1, 50, step = 5, value = 30),
  sliderInput("y", "and y is", 1, 50, step = 5, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
  product <- reactive({
    product <- input$x * input$y
    product
  })
  output$product <- renderText({
    product()
  })
  output$product_plus5 <- renderText({
    product() + 5
  }) 
  output$product_plus10 <- renderText({
    product() + 10
  })
}

shinyApp(ui, server)

### 5.

library(shiny)
library(ggplot2)

datasets <- data(package = "ggplot2")$results[, "Item"]

ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  tableOutput("plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset())
  })
}

shinyApp(ui, server)