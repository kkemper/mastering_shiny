library(shiny)

# Chapter 4 - Basic Reactivity

## 4.2 - The Server Function

### 4.2.1 - Input
ui <- fluidPage(
  numericInput("count", label = "Number of values", value = 100)
)

server <- function(input, output, session) {
  input$count <- 10
}

shinyApp(ui, server)

# Throws error deliberately

## ====================

ui <- fluidPage(
  numericInput("count", label = "Number of values", value = 100)
)

server <- function(input, output, session) {
  message("The value of input$countn is ", input$count)
}

shinyApp(ui, server)

# Throws error deliberately

### 4.2.2 - Output

ui <- fluidPage(
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText("Hello human!")
}

shinyApp(ui, server)

## 4.3 - Reactive Programming

