library(shiny)

# Chapter 9 - Dynamic UI

## 9.1 - Updating Inputs

ui <- fluidPage(
  numericInput("min", "Minimum", 0),
  numericInput("max", "Maximum", 3),
  sliderInput("n", "n", min = 0, max = 3, value = 1)
)

server <- function(input, output, session) {
  observeEvent(input$min, {
    updateNumericInput(session, "n", min = input$min)
  })
  observeEvent(input$max, {
    updateNumericInput(session, "n", max = input$max)
  })
}

shinyApp(ui, server)

### 9.1.1 - Simple Uses

ui <- fluidPage(
  sliderInput("x1", "x1", 0, min = -10, max = 10),
  sliderInput("x2", "x2", 0, min = -10, max = 10),
  sliderInput("x3", "x3", 0, min = -10, max = 10),
  actionButton("reset", "Reset")
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateNumericInput(session, "x1", value = 0)
    updateNumericInput(session, "x2", value = 0)
    updateNumericInput(session, "x3", value = 0)
  })
}

shinyApp(ui, server)

#----------------------------------------------------

ui <- fluidPage(
  numericInput("n", "Simulations", 10),
  actionButton("simulate", "Simulate")
)

server <- function(input, output, session){
  observeEvent(input$n, {
    label <- paste0("Simulate ", input$n, " times")
    updateActionButton(session, "simulate", label = label)
  })
}

shinyApp(ui, server)

### 9.1.2 - Hierarchical Select Boxes

sales <- vroom::vroom("sales-dashboard/sales_data_sample.csv", col_types = list())
sales
