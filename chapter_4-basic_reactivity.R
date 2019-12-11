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

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name, "!")
  })
}

shinyApp(ui, server)

### 4.3.6 - Exercises

## 4.4 - Reactive Expressions

library(ggplot2)

histogram <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)){
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  
  ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(200, mean = 0.15, sd = 0.9)

histogram(x1, x2)
cat(t_test(x1, x2))

### 4.4.2 - The App

ui <- fluidPage(
fluidRow(
  column(4,
         "Distribution 1",
         numericInput("n1", label = "n", value = 1000, min = 1),
         numericInput("mean1", label = "µ", value = 0, step = 0.1),
         numericInput("sd1", label = "s", value = 0.5, min = 0.1, step = 0.1)
         ),
  column(4,
         "Distribution 2",
         numericInput("n2", label = "n", value = 1000, min = 1),
         numericInput("mean2", label = "µ", value = 0, step = 0.1),
         numericInput("sd2", label = "s", value = 0.5, min = 0.1, step = 0.1)
         ),
  column(4,
         "Histogram",
         numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
         sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
  )
),
fluidRow(
  column(9, plotOutput("hist")),
  column(3, verbatimTextOutput("ttest"))
  )
)

server <- function(input, output, session) {
 output$hist <- renderPlot({
   x1 <- rnorm(input$n1, input$mean1, input$sd1)
   x2 <- rnorm(input$n2, input$mean2, input$sd2)
   
   histogram(x1, x2, binwidth = input$binwidth, xlim = input$range)
 }) 

 output$ttest <- renderText({
   x1 <- rnorm(input$n1, input$mean1, input$sd1)
   x2 <- rnorm(input$n2, input$mean2, input$sd2)
   
   t_test(x1, x2)
 })
 }

shinyApp(ui, server)

### 4.4.4 - Simplifying the Graph
library(shiny)
library(ggplot2)
ui <- fluidPage(
  fluidRow(
    column(4,
           "Distribution 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("mean1", label = "µ", value = 0, step = 0.1),
           numericInput("sd1", label = "s", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Distribution 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("mean2", label = "µ", value = 0, step = 0.1),
           numericInput("sd2", label = "s", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Histogram",
           numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
           sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
)

server <- function(input, output, session) {
  x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
  x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }) 
  
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
}

shinyApp(ui, server)

## 4.5 - Controlling the Time of Evaluation

ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1e4, min = 0)
           ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  x1 <- reactive(rpois(input$n, input$lambda1))
  x2 <- reactive(rpois(input$n, input$lambda2))
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  })
}

shinyApp(ui, server)

### 4.5.1 - Timed Invalidation

ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1e4, min = 0)
    ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  timer <- reactiveTimer(500)
  x1 <- reactive({
    timer()
    rpois(input$n, input$lambda1)
    })
  x2 <- reactive({
    timer()
    rpois(input$n, input$lambda2)
    })
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  })
}

shinyApp(ui, server)

### 4.5.2 - On Click

ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {

  x1 <- reactive({
    input$simulate
    rpois(input$n, input$lambda1)
  })
  x2 <- reactive({
    input$simulate
    rpois(input$n, input$lambda2)
  })
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  })
}

shinyApp(ui, server)

# ==================

ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  
  x1 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda1)
  })
  x2 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  })
}

shinyApp(ui, server)

## 4.6 - Observers

server <- function(input, output, session) {
  text <- reactve(paste0("Hello!", input$name, "!"))
  
  output$greeting <- renderText(text())
  observeEvent(input$name, {
    message("Greeting performed")
  })
}