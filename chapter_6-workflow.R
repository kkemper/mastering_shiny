# Chapter 6

### 6.2.1 - Reading Trace Backs

f <- function(x) g(x)
g <- function(x) h(x)
h <- function(x) x * 2

library(shiny)

ui <- fluidPage(
  selectInput("n", "N", 1:10),
  plotOutput("plot")
)

server <- function(input, output, session) {
    n <- f(input$n)
    plot(head(cars, n))
}

shinyApp(ui, server)