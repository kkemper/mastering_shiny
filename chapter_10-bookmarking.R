# Chapter 10 - Bookmarking

##  10.1 Basic Idea

library(shiny)

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("omega", "omega", value = 1, min = -2, max = 2, step = 0.01),
        sliderInput("delta", "delta", value = 1, min = 0, max = 2, step = 0.01),
        sliderInput("damping", "damping", value = 1, min = 0.9, max = 1, step  = 0.001),
        numericInput("length", "length", value = 100),
        bookmarkButton()
      ),
      mainPanel(
        plotOutput("fig")
      )
    )
  )
}

server <- function(input, output, session) {
  t <- reactive(seq(0, input$length, length = input$length * 100))
  x <- reactive(sin(input$omega * t() + input$delta) * input$damping ^ t())
  y <- reactive(sin(t()) * input$damping ^ t())
  
  output$fig <- renderPlot({
    plot(x(), y(), axes = F, xlab = "", ylab = "", type = "l", lwd  = 2)
  })
}

shinyApp(ui, server, enableBookmarking = "url")

### 10.1.1 - Updating the URL

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("omega", "omega", value = 1, min = -2, max = 2, step = 0.01),
        sliderInput("delta", "delta", value = 1, min = 0, max = 2, step = 0.01),
        sliderInput("damping", "damping", value = 1, min = 0.9, max = 1, step  = 0.001),
        numericInput("length", "length", value = 100),
        bookmarkButton()
      ),
      mainPanel(
        plotOutput("fig")
      )
    )
  )
}

server <- function(input, output, session) {
  t <- reactive(seq(0, input$length, length = input$length * 100))
  x <- reactive(sin(input$omega * t() + input$delta) * input$damping ^ t())
  y <- reactive(sin(t()) * input$damping ^ t())
  
  output$fig <- renderPlot({
    plot(x(), y(), axes = F, xlab = "", ylab = "", type = "l", lwd  = 2)
  })
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)

}

shinyApp(ui, server, enableBookmarking = "url")

### 10.1.2 - Storing State

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("omega", "omega", value = 1, min = -2, max = 2, step = 0.01),
        sliderInput("delta", "delta", value = 1, min = 0, max = 2, step = 0.01),
        sliderInput("damping", "damping", value = 1, min = 0.9, max = 1, step  = 0.001),
        numericInput("length", "length", value = 100),
        bookmarkButton()
      ),
      mainPanel(
        plotOutput("fig")
      )
    )
  )
}

server <- function(input, output, session) {
  t <- reactive(seq(0, input$length, length = input$length * 100))
  x <- reactive(sin(input$omega * t() + input$delta) * input$damping ^ t())
  y <- reactive(sin(t()) * input$damping ^ t())
  
  output$fig <- renderPlot({
    plot(x(), y(), axes = F, xlab = "", ylab = "", type = "l", lwd  = 2)
  })
}

shinyApp(ui, server, enableBookmarking = "server")