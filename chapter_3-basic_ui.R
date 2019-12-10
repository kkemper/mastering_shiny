# Chapter 3 -Basic UI

library(shiny)

## 3.2 - Inputs

### 3.2.2 - Free Text

ui <- fluidPage(
  textInput("name", "What's Your Name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3)
)

server <- function(input, output, session){
}

shinyApp(ui, server)

### 3.2.3 Numeric Inputs

library(shiny)
ui <- fluidPage(
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100)
)

server <- function(input, output, session){
}

shinyApp(ui, server)

### 3.2.4 - Dates

ui <- fluidPage(
  dateInput("dob", "When were you brn?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?")
)

server <- function(input, output, session){
}

shinyApp(ui, server)

### 3.2.5 - Limited Choices

animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

ui <- fluidPage(
  selectInput("state", "What's your favorite state?", state.name),
  radioButtons("animal", "What's your favorite animal?", animals)
)

server <- function(input, output, session){
}

shinyApp(ui, server)

# ===============================

ui <- fluidPage(
  radioButtons("rb", "Choose one:",
               choiceNames = list(
                 icon("angry"),
                 icon("smile"),
                 icon("sad-tear")
               ),
               choiceValues = list("angry", "happy", "sad")
  )
)

server <- function(input, output, session){
}

shinyApp(ui, server)

# ================================

ui <- fluidPage(
  selectInput(
    "state", "What's your favorite state?", state.name, multiple = T
  )
)

server <- function(input, output, session){
}

shinyApp(ui, server)

# ===============================

ui <- fluidPage(
  checkboxGroupInput("animal", "What animal do you like?", animals)
)

server <- function(input, output, session){
}

shinyApp(ui, server)

# ===============================

ui <- fluidPage(
  checkboxInput("cleanup", "Clean Up?", value = T),
  checkboxInput("shutdown", "Shutdown?")
)

server <- function(input, output, session){
}

shinyApp(ui, server)

### 3.2.6 - File Uploads

ui <- fluidPage(
  fileInput("upload", NULL)
)

server <- function(input, output, session){
}

shinyApp(ui, server)


## 3.2.7 - Action Buttons
library(shiny)
ui <- fluidPage(
  actionButton("click", "Click me!"),
  actionButton("drink", "Drink me!", icon = icon("cocktail"))
)

server <- function(input, output, session){
}

shinyApp(ui, server)

### 3.2.8 - Exercises

#### 1.

ui <- fluidPage(
  textInput("name", placeholder = "Your name")
)

server <- function(input, output, session){
}

shinyApp(ui, server)

#### 2.

library(shiny)

ui <- fluidPage(
  sliderInput("date", "When should we deliver?", min = as.Date("2019-08-09"), max = as.Date("2019-08-16"), value = as.Date("2019-08-10"))
)
server <- function(input, output, session) {
  
}

shinyApp(ui, server)

#### 3.

shinyApp(
  ui = fluidPage(
    selectInput("state", "Choose a state:",
                list(`East Coast` = list("NY", "NJ", "CT"),
                     `West Coast` = list("WA", "OR", "CA"),
                     `Midwest` = list("MN", "WI", "IA"))
    ),
    textOutput("result")
  ),
  server = function(input, output) {
    output$result <- renderText({
      paste("You chose", input$state)
    })
  }
)



#### 4.

library(shiny)

ui <- fluidPage(
  sliderInput("number", "Choose a number", min = 0, max = 100, value = 0, step = 5, animate = T)
)
  animationOptions(interval = 100, loop = FALSE, playButton = T,
                 pauseButton = NULL)
server <- function(input, output, session) {
  
}

shinyApp(ui, server)

#### 5.

library(shiny)

ui <- fluidPage(
  numericInput("number", "Select a value", value = 150, min = 0, max = 1000, step = 50)
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

# The step function increases the value in the numericSelect box, by an interval of 50 each time the selector is clicked up or down.

## 3.3 - Outputs

### 3.3.1 Text

library(shiny)

ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
)

server <- function(input, output, session) {
  output$text <- renderText({
    "Hello, friend!"
  })
  output$code <- renderPrint({
    summary(1:10)
  })
}

shinyApp(ui, server)

# ==========

ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
)

server <- function(input, output, session) {
  output$text <- renderText("Hello, friend!")
  output$code <- renderPrint(summary(1:10))
}

shinyApp(ui, server)

### 3.3.2 - Tables

library(shiny)

ui <- fluidPage(
  tableOutput("static"),
  dataTableOutput("dynamic")
)

server <- function(input, output, session) {
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
}

shinyApp(ui, server)

### 3.3.3 - Plots

library(shiny)

ui <- fluidPage(
  plotOutput("plot", width = "400px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5))
}

shinyApp(ui, server)

### 3.3.5 Exercises

#### 1.

ui <- fluidPage(
  plotOutput("plot", width = "700px", height ="300px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5))
}

shinyApp(ui, server)

#### 2.

ui <- fluidPage(
  plotOutput("plot", width = "auto",inline = TRUE),
  plotOutput("plot2", width = "auto", inline = TRUE)
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5))
  output$plot2 <- renderPlot(plot(6:10))
}

shinyApp(ui, server)

#### 3.

ui <- fluidPage(
  dataTableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderTable(mtcars, options = list(pageLength = 5))
}

theme_demo <- function(theme) {
  fluidPage(
    theme = shinythemes::shinytheme(theme),
    sidebarLayout(
      sidebarPanel(
        textInput("txt", "Text Input:", "text here"),
        sliderInput("slider", "Slider input:", 1, 100, 30)
      ),
      mainPanel(
        h1("Header 1"),
        h2("Header 2"),
        p("Some text")
      )
    )
  )
}

theme_demo("darkly")
theme_demo("flatly")
theme_demo("sandstone")
theme_demo("united")

server <- function(input, output, session){
  
}

shinyApp(ui, server)

# 3.4.6 - Exercises

#### 1.

ui <- fluidPage(
  headerPanel("Central limit theorem"),
  sidebarLayout(
    mainPanel(
      plotOutput("hist")
      ),
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
    )
  )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  })
}
shinyApp(ui, server)

#### 2.

ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  headerPanel("Central limit theorem"),
  sidebarLayout(
    mainPanel(
      plotOutput("hist")
    ),
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
    )
  )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  })
}
shinyApp(ui, server)

## 3.5 Under the Hood

