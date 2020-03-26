# Chapter 12 - Tidy Evaluations
library(ggplot2)
library(shiny)
library(dplyr, warn.conflicts = FALSE)

diamonds %>% filter (x == z)
ggplot(diamonds, aes(carat, price)) + geom_hex()

iris %>% select(starts_with("sepal"))
diamonds %>% select(is.numeric)

## 12.1 - Data-Masking

df <- data.frame(x = runif(3), y = runif(3))
df$x

filter(diamonds, x == 0 | y == 0)

diamonds[diamonds$x == 0 | diamonds$y == 0, ]

### 12.1.1 - Indirection

min_carat <- 1
diamonds %>% filter(carat > min_carat)

var <- "carat"
min <- 1
diamonds[diamonds[[var]] > min, ]

diamonds %>% filter(.data$carat > min)

var <- "carat"
diamonds %>% filter(.data[[var]] > min)

ui <- fluidPage(
  selectInput("var", "Variable", choices = names(diamonds)),
  numericInput("min", "Minimum", value = 1),
  tableOutput("output")
)
server <- function(input, output, session) {
  data <- reactive(filter(diamonds, .data[[input$var]] > input$min))
  output$output <- renderTable(head(data()))
}

shinyApp(ui, server)

### 12.1.2 Example: ggplot2

ui <- fluidPage(
  selectInput("x", "X Variable", choices = names(iris)),
  selectInput("y", "Y Variable", choices = names(iris)),
  plotOutput("plot")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(iris, aes(.data[[input$x]], .data[[input$y]])) +
      geom_point(position = ggforce::position_auto())
  })
}
shinyApp(ui, server)

# ============================================================

ui <- fluidPage(
  selectInput("x", "X variable", choices = names(iris)),
  selectInput("y", "Y variable", choices = names(iris)),
  selectInput("geom", "geom", c("point", "smooth", "jitter")),
  plotOutput("plot")
)

server <- function(input, output, session) {
  plot_geom <- reactive({
    switch(input$geom,
           point = geom_point(),
           smooth = geom_smooth(se = FALSE),
           jitter = geom_jitter()
           )
  })
  
  output$plot <- renderPlot({
    ggplot(iris, aes(.data[[input$x]], .data[[input$y]])) +
      plot_geom()
  })
}

shinyApp(ui, server)

### 12.1.3 - Example: dplyr

ui <- fluidPage(
  selectInput("var", "Select Variable", choices = names(mtcars)),
  sliderInput("min", "Minimum value", 0, min = 0, max = 100),
  selectInput("sort", "Sort by", choices = names(mtcars)),
  tableOutput("data")
)

server <- function(input, output, session) {
  observeEvent(input$var, {
    rng <- range(mtcars[[input$var]])
    updateSliderInput(session, "min", value = rng[[1]], min = rng[[1]], max = rng[[2]])
  })
  
  output$data <- renderTable({
    mtcars %>%
      filter(.data[[input$var]] > input$min) %>%
      arrange(.data[[input$sort]])
  })
}
shinyApp(ui, server)

# ============================================================

ui <- fluidPage(
  selectInput("var", "Sort by", choices = names(mtcars)),
  checkboxInput("desc", "Descending order?"),
  tableOutput("data")
)

server <- function(input, output, session) {
  sorted <- reactive({
    if(input$desc) {
      arrange(mtcars, desc(.data[[input$var]]))
    } else {
      arrange(mtcars, .data[[input$var]])
    }
  })
  output$data <- renderTable(sorted())
}

shinyApp(ui, server)

### 12.1.4 - User Supplied Data

ui <- fluidPage(
  fileInput("data", "dataset", accept = ".tsv"),
  selectInput("var", "var", character()),
  numericInput("min", "min", 1, min = 0, step = 1),
  tableOutput("output")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$data)
    vroom::vroom(input$datapath)
  })
  observeEvent(data(), {
    updateSelectInput(session, "var", choices = names(data()))
  })
  observeEvent(input$var, {
    val <- data()[[input$var]]
    updateNumericInput(session, "min", value = min(val))
  })
    
    output$output <- renderTable({
      req(input$var)
      
      data() %>%
        filter(.data[[input$var]] > input$min) %>%
        arrange(.data[[input$var]]) %>%
        head(10)
    })
}

shinyApp(ui, server)

# ============================================================

df <- data.frame(x = 1, y = 2)
input <- list(var = "x", min = 0)

df %>% filter(.data[[input$var]] > input$min)


df <- data.frame(x = 1, y = 2, input = 3)
df %>% filter(.data[[input$var]] > input$min)

df$input$min

df %>% filter(.data[[input$var]] > .env$input$min)

### 12.1.5 - Why not use base R?

df[df[[input$var]] > input$min, ]
