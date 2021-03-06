# Chapter 13 - Shiny Modules

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

### 13.2.1 - Module UI

histogramUI <- function(id) {
  list(
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

### 13.2.2 -  Module Server

histogramServer <- function(id) {
  moduleServer(id, function(input, output, server) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    })
  })
}

# 13.2.3 - Updated App

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

moduleServer <- function(id, module) {
  callModule(module, id)
}

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

### 13.2.4 - Namespacing
library(shiny)
ui <- fluidPage(
  histogramUI("hist1"),
  textOutput("out")
)

server <- function(input, output, session) {
  histogramServer("hist1")
  output$out <- renderText(paste0("Bins: ", input$bins))
}

shinyApp(ui, server)

#============================================================

# Updated app

# shim until Shiny 1.5.0
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module ui
histogramUI <- function(id) {
  list(
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

# module server
histogramServer <- function(id) {
  moduleServer(id, function(input, output, server) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    })
  })
}

# generate app
ui <- fluidPage(
  histogramUI("hist1"),
  textOutput("out")
)

server <- function(input, output, session) {
  histogramServer("hist1")
  output$out <- renderText(paste0("Bins: ", input$bins))
}

shinyApp(ui, server)

### 13.3.1 - Getting Started: UI Input + Server Output

# shim until Shiny 1.5.0
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module ui
datasetInput <- function(id, filter = NULL) {
  names <- ls("package:datasets")
  if (!is.null(filter)) {
    data <- lapply(names, get, "package:datasets")
    names <- names[vapply(data, filter, logical(1))]
  }
  
  selectInput(NS(id, "dataset"), "Pick a dataset", names)
}

# module server
datasetServer <- function(id) {
  moduleServer(id, function(input, output, server) {
    reactive(get(input$dataset, "package:datasets"))
  })
}

# module function
datasetModule <- function(filter = NULL) {
  ui <- fluidPage(
    datasetInput("dataset", filter = filter),
    tableOutput("data")
  )
  server <- function(input, output, session){
    data <- datasetServer("dataset")
    output$data <- renderTable(head(data()))
  }
  shinyApp(ui, server)
}

datasetModule(is.data.frame)

### 13.3.2 - Case Study: Numeric Variable Selector

# shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module ui
selectNumericVarInput <- function(id) {
  selectInput(NS(id, "var"), "Variable", choices = NULL)
}

# module server
selectNumericVarServer <- function(id, data) {
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      is_numeric <- vapply(data(), is.numeric, logical(1))
      updateSelectInput(session, "var", choices = names(data())[is_numeric])
    })
    
    reactive(data()[[input$var]])
  })
}

#module function

selectNumericVarModule <- function() {
  ui <- fluidPage(
    datasetInput("data", is.data.frame),
    selectNumericVarInput("var"),
    verbatimTextOutput("out")
  )
  
  server <- function(input, output, session) {
    data <- datasetServer("data")
    var <- selectNumericVarServer("var", data)
    output$out <- renderPrint(var())
}

shinyApp(ui, server)
}

selectNumericVarModule()

### 13.3.3 - Server Inputs

### 13.3.4 - Case Study: Histogram

# shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module ui
histogramOutput <- function(id) {
  list(
    numericInput(NS(id, "bins"), "bins", 10, min = 1, step = 1),
    plotOutput(NS(id, "hist"))
  )
}

# module server
histogramServer <- function(id, x) {
  stopifnot(is.reactive(x))
  
  moduleServer(id, function(input, output, session){
    output$hist <- renderPlot(
      hist(x(), breaks = input$bins, main = NULL)
    )
  })
}

# module function
histogramModule <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        datasetInput("data", is.data.frame),
        selectNumericVarInput("var"),
      ),
      mainPanel(
        histogramOutput("hist")
      )
    )
  )
  
  server <- function(input, output, session) {
    data <- datasetServer("data")
    x <- selectNumericVarServer("var", data)
    histogramServer("hist", x)
  }
  shinyApp(ui, server)
}
histogramModule()

 # 13.3.5 - Multiple Outputs

selectNumericVarServer <- function(id, data) {
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      is_numeric <- vapply(data(), is.numeric, logical(1))
      updateSelectInput(session, "var", choices = names(data())[is_numeric])
    })
    
    list(
      name = reactive(input$var),
      value = reactive(data()[[input$var]])
    )
  })
}

# module server
histogramServer <- function(id, x, title = reactive(NULL)) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(title))
  
  moduleServer(id, function(input, output, session) {
    output$hist <- renderPlot(
      hist(x(), breaks = input$bins, main = title())
    )
  })
}

# module function
histogramModule <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        datasetInput("data", is.data.frame),
        selectNumericVarInput("var"),
      ),
      mainPanel(
        histogramOutput("hist")
      )
    )
  )
  
  server <- function(input, output, session) {
    data <- datasetServer("data")
    x <- selectNumericVarServer("var", data)
    histogramServer("hist", x$value, x$name)
  }
  shinyApp(ui, server)
}
histogramModule()

#13.3.6 - Case Study: Tip Calculator

# shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

inlineNumericInput <- function(id, value) {
  tags$input(id = id, type = "number", value = value, style = "width: 3em", step = 1)
}

# module UI
tipUI <- function(id, value) {
  tags$p(
    textOutput(NS(id, "total"), inline = TRUE),
    " * ",
    inlineNumericInput(NS(id, "percent_tip"), value = value),
    "% = ",
    textOutput(NS(id, "tip"), inline = TRUE)
  )
}

# module server
tipServer <- function(id, total) {
  stopifnot(is.reactive(total))
  dollar <- function(x) sprintf("$%0.2f", x)
  
  moduleServer(id, function(input, output, session) {
    output$total <- renderText(dollar(total()))
    output$tip <- renderText(dollar(input$percent_tip / 100 * total()))
  })
}

# module function
tipModule <- function() {
  ui <- fluidPage(
    numericInput("bill", "Total bill", value = 10),
    tipUI("tip1", value = 10),
    tipUI("tip2", value = 20)
  )
  
  server <- function(input, output, session) {
    tipServer("tip1", reactive(input$bill))
    tipServer("tip2", reactive(input$bill))
  }
  
  shinyApp(ui, server)
}

tipModule()

### 13.3.7 - Case Study - Summary

#shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module ui
summaryOutput <- function(id) {
  tags$ul(
    tags$li("Min: ", textOutput(NS(id, "min"), inline = TRUE)),
    tags$li("Max: ", textOutput(NS(id, "max"), inline = TRUE)),
    tags$li("Missing: ", textOutput(NS(id, "n_na"), inline = TRUE))
  )
}

#module server
summaryServer <- function(id, var) {
  moduleServer(id, function(input, output, session) {
    rng <- reactive({
      req(var(), na.rm = TRUE)
    })
    
    output$min <- renderText(rng()[[1]])
    output$max <- renderText(rng()[[2]])
    output$n_na <- renderText(sum(is.na(var())))
  })
}

# module function
summaryModule <- function() {
  ui <- fluidPage(
    datasetInput("data", is.data.frame),
    selectNumericVarInput("var"),
    summaryOutput("summary")
)
  
  server <- function(input, output, session) {
    df <- datasetServer("data")
    var <- selectNumericVarServer("var", df)
    summaryServer("summary", var)
  }

shinyApp(ui, server)
}
summaryModule()

## 13.4 - Reusable  Components

### 13.4.1 - Date with Error
library(shiny)
# shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module ui
ymdDateInput <- function(id, label) {
  label <- paste0(label, " (yyyy-mm-dd)")
  
  fluidRow(
    textInput(NS(id, "date"), label),
    textOutput(NS(id, "error"))
  )
}

# module server
ymdDateServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    date <- reactive({
      req(input$date)
      ymd(input$date, quiet = TRUE)
    })
    
    output$error <- renderText({
      if (is.na(date())) {
        "Please enter valid date in yyyy-mm-dd form"
      }
    })
    
    date
  })
}

# module function
ymdDateModule <- function(label) {
  ui <- fluidPage(
    ymdDateInput("ymd", label),
    textOutput("date")
  )
  server <- function(input, output, session) {
    date <- ymdDateServer("ymd")
    output$date <- renderText(date)
  }
}

server <- function(input, output, session) {
  birthday <- ymdDate("birthday")
  
  age <- reactive({
    req(birthday())
    (birthday() %--% today()) %/% years(1)
  })
  
  output$age <- renderText({
    paste0("You are ", age(), "yearsold")
  })
}

ymdDateModule()

# THIS EXAMPLE DOES NOT SEEM TO WORK

#  13.4.2 - Limited Selection + Other

#ui
ui <- fluidPage(
  radioButtons("gender", "Gender:",
               choiceValues = list("male", "female", "self-described", "na"),
               choiceNames = list(
                 "Male",
                 "Female",
                 textInput("gender_self", NULL, placeholder = "Self-described"),
                 "Prefer not to say"
               ),
               selected = "na",
  ),
  textOutput("txt")
)

# server
server <- function(input, output, session) {
  observeEvent(input$gender_self, {
    req(input$gender_self)
    updateRadioButtons(session, "gender", selected = "self_described")
  })
  gender <- reactive({
    if(input$gender == "self-described") {
      input$gender_self
    } else {
      input$gender
    }
  })
  
  output$txt <- renderText({
    paste("You chose", gender())
  })
}
shinyApp(ui, server)

# =============================================================

# Convert to module

# shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module ui
radioButtonsExtraUI <- function(id, label, choices, selected = NULL, placeholder = NULL) {
  ns <- NS(id)
  
  radioButtons(ns("primary"), "Gender:",
               choiceValues = c(names(choices), "other"),
               choiceNames = c(
                 unname(choices),
                 list(textInput(ns("other"), NULL, placeholder = NULL))
               ),
               selected = selected
               )
}

# module server
radioButtonsExtraServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$primary, {
      req(input$other)
      updateRadioButtons(session, "primary", selected = "other")
    })
    
    reactive({
      if(input$primary == "other") {
        input$other
      } else {
        input$primary
      }
    })
  })
}

# ui
ui <- fluidPage(
  radioButtonsExtraUI("gender",
                      label = "Gender",
                      choices = list(
                        male = "Male",
                        female = "Female",
                        na = "Prefer not to say"
                      ),
                      placeholder = "Self-described",
                      selected = "na"
  ),
  textOutput("txt")
)

# server
server <- function(input, output, session) {
  gender <- radioButtonsExtraServer("gender")
  
  output$txt <- renderText({
    paste("You chose", gender())
  })
}

shinyApp(ui, server)

# 13.5 - Single Object Modules

# shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

# module UI
histogramUI <- function(id, df) {
  list(
    selectInput(NS(id, "var"), "Variable", names(df)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

# module server

histogramServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(df[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    })
  })
}

# ui
ui <- fluidPage (
  tabsetPanel(
    tabPanel("mtcars", histogramUI("mtcars", mtcars)),
    tabPanel("iris", histogramUI("iris", iris))
  )
)

# server
serve <- function(input, output, session) {
  histogramServer("mtcars", mtcars)
  histogramServer("iris", iris)
}

shinyApp(ui, server)

## 13.6 Exercises

### 13.6.1
# The following app plots user selected variables from the msleep dataset for three different types of mammals (carnivores, omnivores, and herbivores), with one tab for each type of mammal. Remove the redundancy in the selectInput() definitions with the use of functions.

library(tidyverse)

ui <- fluidPage(
  selectInput(inputId = "x",
              label = "X-axis:",
              choices = c("sleep_total", "sleep_rem", "sleep_cycle", 
                          "awake", "brainwt", "bodywt"),
              selected = "sleep_rem"),
  selectInput(inputId = "y",
              label = "Y-axis:",
              choices = c("sleep_total", "sleep_rem", "sleep_cycle", 
                          "awake", "brainwt", "bodywt"),
              selected = "sleep_total"),
  tabsetPanel(id = "vore",
              tabPanel("Carnivore",
                       plotOutput("plot_carni")),
              tabPanel("Omnivore",
                       plotOutput("plot_omni")),
              tabPanel("Herbivore",
                       plotOutput("plot_herbi")))
)

server <- function(input, output, session) {
  
  # make subsets
  carni <- reactive( filter(msleep, vore == "carni") )
  omni  <- reactive( filter(msleep, vore == "omni")  )
  herbi <- reactive( filter(msleep, vore == "herbi") )
  
  # make plots
  output$plot_carni <- renderPlot({
    ggplot(data = carni(), aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  output$plot_omni <- renderPlot({
    ggplot(data = omni(), aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  output$plot_herbi <- renderPlot({
    ggplot(data = herbi(), aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
}

shinyApp(ui = ui, server = server)

# ============================================================

sliderInput01 <- function(id, label, selected) {
  selectInput(inputId = id, label,
              choices = c("sleep_total", "sleep_rem", "sleep_cycle", 
                          "awake", "brainwt", "bodywt"),
              selected)
}

tabsetPanel01 <- function(label2, graph) {
  tabPanel(label2,
           plotOutput(graph))
}

# ui
ui <- fluidRow(
  sliderInput01("x", "X-axis", "sleep_rem"),
  sliderInput01("y", "Y-axis", "sleep_total"),
  tabsetPanel(id = "vore",
              tabPanel01("Carnivore", "plot_carni"),
              tabPanel01("Omnivore", "plot_omni"),
              tabPanel01("Herbivore", "plot_herbi")
  )
)

# server
server <- function(input, output, session) {
  
  # make subsets
  carni <- reactive( filter(msleep, vore == "carni") )
  omni  <- reactive( filter(msleep, vore == "omni")  )
  herbi <- reactive( filter(msleep, vore == "herbi") )
  
  # make plots
  output$plot_carni <- renderPlot({
    ggplot(data = carni(), aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  output$plot_omni <- renderPlot({
    ggplot(data = omni(), aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  output$plot_herbi <- renderPlot({
    ggplot(data = herbi(), aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
}

shinyApp(ui = ui, server = server)

### 13.6.2 - TO BE COMPLETED
# Continue working with the same app from the previous exercise, and further remove redundancy in the code by modularizing how subsets and plots are created.

# shim
moduleServer <- function(id, module) {
  callModule(module, id)
}

# 
sliderInput01 <- function(id, label, selected) {
  selectInput(inputId = id, label,
              choices = c("sleep_total", "sleep_rem", "sleep_cycle", 
                          "awake", "brainwt", "bodywt"),
              selected)
}

tabsetPanel01 <- function(label2, graph) {
  tabPanel(label2,
           plotOutput(graph))
}

# ui
ui <- fluidRow(
  sliderInput01("x", "X-axis", "sleep_rem"),
  sliderInput01("y", "Y-axis", "sleep_total"),
  tabsetPanel(id = "vore",
              tabPanel01("Carnivore", "plot_carni"),
              tabPanel01("Omnivore", "plot_omni"),
              tabPanel01("Herbivore", "plot_herbi")
  )
)

filter01 <- function(vore){
  reactive( filter(msleep, vore == vore))
}

# ============================================================



### 13.6.5 - TO BE COMPLETED
# The following module input provides a text control that lets you type a date in ISO8601 format (yyyy-mm-dd). Complete the module by providing a server function that uses the “error” output to display a message if the entered value is not a valid date. You can use strptime(x, "%Y-%m-%d") to parse the string; it will return NA if the value isn’t a valid date.

ymdDateInput <- function(id, label) {
  label <- paste0(label, " (yyyy-mm-dd)")
  
  fluidRow(
    textInput(NS(id, "date"), label),
    textOutput(NS(id, "error"))
  )
}

### 13.6.6 - TO BE COMPLETED
#The following code defines output and server components of a module that takes a numeric input and produces a bulleted list of three summary statistics. Create an app function that allows you to experiment with it. The app function should take a data frame as input, and use numericVarSelectInput() to pick the variable to summarise.

summaryOuput <- function(id) {
  tags$ul(
    tags$li("Min: ", textOutput(NS(id, "min"), inline = TRUE)),
    tags$li("Max: ", textOutput(NS(id, "max"), inline = TRUE)),
    tags$li("Missing: ", textOutput(NS(id, "n_na"), inline = TRUE))
  )
}

summaryServer <- function(id, var) {
  moduleServer(id, function(input, output, session) {
    rng <- reactive({
      req(var())
      range(var(), na.rm = TRUE)
    })
    
    output$min <- renderText(rng()[[1]])
    output$max <- renderText(rng()[[2]])
    output$n_na <- renderText(sum(is.na(var())))
  })
}