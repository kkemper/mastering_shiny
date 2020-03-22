# Chapter 12 - Shiny Modules

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

### 12.2.1 - Module UI

histogramUI <- function(id) {
  list(
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

### 12.2.2 -  Module Server

histogramServer <- function(id) {
  moduleServer(id, function(input, output, server) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    })
  })
}

# 12.2.3 - Updated App

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

### 12.2.4 - Namespacing
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

### 12.3.1 - Getting Started: UI Input + Server Output

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

### 12.3.2 - Case Study: Numeric Variable Selector

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

### 12.3.3 - Server Inputs

### 12.3.4 - Case Study: Histogram

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

 # 12.3.5 - Multiple Outputs

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

#12.3.6 - Case Study: Tip Calculator

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

### 12.3.7 - Case Study - Summary

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

## 12.4 - Reusable  Components

### 12.4.1 - Date with Error
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

# Limited Selection + Other