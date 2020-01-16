# Chapter 8 - Uploads and Downloads

library(shiny)

### 8.1.2 - Server
ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("files")
)

server <- function(input, output, session) {
  output$files <- renderTable(input$upload)
}

shinyApp(ui, server)

###  8.1.3 Uploading Data

ui <- fluidPage(
fileInput("file", NULL, accept = c(".csv", ".tsv")),
numericInput("n", "Rows", value = 5, min = 1, step = 1),
tableOutput("head")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; please upload a .csv or .tsv file")
           )
  })
  
  output$head <- renderTable({
    head(data()< input$n)
  })
}

shinyApp(ui, server)

## 8.2 - Download

### 8.2.2 - Downloading Data

ui <- fluidPage(
  selectInput("dataset", "Pick a dataset", ls("package:datasets")),
  tableOutput("preview"),
  downloadButton("download", "Download .tsv")
)

server <- function(input, output, session) {
  data <- reactive({
    out <- get(input$dataset, "package:datasets")
    if(!is.data.frame(out)) {
      validate(paste0("'", input$dataset, "' is not a data frame"))
    }
    out
  })
  
  output$preview <- renderTable({
    head(data())
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data(), file)
    }
  )
}

shinyApp(ui, server)

### 8.2.3 - Downloading Reports

ui <- fluidPage(
sliderInput("n", "Number of Points", 1, 100, 50),
downloadButton("report", "Generate Report")
)

server <- function(input, output, session) {
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      params <- list(n = input$n)
      
      rmarkdown::render("report.Rmd",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
    }
  )
}

shinyApp(ui, server)

#======================

## 8.3 - Case Study
library(shiny)
ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file", "Data", buttonLabel = "Upload..."),
    textInput("delim", "Delimiter (leave blank to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows", "Rows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Raw Data"),
    tableOutput("preview1")
  )
)

ui_clean <- sidebarLayout(
  sidebarPanel(
    checkboxInput("snake", "Rename columnds to snake case"),
    checkboxInput("constant", "Remove constant columns"),
    checkboxInput("empty", "Remove empty columns")
  ),
  mainPanel(
    h3("Clean Data"),
    tableOutput("preview2")
  )
)

ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block"))
)

ui <- fluidPage(
  ui_upload,
  ui_clean,
  ui_download
)

server <- function(input, output, session) {
  raw <- reactive({
    req(input$file)
    delim <- if (input$delim == "") NULL else input$delim
    vroom::vroom(input$file$datapath, delim = delim, skip = input$skip)
  })
  output$preview1 <- renderTable(head(raw(), input$rows))
    
  tidied <- reactive({
    out <- raw()
    if(input$snake) {
      names(out) <- janitor::make_clean_names(names(out))
    }
    if(input$empty) {
      out <- janitor::remove_empty(out, "cols")
    }
    if (input$constant) {
      out <- janitor::remove_constant(out)
    }
    
    out
  })
  output$preview2 <- renderTable(head(tidied(), input$rows))

  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(tidied(), file)
    }
  )
}

shinyApp(ui, server)