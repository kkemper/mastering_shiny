library(shiny)

# Chapter 8 - Dynamic UI

## 8.1 - Updating Inputs

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

### 8.1.1 - Simple Uses

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

### 8.1.2 - Hierarchical Select Boxes

sales <- vroom::vroom("sales-dashboard/sales_data_sample.csv", col_types = list())
sales

ui <- fluidPage(
  selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
  selectInput("customername", "Customer", choices = NULL),
  selectInput("ordernumber", "Order Number", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session){
  territory <- reactive({
    req(input$territory)
    if (input$territory == "NA") {
      filter(sales, is.na(TERRITORY))
    } else {
    filter(sales, TERRITORY == input$territory)
    }
  })
  observeEvent(territory(), {
    choices <- unique(territory()$CUSTOMERNAME)
    updateSelectInput(session, "customername", choices = choices)
  })
  
  customer <- reactive({
    req(input$customername)
    filter(territory(), CUSTOMERNAME == input$customername)
  })
  observeEvent(customer(), {
    choices <- unique(customer()$ORDERNUMBER)
    updateSelectInput(session, "ordernumber", choices = choices)
  })
  
  output$data <- renderTable({
    req(input$ordernumber)
    customer() %>%
      filter(ORDERNUMBER == input$ordernumber) %>%
      select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
  })
}

shinyApp(ui, server)

#----------------------------------------------------

### 8.1.3 - Circular References

### 8.1.4 - Inter-related Inputs

### 8.1.5 - Exercises

#### 8.1.5.1
library(shiny)
ui <- fluidPage(
  numericInput("year", "Year", value = 2020),
  dateInput("date", "Date")
)

server <- function(input, output, session) {
  observe({
    yr <- input$year
    
    updateDateInput(session, "date", 
                    min = paste(yr, "-01-01", sep = ""),
                    max = paste(yr, "-12-31", sep = "")
                    )
  })
  }

shinyApp(ui, server)

#### 8.1.5.2

library(openintro)

states <- unique(county$state)

ui <- fluidPage(
  selectInput("state", "State", choices = states),
  selectInput("county", "County", choices = NULL)
)

server <- function(input, output, session){
  st <- reactive({
    filter(county, state == input$state)
  })
 observeEvent(st(), {
   choices <- st()$name
   updateSelectInput(session, "county", choices = choices)
 }) 
}
shinyApp(ui, server)

#### 8.1.5.3

library(gapminder)
continents <- unique(gapminder$continent)

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents),
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session){
  cont <- reactive({
    filter(gapminder, continent == input$continent)
  })
  observeEvent(cont(), {
    choices <- cont()$country
    updateSelectInput(session, "country", choices = choices)
  }) 
  
  output$data <- renderTable({
    req(input$country)
    cont() %>%
      filter(country == input$country)
  })
}
shinyApp(ui, server)

#### 8.1.5.4
##### TO BE COMPLETED

#### 8.1.5.5
##### TO BE COMPLETED

## 8.2 - Dynamic Visibility
                                                                                                                                                                  library(shiny)
                                                                                                                                                                  ui <- fluidPage(
                                                                                                                                                                    tags$style("#switcher {display:none; }"),
                                                                                                                                                                    sidebarLayout((
                                                                                                                                                                      sidebarPanel(
                                                                                                                                                                        selectInput("controller", "Show", choices = paste0("panel", 1:3))
                                                                                                                                                                                    )
                                                                                                                                                                      ),
                                                                                                                                                                      mainPanel(
                                                                                                                                                                        tabsetPanel(
                                                                                                                                                                          id = "switcher",
                                                                                                                                                                          tabPanel("panel1", "Panel 1 content"),
                                                                                                                                                                          tabPanel("panel2", "Panel 2 content"),
                                                                                                                                                                          tabPanel("panel3", "Panel 3 content")
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  
                                                                                                                                                                  server <- function(input, output, session){
                                                                                                                                                                    observeEvent(input$controller, {
                                                                                                                                                                      updateTabsetPanel(session, "switcher", selected = input$controller)
                                                                                                                                                                    })
                                                                                                                                                                  }
                                                                                                                                                                  
                                                                                                                                                                  shinyApp(ui, server)

### 8.2.1  - Conditional UI

parameter_tabs <- tagList(
  tags$style("#params {display:none; }"),
  tabsetPanel(id = "params",
              tabPanel("normal",
                       numericInput("mean", "mean", value = 1),
                       numericInput("sd", "standard deviation", min = 0, value = 1)
                       ),
              tabPanel("uniform",
                       numericInput("min", "min", value = 0),
                       numericInput("max", "max", value = 1)
                       ),
              tabPanel("exponential",
                       numericInput("rate", "rate", value = 1, min = 0),
                       )
              )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution",
                  choices = c("normal", "uniform", "exponential")
      ),
      numericInput("n", "Number of Samples", value = 100),
      parameter_tabs,
    ),
    mainPanel(plotOutput("hist")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(session, "params", selected = input$dist)
  })
  
  sample <- reactive({
  switch(input$dist,
         normal = rnorm(input$n, input$mean, input$sd),
         uniform = runif(input$n, input$min, input$max),
         exponential = rexp(input$n, input$rate)
         )
})
output$hist <- renderPlot(hist(sample()))
}

shinyApp(ui, server)

### 8.2.2 - Wizard Interface

library(shiny)

ui <- fluidPage(
  tags$style("wizard { display:none; }"),
  tabsetPanel(id = "wizard", 
              tabPanel("page1", "Welcome!",
                       actionButton("page12", "next")
                       ),
              tabPanel("page2", "Only one page to go",
                       actionButton("page21", "prev"),
                       actionButton("page23", "next")
                       ),
              tabPanel("page3",
                       "You're done!",
                       actionButton("page32", "prev")
                       )
              )
)

server <- function(input, output, session) {
  switch_tab <- function(page) {
    updateTabsetPanel(session, "wizard", selected = page)
  }
  
  observeEvent(input$page12, switch_tab("page2"))
  observeEvent(input$page21, switch_tab("page1"))
  observeEvent(input$page23, switch_tab("page3"))
  observeEvent(input$page32, switch_tab("page2"))
}

shinyApp(ui, server)

### 8.2.3 - Exercises

#### TO BE ADDED IN BOOK

## 8.3 Dialog Boxes

## 8.4 - Creating UI with Code

library(shiny)

ui <- fluidPage(
  textInput("label", "label"),
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
)

server <- function(input, output, session) {
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("dynamic", input$label, value = isolate(input$dynamic), min = 0, max = 10)
    } else {
      numericInput("dynamic", input$label, value = isolate(input$dynamic), min = 0, max = 10)
    }
  })
}

shinyApp(ui, server)

### 8.4.1 - Multiple Controls

library(shiny)
library(tidyverse)

ui <- fluidPage(
  numericInput("n", "Number of Colors", value = 5, min = 1),
  uiOutput("col"),
  textOutput("palette")
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))
  
  output$col <- renderUI({
    map(col_names(), ~textInput(.x, NULL))
  })
  
  output$palette <- renderText({
  map_chr(col_names(), ~input[[.x]])
  })
}

shinyApp(ui, server)


#=============================================

library(shiny)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of Colors", value = 5, min = 1),
      uiOutput("col"),
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))
  
  output$col <- renderUI({
    map(col_names(), ~textInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
  })
  
  output$plot <- renderPlot({
    cols <- map_chr(col_names(), ~ input[[.x]])
    cols[cols == ""] <- NA
    
    barplot(
      rep(1, length(cols)),
      col = cols,
      space  = 0,
      axes = F
    )
  })
}

shinyApp(ui, server)

### 8.4.2 - Dynamic Filtering

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
  # Not supported
  NULL
  }
}

filter_var <- function(x, val) {
  if(is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      make_ui(iris$Sepal.Length, "Sepal.Length"),
      make_ui(iris$Sepal.Width, "Sepal.Width"),
      make_ui(iris$Species, "Species")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive({
    filter_var(iris$Sepal.Length, input$Sepal.Length) &
      filter_var(iris$Sepal.Width, input$Sepal.Width) &
      filter_var(iris$Species, input$Species)
  })
  
  output$data <- renderTable(head(iris[selected(), ], 12))
}

shinyApp(ui, server)


#================================================================
# Test Code

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        make_ui(iris$Sepal.Length, "Sepal.Length"),
        make_ui(iris$Sepal.Width, "Sepal.Width"),
        make_ui(iris$Species, "Species")
      ),
      mainPanel(
        tableOutput("data")
      )
    )
  )
  server <- function(input, output, session) {
    selected <- reactive({
      filter_var(iris$Sepal.Length, input$Sepal.Length) &
        filter_var(iris$Sepal.Width, input$Sepal.Width) &
        filter_var(iris$Species, input$Species)
    })
    
    output$data <- renderTable(head(iris[selected(), ], 12))
  }
  
  shinyApp(ui, server)
#===============================================================
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(iris), ~ make_ui(iris[[.x]], .x))
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive({
    each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
    reduce(each_var, `&`)
  })
  
  output$data <- renderTable(head(iris[selected(), ], 12))
}

shinyApp(ui, server)

#=================================================================
dfs <- keep(ls("package:datasets"), ~ is.data.frame(get(.x, "package:datasets")))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Dataset", choices = dfs),
      uiOutput("filter")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    get(input$dataset, "package:datasets")
  })
  vars <- reactive(names(data()))
  
  output$filter <- renderUI(
    map(vars(), ~make_ui(data()[[.x]], .x))
  )
  
  selected <- reactive({
    each_var <- map(vars(), ~ filter_var(data()[[.x]], input[[.x]]))
    reduce(each_var, `&`)
  })
  
  output$data <- renderTable(head(data()[selected(), ], 12))
}

shinyApp(ui, server)