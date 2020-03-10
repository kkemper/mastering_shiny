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