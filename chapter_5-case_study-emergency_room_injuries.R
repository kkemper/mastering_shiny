# Chapter 5 - Case Study: Emergency Room Injuries
library(shiny)
library(vroom)
library(tidyverse)

## 5.2 - The Data

injuries <- vroom::vroom("neiss/injuries.tsv")
injuries

products <- vroom::vroom("https://raw.githubusercontent.com/hadley/mastering-shiny/master/neiss/products.tsv")
products

population <- vroom::vroom("https://raw.githubusercontent.com/hadley/mastering-shiny/master/neiss/population.tsv")
population

## 5.3 - Exploration

selected <- injuries %>% filter(prod_code == 1842)
nrow(selected)

selected %>% count(diag, wt = weight, sort = TRUE)
selected %>% count(body_part, wt = weight, sort = TRUE)
selected %>% count(location, wt = weight, sort = TRUE)

summary <- selected %>%
  count(age, sex, wt = weight)
summary %>%
  ggplot(aes(age, n, color = sex)) +
  geom_line() +
labs(y = "Estimated Number of Injuries")

summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n / population * 1e4)
summary

summary %>%
  ggplot(aes(age, rate, color = sex)) +
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 People")

selected %>%
  sample_n(10) %>%
  pull(narrative)

## 5.4 - Prototype

ui <- fluidPage(
  fluidRow(
    column(6, selectInput("code", "Product", setNames(products$prod_code, products$title))
           )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )
  
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, color = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries") +
      theme_grey(15)
  })
}

shinyApp(ui, server)

## 5.5 - Polish Tables

injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  group_by(diag) %>%
  summarize(n = as.integer(sum(weight)))

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{var}} := fct_lump(fct_infreq({{var}}), n = n)) %>%
    group_by({{var}}) %>%
    summarize(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(6, selectInput("code", "Product", setNames(products$prod_code, products$title))
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
output$diag <- renderTable(count_top(selected(), diag), width = "100%")
output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, color = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries") +
      theme_grey(15)
  })
}
shinyApp(ui, server)

## 5.6 - Rate vs. Count

ui <- fluidPage(
  fluidRow(
    column(8, selectInput("code", "Product", choices = setNames(products$prod_code, products$title), width = "100%")
  ),
  column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if(input$y == "count"){
      summary() %>%
        ggplot(aes(age, n, color = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_gray(15)
    } else {
    summary() %>%
      ggplot(aes(age, rate, color = sex)) +
      geom_line(na.rm = TRUE) +
      labs(y = "Injuries per 10,000 people") +
      theme_grey(15)
    }
  })
}
shinyApp(ui, server)

## 5.7 - Narrative

ui <- fluidPage(
  fluidRow(
    column(8, selectInput("code", "Product", choices = setNames(products$prod_code, products$title), width = "100%")
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if(input$y == "count"){
      summary() %>%
        ggplot(aes(age, n, color = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_gray(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, color = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_grey(15)
    }
  })
  
  output$narrative <- renderText({
    input$story
    selected() %>% pull(narrative) %>% sample(1)
  })
}
shinyApp(ui, server)

## 5.8 - Exercises


### 2.

ui <- fluidPage(
  fluidRow(
    column(8, selectInput("code", "Product", choices = setNames(products$prod_code, products$title), width = "100%")
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if(input$y == "count"){
      summary() %>%
        ggplot(aes(age, n, color = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_gray(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, color = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_grey(15)
    }
  })
  
  output$narrative <- renderText({
    input$story
    selected() %>% pull(narrative) %>% sample(1)
  })
}
shinyApp(ui, server)