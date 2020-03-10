# Chapter 11 - Functions

library(shiny)

### 11.2.2 - Case Study

fluidRow(
  box(
    width = 4,
    solidHeader = TRUE,
    selectInput("traffickingType",
                label = "Choose a trafficking type: ",
                choices = sort(unique(ngo$Trafficking.Type)),
                multiple = TRUE
    )
  ),
  box(
    width = 4,
    solidHeader = TRUE,
    selectInput("traffickingSubType",
                label = "Choose a trafficking sub type: ",
                choices = sort(unique(ngo$Trafficking.Sub.Type)),
                multiple = TRUE
    )
  ),
  box(
    width = 4,
    solidHeader = TRUE,
    selectInput("gender",
                label = "Choose a gender: ",
                choices = sort(unique(ngo$Victim.Gender)),
                multiple = TRUE
    )
  )
)

#============================================================

# Convert to function

ngoSelectInput <- function(var, label, multiple = T) {
  choices <- sort(unique(ngo[[var]]))
  label <- paste0("Choose a ", label, ": ")
  selectInput(var, label, choices = choices, multiple = multiple)
}
boxHeader <- function(...) {
  box(width = 4, solidHeader = T, ...)
}

fluidRow(
  boxHeader(ngoSelectInput("Trafficking.Type", "trafficking type")),
  boxHeader(ngoSelectInput("Trafficking.Sub.Type", "trafficking sub type")),
  boxHeader(ngoSelectInput("Victim.Gender", "gender"))
)

ngo_filtered <- reactive({
  filter(ngo,
         Trafficking.Type %in% input$Trafficking.Type,
         Trafficking.Sub.Type %in% input$Trafficking.Sub.Type,
         Victim.Gender %in% input$Victim.Gender
         )
})

## 11.4 - Limitations of Functions

library(lubridate)

library(shiny)

ui <- fluidPage(
  textInput("date", "When were you born? (yyyy-mm-dd)"),
  textOutput("error"),
  textOutput("age")
)

server <- function(input, output, session) {
  birthday <- reactive({
    req(input$date)
    ymd(input$date, quiet = T)
  })
  
  output$error <- renderText({
    if (is.na(birthday())) {
      "Please enter a valid date in yyyy-mm-dd form."
    }
  })
  
  age <- reactive({
    req(birthday())
    (birthday() %--% today() %/% years(1))
  })
  output$age <- renderText({
    paste0("You are ", age(), " Years Old")
  })
}

shinyApp(ui, server)
shinyApp(ui, server)

#============================================================

ymdInputUI <- function(label) {
  label <- paste0(label, " (yyyy-mm-dd)")
  
  fluidRow(
    textInput("date", label),
    textOutput("error")
  )
}

ymdInputServer <- function(input, output, session) {
  date <- reactive({
    req(input$date)
    ymd(input$date, quiet = T)
  })
  
  output$error <- renderText({
    if (is.na(date())) {
      "Please enter a valid date in yyyy-mm-dd form."
    }
  })

date

}

ui <- fluidPage(
  ymdInputUI("When were you born?"),
  textOutput("age")
)

server <- function(input, output, session) {
  birthday <- ymdInputServer(input, output, session)
  
  age <- reactive({
    req(birthday())
    (birthday() %--% today()) %/% years(1)
  })
  
  output$age <- renderText({
    paste0("You are ", age(), " years old")
  })
}

shinyApp(ui, server)

# Why is the UI broken on this app? It is aligned to the left margin as opposed to being responsive.
