# Shiny app scaffold for FI and EDA pages

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

get_data <- function() {
  path <- Sys.getenv("BZ_DATA_XLSX", unset = "data/raw/data.xlsx")
  if (file.exists(path)) readxl::read_excel(path) else tibble::tibble()
}

ui <- navbarPage(
  "BZ-Data",
  tabPanel(
    "FI Analysis",
    sidebarLayout(
      sidebarPanel(
        uiOutput("inst_select")
      ),
      mainPanel(
        plotOutput("fi_plot")
      )
    )
  ),
  tabPanel(
    "EDA",
    sidebarLayout(
      sidebarPanel(
        uiOutput("mod_select")
      ),
      mainPanel(
        plotOutput("eda_hist"),
        plotOutput("eda_ts")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    df <- get_data()
    if (nrow(df) > 0 && "date" %in% names(df)) df$date <- as.Date(df$date)
    df
  })

  output$inst_select <- renderUI({
    df <- data()
    inst <- sort(unique(df$institution))
    selectInput("institution", "Institution", choices = inst, selected = if (length(inst)>0) inst[1] else NULL)
  })

  output$fi_plot <- renderPlot({
    req(input$institution)
    df <- data()
    df <- df %>% filter(institution == input$institution)
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(date, value, color = modality)) + geom_line() + labs(x = NULL, y = NULL)
  })

  output$mod_select <- renderUI({
    df <- data()
    mods <- sort(unique(df$modality))
    selectInput("modality", "Modality", choices = mods, selected = if (length(mods)>0) mods[1] else NULL)
  })

  output$eda_hist <- renderPlot({
    req(input$modality)
    df <- data() %>% filter(modality == input$modality)
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(value)) + geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7)
  })

  output$eda_ts <- renderPlot({
    req(input$modality)
    df <- data() %>% filter(modality == input$modality)
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(date, value)) + geom_line(color = "tomato") + labs(x = NULL, y = NULL)
  })
}

shinyApp(ui, server)

