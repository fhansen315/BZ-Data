# BZ-Data Shiny Dashboard
# Interactive FX Modality Analytics

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

# Source utility functions
source("../../R/data_loader.R", local = TRUE)
source("../../R/plotting.R", local = TRUE)
source("../../R/utils.R", local = TRUE)

# UI Definition
ui <- page_navbar(
  title = "BZ-Data Analytics",
  theme = bs_theme(
    bootswatch = "cosmo",
    primary = "#0066CC",
    base_font = font_google("Inter")
  ),

  # FI Analysis Tab
  nav_panel(
    "FI Analysis",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        selectInput(
          "institution",
          "Select Institution",
          choices = NULL,
          selected = NULL
        ),
        dateRangeInput(
          "date_range_fi",
          "Date Range",
          start = NULL,
          end = NULL
        ),
        checkboxGroupInput(
          "modalities_fi",
          "Modalities",
          choices = NULL,
          selected = NULL
        ),
        hr(),
        downloadButton("download_fi", "Download Data", class = "btn-primary")
      ),
      card(
        card_header("Time Series Analysis"),
        plotlyOutput("fi_plot", height = "400px")
      ),
      layout_columns(
        card(
          card_header("Summary Statistics"),
          DTOutput("fi_summary")
        ),
        card(
          card_header("Distribution"),
          plotOutput("fi_boxplot", height = "300px")
        )
      )
    )
  ),

  # EDA Tab
  nav_panel(
    "EDA",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        selectInput(
          "modality_eda",
          "Select Modality",
          choices = NULL,
          selected = NULL
        ),
        dateRangeInput(
          "date_range_eda",
          "Date Range",
          start = NULL,
          end = NULL
        ),
        checkboxGroupInput(
          "institutions_eda",
          "Institutions",
          choices = NULL,
          selected = NULL
        ),
        hr(),
        downloadButton("download_eda", "Download Data", class = "btn-primary")
      ),
      card(
        card_header("Time Series by Institution"),
        plotlyOutput("eda_ts", height = "400px")
      ),
      layout_columns(
        card(
          card_header("Distribution"),
          plotOutput("eda_hist", height = "300px")
        ),
        card(
          card_header("Statistics"),
          DTOutput("eda_stats")
        )
      )
    )
  ),

  # Overview Tab
  nav_panel(
    "Overview",
    layout_columns(
      value_box(
        title = "Total Records",
        value = textOutput("total_records"),
        showcase = icon("database"),
        theme = "primary"
      ),
      value_box(
        title = "Institutions",
        value = textOutput("total_institutions"),
        showcase = icon("building"),
        theme = "info"
      ),
      value_box(
        title = "Total Volume",
        value = textOutput("total_volume"),
        showcase = icon("chart-line"),
        theme = "success"
      )
    ),
    card(
      card_header("Volume Heatmap"),
      plotOutput("overview_heatmap", height = "400px")
    ),
    card(
      card_header("Complete Dataset"),
      DTOutput("overview_table")
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Load data
  data <- reactive({
    load_fx_data()
  })

  # Initialize filters
  observe({
    df <- data()
    institutions <- get_institutions(df)
    modalities <- get_modalities(df)

    updateSelectInput(session, "institution", choices = institutions, selected = institutions[1])
    updateSelectInput(session, "modality_eda", choices = modalities, selected = modalities[1])

    updateCheckboxGroupInput(session, "modalities_fi", choices = modalities, selected = modalities)
    updateCheckboxGroupInput(session, "institutions_eda", choices = institutions, selected = institutions)

    updateDateRangeInput(session, "date_range_fi", start = min(df$date), end = max(df$date))
    updateDateRangeInput(session, "date_range_eda", start = min(df$date), end = max(df$date))
  })

  # Filtered data for FI Analysis
  fi_data <- reactive({
    req(input$institution, input$modalities_fi)
    df <- data()
    df <- filter_by_institution(df, input$institution)
    df <- filter_by_modality(df, input$modalities_fi)
    df <- filter_by_date_range(df, input$date_range_fi[1], input$date_range_fi[2])
    df
  })

  # Filtered data for EDA
  eda_data <- reactive({
    req(input$modality_eda, input$institutions_eda)
    df <- data()
    df <- filter_by_modality(df, input$modality_eda)
    df <- filter_by_institution(df, input$institutions_eda)
    df <- filter_by_date_range(df, input$date_range_eda[1], input$date_range_eda[2])
    df
  })

  # FI Analysis Outputs
  output$fi_plot <- renderPlotly({
    plot_modality_timeseries(fi_data(), title = paste("FX Modalities -", input$institution), interactive = TRUE)
  })

  output$fi_summary <- renderDT({
    summary <- summarize_data(fi_data(), group_by = "modality")
    datatable(summary, options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatRound(c("mean", "median", "sd", "min", "max", "total"), digits = 0)
  })

  output$fi_boxplot <- renderPlot({
    plot_modality_boxplot(fi_data(), title = "Modality Comparison")
  })

  # EDA Outputs
  output$eda_ts <- renderPlotly({
    df <- eda_data() %>%
      group_by(date, institution) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

    p <- ggplot(df, aes(x = date, y = value, color = institution)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2, alpha = 0.6) +
      labs(title = paste(input$modality_eda, "- Time Series"), x = NULL, y = "Value") +
      theme_minimal() +
      scale_y_continuous(labels = comma)

    ggplotly(p)
  })

  output$eda_hist <- renderPlot({
    plot_modality_histogram(eda_data(), title = paste(input$modality_eda, "Distribution"), bins = 30)
  })

  output$eda_stats <- renderDT({
    stats <- eda_data() %>%
      group_by(institution) %>%
      summarize(
        n = n(),
        total = sum(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total))

    datatable(stats, options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatRound(c("total", "mean", "median"), digits = 0)
  })

  # Overview Outputs
  output$total_records <- renderText({
    format_number(nrow(data()))
  })

  output$total_institutions <- renderText({
    length(get_institutions(data()))
  })

  output$total_volume <- renderText({
    format_currency(sum(data()$value, na.rm = TRUE))
  })

  output$overview_heatmap <- renderPlot({
    plot_heatmap(data(), title = "Total FX Volume by Institution and Modality")
  })

  output$overview_table <- renderDT({
    datatable(
      data(),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = 'top',
      rownames = FALSE
    ) %>%
      formatCurrency("value", digits = 0) %>%
      formatDate("date", method = "toLocaleDateString")
  })

  # Download Handlers
  output$download_fi <- downloadHandler(
    filename = function() {
      paste0("fi_analysis_", input$institution, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(fi_data(), file, row.names = FALSE)
    }
  )

  output$download_eda <- downloadHandler(
    filename = function() {
      paste0("eda_", input$modality_eda, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(eda_data(), file, row.names = FALSE)
    }
  )
}

# Run App
shinyApp(ui, server)

