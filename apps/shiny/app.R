# BZ-Data Advanced Shiny Dashboard
# Interactive FX Modality Analytics with Advanced Features

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(shinycssloaders)
library(shinyWidgets)

# Source utility functions
source("../../R/data_loader.R", local = TRUE)
source("../../R/plotting.R", local = TRUE)
source("../../R/utils.R", local = TRUE)
source("../../R/financial_analytics.R", local = TRUE)
source("../../R/forecasting.R", local = TRUE)
source("../../R/advanced_plotting.R", local = TRUE)
source("../../R/data_generator.R", local = TRUE)

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

  # Advanced Analytics Tab
  nav_panel(
    "Advanced Analytics",
    layout_sidebar(
      sidebar = sidebar(
        title = "Analytics Settings",
        selectInput(
          "analytics_type",
          "Analysis Type",
          choices = c(
            "Risk Analysis" = "risk",
            "Forecasting" = "forecast",
            "Anomaly Detection" = "anomaly",
            "Correlation Analysis" = "correlation"
          ),
          selected = "risk"
        ),
        conditionalPanel(
          condition = "input.analytics_type == 'risk'",
          numericInput("var_confidence", "VaR Confidence Level", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
          selectInput("var_method", "VaR Method",
                     choices = c("Historical" = "historical", "Parametric" = "parametric"),
                     selected = "historical")
        ),
        conditionalPanel(
          condition = "input.analytics_type == 'forecast'",
          numericInput("forecast_periods", "Forecast Periods", value = 6, min = 1, max = 24),
          selectInput("forecast_method", "Forecast Method",
                     choices = c("Ensemble" = "ensemble", "Moving Average" = "ma",
                               "Exponential Smoothing" = "es", "Linear Trend" = "trend"),
                     selected = "ensemble")
        ),
        conditionalPanel(
          condition = "input.analytics_type == 'anomaly'",
          selectInput("anomaly_method", "Detection Method",
                     choices = c("Z-Score" = "zscore", "IQR" = "iqr", "Modified Z-Score" = "modified_zscore"),
                     selected = "zscore"),
          numericInput("anomaly_threshold", "Threshold", value = 2.5, min = 1, max = 5, step = 0.1)
        ),
        hr(),
        downloadButton("download_analytics", "Download Results", class = "btn-primary")
      ),
      card(
        card_header("Advanced Analytics Results"),
        withSpinner(plotlyOutput("analytics_plot", height = "500px"))
      ),
      layout_columns(
        card(
          card_header("Metrics Summary"),
          withSpinner(DTOutput("analytics_table"))
        ),
        card(
          card_header("Statistical Tests"),
          withSpinner(verbatimTextOutput("analytics_stats"))
        )
      )
    )
  ),

  # Data Generator Tab
  nav_panel(
    "Data Generator",
    layout_sidebar(
      sidebar = sidebar(
        title = "Generate Synthetic Data",
        dateRangeInput(
          "gen_date_range",
          "Date Range",
          start = Sys.Date() - 365,
          end = Sys.Date() + 365
        ),
        checkboxGroupInput(
          "gen_institutions",
          "Institutions",
          choices = c("Bank_Alpha", "Bank_Beta", "Bank_Gamma", "Bank_Delta"),
          selected = c("Bank_Alpha", "Bank_Beta", "Bank_Gamma")
        ),
        sliderInput("gen_volatility", "Volatility", min = 0.05, max = 0.5, value = 0.15, step = 0.01),
        sliderInput("gen_trend", "Trend Strength", min = -0.2, max = 0.2, value = 0.05, step = 0.01),
        checkboxInput("gen_seasonality", "Include Seasonality", value = TRUE),
        checkboxInput("gen_stress", "Add Stress Scenario", value = FALSE),
        conditionalPanel(
          condition = "input.gen_stress == true",
          dateRangeInput("stress_period", "Stress Period",
                        start = Sys.Date() - 90, end = Sys.Date() - 30),
          sliderInput("stress_magnitude", "Stress Magnitude", min = 0.1, max = 0.8, value = 0.3, step = 0.05)
        ),
        hr(),
        actionButton("generate_data", "Generate Data", class = "btn-success"),
        br(), br(),
        downloadButton("download_generated", "Download Generated Data", class = "btn-primary")
      ),
      card(
        card_header("Generated Data Preview"),
        withSpinner(plotlyOutput("generated_plot", height = "400px"))
      ),
      card(
        card_header("Data Summary"),
        withSpinner(DTOutput("generated_summary"))
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
      ),
      value_box(
        title = "Avg Daily VaR",
        value = textOutput("avg_var"),
        showcase = icon("exclamation-triangle"),
        theme = "warning"
      )
    ),
    layout_columns(
      card(
        card_header("Volume Heatmap"),
        withSpinner(plotOutput("overview_heatmap", height = "400px"))
      ),
      card(
        card_header("Risk Metrics"),
        withSpinner(plotlyOutput("overview_risk", height = "400px"))
      )
    ),
    card(
      card_header("Complete Dataset"),
      withSpinner(DTOutput("overview_table"))
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

  # Generated data reactive
  generated_data <- reactiveVal(NULL)

  # Generate synthetic data
  observeEvent(input$generate_data, {
    req(input$gen_date_range, input$gen_institutions)

    withProgress(message = "Generating synthetic data...", {
      new_data <- generate_fx_data(
        start_date = input$gen_date_range[1],
        end_date = input$gen_date_range[2],
        institutions = input$gen_institutions,
        volatility = input$gen_volatility,
        trend_strength = input$gen_trend,
        seasonality = input$gen_seasonality
      )

      if (input$gen_stress) {
        new_data <- generate_stress_scenario(
          new_data,
          stress_start = input$stress_period[1],
          stress_end = input$stress_period[2],
          stress_magnitude = input$stress_magnitude
        )
      }

      generated_data(new_data)
    })
  })

  # Advanced Analytics Outputs
  output$analytics_plot <- renderPlotly({
    req(input$analytics_type)
    df <- data()

    if (input$analytics_type == "risk") {
      # Risk analysis
      risk_data <- df %>%
        arrange(institution, date) %>%
        group_by(institution, date) %>%
        summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(institution, date) %>%
        group_by(institution) %>%
        mutate(
          returns = (total_value - lag(total_value)) / lag(total_value),
          rolling_vol = calculate_rolling_volatility(total_value, window = 3, annualize = FALSE)
        ) %>%
        filter(!is.na(rolling_vol))

      p <- ggplot(risk_data, aes(x = date, y = rolling_vol, color = institution)) +
        geom_line(linewidth = 1) +
        labs(title = "Rolling Volatility Analysis", x = "Date", y = "Volatility") +
        theme_minimal()

      ggplotly(p)

    } else if (input$analytics_type == "forecast") {
      # Forecasting
      forecast_data <- df %>%
        group_by(date) %>%
        summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(date) %>%
        pull(total_value)

      forecast_result <- switch(input$forecast_method,
        "ensemble" = forecast_ensemble(forecast_data, input$forecast_periods),
        "ma" = forecast_moving_average(forecast_data, input$forecast_periods),
        "es" = forecast_exponential_smoothing(forecast_data, input$forecast_periods),
        "trend" = forecast_linear_trend(forecast_data, input$forecast_periods)
      )

      plot_forecast(forecast_data, forecast_result) %>% ggplotly()

    } else if (input$analytics_type == "anomaly") {
      # Anomaly detection
      anomaly_data <- df %>%
        group_by(date) %>%
        summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(date) %>%
        mutate(
          anomaly = detect_anomalies(total_value, method = input$anomaly_method,
                                   threshold = input$anomaly_threshold)
        )

      p <- plot_anomalies(anomaly_data %>% rename(time = date),
                         title = paste("Anomaly Detection -", input$anomaly_method))
      ggplotly(p)

    } else if (input$analytics_type == "correlation") {
      # Correlation analysis
      corr_data <- df %>%
        select(date, institution, value) %>%
        pivot_wider(names_from = institution, values_from = value, values_fill = 0)

      if (ncol(corr_data) > 2) {
        corr_matrix <- cor(corr_data[, -1], use = "complete.obs")
        plot_correlation_heatmap(corr_matrix) %>% ggplotly()
      } else {
        plotly_empty() %>% layout(title = "Not enough institutions for correlation analysis")
      }
    }
  })

  output$analytics_table <- renderDT({
    req(input$analytics_type)
    df <- data()

    if (input$analytics_type == "risk") {
      # Risk metrics table
      risk_metrics <- df %>%
        arrange(institution, date) %>%
        group_by(institution, date) %>%
        summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(institution, date) %>%
        group_by(institution) %>%
        mutate(returns = (total_value - lag(total_value)) / lag(total_value)) %>%
        filter(!is.na(returns)) %>%
        summarize(
          var_95 = calculate_var(returns, input$var_confidence, input$var_method),
          expected_shortfall = calculate_expected_shortfall(returns, input$var_confidence),
          volatility = sd(returns, na.rm = TRUE),
          sharpe_ratio = calculate_sharpe_ratio(returns),
          .groups = "drop"
        )

      datatable(risk_metrics, options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
        formatRound(c("var_95", "expected_shortfall", "volatility", "sharpe_ratio"), digits = 4)
    } else {
      datatable(data.frame(Message = "Select Risk Analysis to see metrics"),
               options = list(dom = 't'), rownames = FALSE)
    }
  })

  output$analytics_stats <- renderText({
    req(input$analytics_type)

    if (input$analytics_type == "risk") {
      paste("Risk Analysis Summary:\n",
            "- VaR Confidence Level:", input$var_confidence, "\n",
            "- Method:", input$var_method, "\n",
            "- Analysis covers all institutions and modalities")
    } else if (input$analytics_type == "forecast") {
      paste("Forecast Analysis Summary:\n",
            "- Forecast Periods:", input$forecast_periods, "\n",
            "- Method:", input$forecast_method, "\n",
            "- Confidence intervals included")
    } else if (input$analytics_type == "anomaly") {
      paste("Anomaly Detection Summary:\n",
            "- Method:", input$anomaly_method, "\n",
            "- Threshold:", input$anomaly_threshold, "\n",
            "- Applied to total daily volumes")
    } else if (input$analytics_type == "correlation") {
      paste("Correlation Analysis Summary:\n",
            "- Method: Pearson correlation\n",
            "- Applied to institution volumes\n",
            "- Hierarchical clustering applied")
    }
  })

  # Data Generator Outputs
  output$generated_plot <- renderPlotly({
    req(generated_data())

    p <- plot_modality_timeseries(generated_data(),
                                 title = "Generated Synthetic Data",
                                 interactive = FALSE)
    ggplotly(p)
  })

  output$generated_summary <- renderDT({
    req(generated_data())

    summary <- generated_data() %>%
      group_by(institution, modality) %>%
      summarize(
        records = n(),
        total_volume = sum(value, na.rm = TRUE),
        avg_value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )

    datatable(summary, options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatRound(c("total_volume", "avg_value"), digits = 0)
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

  output$avg_var <- renderText({
    df <- data()
    returns_data <- df %>%
      group_by(date) %>%
      summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(date) %>%
      mutate(returns = (total_value - lag(total_value)) / lag(total_value)) %>%
      filter(!is.na(returns))

    if (nrow(returns_data) > 0) {
      var_val <- calculate_var(returns_data$returns, 0.95, "historical")
      paste0(round(abs(var_val) * 100, 2), "%")
    } else {
      "N/A"
    }
  })

  output$overview_heatmap <- renderPlot({
    plot_heatmap(data(), title = "Total FX Volume by Institution and Modality")
  })

  output$overview_risk <- renderPlotly({
    df <- data()
    risk_data <- df %>%
      arrange(institution, date) %>%
      group_by(institution, date) %>%
      summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(institution, date) %>%
      group_by(institution) %>%
      mutate(
        returns = (total_value - lag(total_value)) / lag(total_value),
        rolling_vol = calculate_rolling_volatility(total_value, window = 3, annualize = FALSE)
      ) %>%
      filter(!is.na(rolling_vol))

    p <- ggplot(risk_data, aes(x = date, y = rolling_vol, color = institution)) +
      geom_line(linewidth = 1) +
      labs(title = "Risk Overview", x = "Date", y = "Volatility") +
      theme_minimal()

    ggplotly(p)
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

  output$download_analytics <- downloadHandler(
    filename = function() {
      paste0("analytics_", input$analytics_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- data()

      if (input$analytics_type == "risk") {
        risk_metrics <- df %>%
          arrange(institution, date) %>%
          group_by(institution, date) %>%
          summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          arrange(institution, date) %>%
          group_by(institution) %>%
          mutate(returns = (total_value - lag(total_value)) / lag(total_value)) %>%
          filter(!is.na(returns)) %>%
          summarize(
            var_95 = calculate_var(returns, input$var_confidence, input$var_method),
            expected_shortfall = calculate_expected_shortfall(returns, input$var_confidence),
            volatility = sd(returns, na.rm = TRUE),
            sharpe_ratio = calculate_sharpe_ratio(returns),
            .groups = "drop"
          )
        write.csv(risk_metrics, file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )

  output$download_generated <- downloadHandler(
    filename = function() {
      paste0("generated_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(generated_data())
      write.csv(generated_data(), file, row.names = FALSE)
    }
  )
}

# Run App
shinyApp(ui, server)

