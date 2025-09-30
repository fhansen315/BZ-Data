#' Create time series plot for FX modalities
#'
#' @param df Data frame with date, modality, value columns
#' @param title Plot title
#' @param interactive Use plotly for interactivity (default: TRUE)
#' @return ggplot2 or plotly object
#' @export
plot_modality_timeseries <- function(df, title = "FX Modalities Over Time", interactive = TRUE) {
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, color = modality)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2, alpha = 0.6) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Value",
      color = "Modality"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma)
  
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create histogram for a single modality
#'
#' @param df Data frame with value column
#' @param title Plot title
#' @param bins Number of bins (default: 30)
#' @return ggplot2 object
#' @export
plot_modality_histogram <- function(df, title = "Distribution", bins = 30) {
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = bins, fill = "steelblue", alpha = 0.7, color = "white") +
    ggplot2::labs(
      title = title,
      x = "Value",
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14)) +
    ggplot2::scale_x_continuous(labels = scales::comma)
}

#' Create boxplot comparing modalities
#'
#' @param df Data frame with modality and value columns
#' @param title Plot title
#' @return ggplot2 object
#' @export
plot_modality_boxplot <- function(df, title = "Modality Comparison") {
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  ggplot2::ggplot(df, ggplot2::aes(x = modality, y = value, fill = modality)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::labs(
      title = title,
      x = "Modality",
      y = "Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma)
}

#' Create summary statistics table
#'
#' @param df Data frame
#' @param group_by Column name(s) to group by
#' @return Summary tibble
#' @export
summarize_data <- function(df, group_by = c("institution", "modality")) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
    dplyr::summarize(
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      total = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total))
}

#' Create heatmap of values by institution and modality
#'
#' @param df Data frame
#' @param title Plot title
#' @return ggplot2 object
#' @export
plot_heatmap <- function(df, title = "FX Activity Heatmap") {
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  summary_df <- df %>%
    dplyr::group_by(institution, modality) %>%
    dplyr::summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  ggplot2::ggplot(summary_df, ggplot2::aes(x = modality, y = institution, fill = total_value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(total_value)), color = "white", fontface = "bold") +
    ggplot2::scale_fill_gradient(low = "steelblue", high = "darkred", labels = scales::comma) +
    ggplot2::labs(
      title = title,
      x = "Modality",
      y = "Institution",
      fill = "Total Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

