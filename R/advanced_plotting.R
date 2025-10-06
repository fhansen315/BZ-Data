#' Advanced Plotting Functions for Financial Analytics
#'
#' This module provides sophisticated visualization functions including
#' 3D plots, network graphs, and advanced statistical visualizations.

#' Create 3D surface plot for risk analysis
#'
#' @param data Data frame with x, y, z columns
#' @param title Plot title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param z_label Z-axis label
#' @return Plotly 3D surface plot
#' @export
plot_3d_surface <- function(data, title = "3D Risk Surface", 
                           x_label = "Time", y_label = "Institution", z_label = "Risk") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for 3D plots")
  }
  
  # Create matrix for surface plot
  x_vals <- unique(data[[1]])
  y_vals <- unique(data[[2]])
  z_matrix <- matrix(data[[3]], nrow = length(x_vals), ncol = length(y_vals))
  
  plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    z = z_matrix,
    type = "surface",
    colorscale = "Viridis"
  ) %>%
    plotly::layout(
      title = title,
      scene = list(
        xaxis = list(title = x_label),
        yaxis = list(title = y_label),
        zaxis = list(title = z_label)
      )
    )
}

#' Create network graph for institution relationships
#'
#' @param correlation_matrix Correlation matrix between institutions
#' @param threshold Minimum correlation to show edge
#' @param title Plot title
#' @return Network graph plot
#' @export
plot_network_graph <- function(correlation_matrix, threshold = 0.5, title = "Institution Network") {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    warning("igraph package not available, returning correlation heatmap instead")
    return(plot_correlation_heatmap(correlation_matrix, title))
  }
  
  # Create adjacency matrix
  adj_matrix <- abs(correlation_matrix) > threshold
  diag(adj_matrix) <- FALSE
  
  # Create graph
  graph <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
  
  # Set edge weights
  igraph::E(graph)$weight <- correlation_matrix[adj_matrix]
  
  # Plot
  plot(graph,
       vertex.color = "lightblue",
       vertex.size = 20,
       vertex.label.color = "black",
       edge.width = abs(igraph::E(graph)$weight) * 5,
       edge.color = ifelse(igraph::E(graph)$weight > 0, "blue", "red"),
       main = title)
}

#' Create correlation heatmap with hierarchical clustering
#'
#' @param correlation_matrix Correlation matrix
#' @param title Plot title
#' @param cluster Whether to apply hierarchical clustering
#' @return ggplot2 heatmap
#' @export
plot_correlation_heatmap <- function(correlation_matrix, title = "Correlation Heatmap", cluster = TRUE) {
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 package is required for heatmaps")
  }
  
  # Apply clustering if requested
  if (cluster && nrow(correlation_matrix) > 2) {
    hc <- hclust(as.dist(1 - abs(correlation_matrix)))
    correlation_matrix <- correlation_matrix[hc$order, hc$order]
  }
  
  # Melt correlation matrix
  melted_corr <- reshape2::melt(correlation_matrix)
  
  ggplot2::ggplot(melted_corr, ggplot2::aes(Var1, Var2, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), color = "white", fontface = "bold") +
    ggplot2::scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                                 midpoint = 0, limit = c(-1, 1), space = "Lab",
                                 name = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL)
}

#' Create Sankey diagram for flow analysis
#'
#' @param flow_data Data frame with source, target, value columns
#' @param title Plot title
#' @return Plotly Sankey diagram
#' @export
plot_sankey_diagram <- function(flow_data, title = "Flow Analysis") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for Sankey diagrams")
  }
  
  # Get unique nodes
  nodes <- unique(c(flow_data$source, flow_data$target))
  
  # Create node indices
  flow_data$source_idx <- match(flow_data$source, nodes) - 1
  flow_data$target_idx <- match(flow_data$target, nodes) - 1
  
  plotly::plot_ly(
    type = "sankey",
    node = list(
      label = nodes,
      color = "blue",
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = flow_data$source_idx,
      target = flow_data$target_idx,
      value = flow_data$value
    )
  ) %>%
    plotly::layout(title = title, font = list(size = 10))
}

#' Create risk dashboard with multiple metrics
#'
#' @param data Data frame with risk metrics
#' @param title Dashboard title
#' @return Combined plot
#' @export
plot_risk_dashboard <- function(data, title = "Risk Dashboard") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork package is required for dashboard layout")
  }
  
  # VaR plot
  p1 <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = var)) +
    ggplot2::geom_line(color = "red", linewidth = 1) +
    ggplot2::geom_area(alpha = 0.3, fill = "red") +
    ggplot2::labs(title = "Value at Risk", x = NULL, y = "VaR") +
    ggplot2::theme_minimal()
  
  # Volatility plot
  p2 <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = volatility)) +
    ggplot2::geom_line(color = "orange", linewidth = 1) +
    ggplot2::labs(title = "Rolling Volatility", x = NULL, y = "Volatility") +
    ggplot2::theme_minimal()
  
  # Sharpe ratio plot
  p3 <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = sharpe_ratio)) +
    ggplot2::geom_line(color = "green", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
    ggplot2::labs(title = "Sharpe Ratio", x = "Date", y = "Sharpe Ratio") +
    ggplot2::theme_minimal()
  
  # Drawdown plot
  p4 <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = drawdown)) +
    ggplot2::geom_area(fill = "darkred", alpha = 0.7) +
    ggplot2::labs(title = "Drawdown", x = "Date", y = "Drawdown") +
    ggplot2::theme_minimal()
  
  # Combine plots
  (p1 | p2) / (p3 | p4) +
    patchwork::plot_annotation(title = title, theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold")))
}

#' Create forecast visualization with confidence intervals
#'
#' @param historical Historical data
#' @param forecast_result Forecast result from forecasting functions
#' @param title Plot title
#' @return ggplot2 forecast plot
#' @export
plot_forecast <- function(historical, forecast_result, title = "Forecast Analysis") {
  # Prepare data
  n_hist <- length(historical)
  n_forecast <- length(forecast_result$forecast)
  
  # Create time indices
  hist_time <- 1:n_hist
  forecast_time <- (n_hist + 1):(n_hist + n_forecast)
  
  # Combine data
  plot_data <- data.frame(
    time = c(hist_time, forecast_time),
    value = c(historical, forecast_result$forecast),
    type = c(rep("Historical", n_hist), rep("Forecast", n_forecast)),
    lower = c(rep(NA, n_hist), forecast_result$lower_ci),
    upper = c(rep(NA, n_hist), forecast_result$upper_ci)
  )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(ggplot2::aes(color = type), linewidth = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), 
                        alpha = 0.3, fill = "blue", na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("Historical" = "black", "Forecast" = "blue")) +
    ggplot2::labs(title = paste(title, "-", forecast_result$method), 
                 x = "Time", y = "Value", color = "Type") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
}

#' Create anomaly detection plot
#'
#' @param data Data frame with time, value, and anomaly columns
#' @param title Plot title
#' @return ggplot2 anomaly plot
#' @export
plot_anomalies <- function(data, title = "Anomaly Detection") {
  ggplot2::ggplot(data, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(color = "gray", alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(color = anomaly), size = 2) +
    ggplot2::scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                               labels = c("Normal", "Anomaly")) +
    ggplot2::labs(title = title, x = "Time", y = "Value", color = "Status") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
}

#' Create portfolio optimization efficient frontier
#'
#' @param returns Matrix of asset returns
#' @param title Plot title
#' @return ggplot2 efficient frontier plot
#' @export
plot_efficient_frontier <- function(returns, title = "Efficient Frontier") {
  if (ncol(returns) < 2) {
    stop("Need at least 2 assets for efficient frontier")
  }
  
  # Calculate mean returns and covariance matrix
  mean_returns <- colMeans(returns, na.rm = TRUE)
  cov_matrix <- cov(returns, use = "complete.obs")
  
  # Generate random portfolios
  n_portfolios <- 1000
  n_assets <- ncol(returns)
  
  portfolio_returns <- numeric(n_portfolios)
  portfolio_risks <- numeric(n_portfolios)
  
  for (i in 1:n_portfolios) {
    weights <- runif(n_assets)
    weights <- weights / sum(weights)  # Normalize to sum to 1
    
    portfolio_returns[i] <- sum(weights * mean_returns)
    portfolio_risks[i] <- sqrt(t(weights) %*% cov_matrix %*% weights)
  }
  
  # Create plot data
  plot_data <- data.frame(
    risk = portfolio_risks,
    return = portfolio_returns,
    sharpe = portfolio_returns / portfolio_risks
  )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = risk, y = return, color = sharpe)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::scale_color_gradient(low = "red", high = "green", name = "Sharpe Ratio") +
    ggplot2::labs(title = title, x = "Risk (Volatility)", y = "Expected Return") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
}
