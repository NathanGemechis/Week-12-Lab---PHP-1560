# Utils script: Customer satisfaction plots


# Plot overall daily customer satisfaction as a bar chart
plot_overall_satisfaction <- function(served_by_station, unmet_by_station) {
  happy  <- sum(served_by_station)
  unhappy <- sum(unmet_by_station)
  
  counts <- c(happy, unhappy)
  names(counts) <- c("Happy", "Unhappy")
  
  barplot(
    counts,
    col = c("lightgreen", "salmon"),
    ylab = "Total Customers",
    main = "Overall Daily Customer Satisfaction"
  )
}

# Plot per-station customer satisfaction as a grouped bar chart
plot_station_satisfaction <- function(served_by_station, unmet_by_station) {
  mat <- rbind(served_by_station, unmet_by_station)
  rownames(mat) <- c("Happy", "Unhappy")
  
  barplot(
    mat,
    beside = TRUE,
    col = c("lightgreen", "salmon"),
    main = "Per-Station Customer Satisfaction",
    ylab = "Count",
    legend.text = TRUE,
    args.legend = list(x = "topright")
  )
}

# Plot per-station satisfaction as a heatmap
plot_satisfaction_heatmap <- function(served_by_station, unmet_by_station) {
  mat <- rbind(served_by_station, unmet_by_station)
  rownames(mat) <- c("Happy", "Unhappy")
  
  heatmap(
    mat,
    Rowv = NA, Colv = NA,
    col = heat.colors(20),
    scale = "none",
    main = "Station-Level Satisfaction Heatmap"
  )
}

# Plot overall satisfaction from Monte Carlo simulations
plot_mc_overall_satisfaction <- function(mc_df) {
  happy  <- sum(mc_df$served_mean)
  unhappy <- sum(mc_df$unmet_mean)
  
  counts <- c(unhappy, happy)   # FIXED
  
  names(counts) <- c("Unhappy", "Happy")
  
  barplot(
    counts,
    col = c("salmon", "lightgreen"),
    ylab = "Average Customers per Day",
    main = "MC Average Overall Customer Satisfaction"
  )
}

# Plot per-station mean satisfaction from Monte Carlo simulations
plot_mc_station_satisfaction <- function(mc_df) {
  mat <- rbind(mc_df$served_mean, mc_df$unmet_mean)
  rownames(mat) <- c("Happy", "Unhappy")
  
  barplot(
    mat,
    beside = TRUE,
    col = c("lightgreen", "salmon"),
    names.arg = mc_df$station,
    main = "MC Mean Per-Station Customer Satisfaction",
    ylab = "Mean Count",
    legend.text = TRUE
  )
}

# Plot Monte Carlo station-level satisfaction as a heatmap
plot_mc_satisfaction_heatmap <- function(mc_df) {
  mat <- rbind(mc_df$served_mean, mc_df$unmet_mean)
  rownames(mat) <- c("Happy", "Unhappy")
  colnames(mat) <- mc_df$station
  
  heatmap(
    mat,
    Rowv = NA, Colv = NA,
    col = heat.colors(20),
    scale = "none",
    main = "MC Station Satisfaction Heatmap"
  )
}

# Print per-station satisfaction table
print_station_satisfaction_table <- function(served_by_station, unmet_by_station) {
  df <- data.frame(
    station = names(served_by_station),
    happy   = as.numeric(served_by_station),
    unhappy = as.numeric(unmet_by_station)
  )
  
  # sort by station ID
  df$station <- as.integer(df$station)
  df <- df[order(df$station), ]
  
  print(df, row.names = FALSE)
  
}
