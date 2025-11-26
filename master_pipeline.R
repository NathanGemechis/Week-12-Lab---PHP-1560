# Master Pipeline

print("Loading scripts...")
source("estimation.R")      # estimate_all, clean_trips, etc.
source("simulations.R")     # simulate_day_nhpp, mc_simulate
source("placement.R")       # greedy_placement, summarize_results
source("utils.R")           # plotting helper + station table

print("Loading bike data...")
trips <- read.csv("*")  # your dataset path

print("Estimating λ(s,t,h) & return probabilities...")
est <- estimate_all(trips)
lambda_hat <- est$lambda_hat
returns    <- est$returns

# examples with different total bikes to allocate
examples <- list(
  list(B = 100),
  list(B = 200),
  list(B = 300)
)

for (ex in examples) {
  cat("\nTotal bikes:", ex$B, "\n")
  
  # Greedy placement
  b_star <- greedy_placement(lambda_hat, returns, B = ex$B, reps = 30, seed = 2025)
  
  # Summary of allocation & performance
  summary_df <- summarize_results(lambda_hat, returns, b_star, n_sims = 1000, seed = 2025)
  
  print(summary_df)
  
  # Plot results
  plot_overall_satisfaction(summary_df$served_mean, summary_df$unmet_mean)
  plot_station_satisfaction(summary_df$served_mean, summary_df$unmet_mean)
  plot_satisfaction_heatmap(summary_df$served_mean, summary_df$unmet_mean)
  
  # table of happy/unhappy per station
  print_station_summary_table(
    served_by_station = summary_df$served_mean,
    unmet_by_station  = summary_df$unmet_mean,
    stations          = summary_df$station
  )
}

print("Finished all examples.")
