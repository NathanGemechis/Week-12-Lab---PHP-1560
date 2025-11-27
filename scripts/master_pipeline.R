# Master Pipeline

print("Loading scripts...")
source("estimation.R")      # estimate_all, clean_trips, etc.
source("simulations.R")     # simulate_day_nhpp, mc_simulate
source("placement.R")       # greedy_placement, summarize_results
source("utils.R")           # plotting helper + station table

print("Loading bike data...")
trips <- read.csv("*Insert path to data here*")

print("Estimating λ(s,t,h) & return probabilities...")
est <- estimate_all(trips)
lambda_hat <- est$lambda_hat
returns    <- est$returns

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
  
  # PLOTS & TABLES
  
  # 1. Satisfaction plots
  plot_overall_satisfaction(summary_df$served_mean, summary_df$unmet_mean)
  plot_station_satisfaction(summary_df$served_mean, summary_df$unmet_mean)
  plot_satisfaction_heatmap(summary_df$served_mean, summary_df$unmet_mean)
  
  # 2. Final allocation plot + table
  plot_final_allocation(b_star)
  print_final_allocation_table(b_star)
  
  # 3. Per-station happy/unhappy table
  print_station_satisfaction_table(
    served_by_station = summary_df$served_mean,
    unmet_by_station  = summary_df$unmet_mean
  )
}

print("Finished all examples.")
