# Bike-Share Simulation & Optimization

## Overview
Estimate bike-share demand, simulate daily operations, and recommend optimal bike allocations to minimize unmet pickups.

## Scripts

- **estimation.R**
  - Cleans raw trip data (removes rebalancing rows, extracts date/hour)
  - Estimates hourly arrival rates λ(s,t,h) between stations
  - Computes empirical return probabilities P(t | s, h)
  - Wrapper `estimate_all()` produces `lambda_hat` and `returns`

- **simulations.R**
  - Simulates one day of bike-share operations (`simulate_day_nhpp()`)
  - Monte Carlo evaluation of bike allocations (`mc_simulate()`)

- **placement.R**
  - Computes marginal value of adding bikes (`marginal_value()`)
  - Greedy bike allocation optimization (`greedy_placement()`)
  - Summarizes results per station (`summarize_results()`)
  - Includes a per-station table of happy vs. unhappy customers

- **utils.R**
  - Visualization functions for customer satisfaction
  - Overall and per-station bar charts
  - Station-level heatmaps
  - Prints a table showing each station’s happy/unhappy counts

- **tests.R**
  - Unit tests for cleaning, λ estimation, return probabilities, simulations, placement, and plotting

- **master_pipeline.R**
  - Loads all scripts and trip data
  - Runs estimation, Monte Carlo simulation, greedy optimization, plotting, and prints per-station satisfaction table

## How to Run

1. Place all scripts and the trip CSV in the same directory.  
2. Insert file path to your bike data CSV in `master_pipeline.R`  
3. Open and run `master_pipeline.R` in R.  
4. Outputs:
   - `summary_df`: recommended allocations and expected served/unmet counts  
   - Plots: overall and per-station satisfaction, bike distribution
   - Table: station-level happy/unhappy customer counts, bike allocations
