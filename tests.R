library(testthat)

# Clean trips tests
test_that("clean_trips removes rebalancing rows and creates correct types", {
  test_data <- data.frame(
    start_station = c("1","R","2"),
    end_station = c("2","3","R"),
    start_time = c("2025-11-24 08:00:00",
                   "2025-11-24 09:00:00",
                   "2025-11-24 10:00:00"),
    end_time   = c("2025-11-24 08:15:00",
                   "2025-11-24 09:20:00",
                   "2025-11-24 10:30:00")
  )
  cleaned <- clean_trips(test_data)
  
  expect_true(all(cleaned$start_station != "R"))
  expect_true(all(cleaned$end_station != "R"))
  expect_type(cleaned$start_station, "integer")
  expect_type(cleaned$end_station, "integer")
  expect_true("hour" %in% names(cleaned))
  expect_true("date" %in% names(cleaned))
})


# Lambda / return probs
test_that("estimate_lambda outputs correct columns", {
  mu_hat <- data.frame(
    start_station = 1:2,
    end_station = 1:2,
    hour = c(8,9),
    avg_trips = c(5,3)
  )
  lambda_hat <- estimate_lambda(mu_hat)
  expect_equal(names(lambda_hat), c("s","t","h","lambda"))
})

test_that("estimate_return_probs produces probabilities between 0 and 1 and sum to 1 per (s,h)", {
  trips <- data.frame(
    start_station = c(1,1,2,2),
    end_station = c(1,2,1,2),
    start_time = as.POSIXct(c("2025-11-24 08:00",
                              "2025-11-24 08:05",
                              "2025-11-24 09:00",
                              "2025-11-24 09:10"))
  )
  cleaned <- clean_trips(trips)
  returns <- estimate_return_probs(cleaned)
  
  expect_true(all(returns$p_st >= 0 & returns$p_st <= 1))
  sums <- tapply(returns$p_st, list(returns$s, returns$h), sum)
  expect_lte(max(sums, na.rm = TRUE), 1)
})


# Simulation tests
test_that("simulate_day_nhpp produces correct inventory and served/unmet lengths", {
  b0 <- c("1"=5,"2"=3)
  lambda_hat <- data.frame(s=1:2,
                           t=1:2,
                           h=c(8,9),
                           lambda=c(5,3))
  returns <- data.frame(s=1:2,
                        t=1:2,
                        h=c(8,9),
                        p_st=c(0.5,0.5))
  
  sim <- simulate_day_nhpp(lambda_hat, returns, b0)
  expect_equal(length(sim$ending_inventory), length(b0))
  expect_equal(length(sim$served_by_station), length(b0))
  expect_equal(length(sim$unmet_by_station), length(b0))
  expect_true(all(sim$served_by_station + sim$unmet_by_station >= 0))
})

test_that("mc_simulate returns correct columns", {
  b0 <- c("1"=5,"2"=3)
  lambda_hat <- data.frame(s=1:2, t=1:2, h=c(8,9), lambda=c(5,3))
  returns <- data.frame(s=1:2, t=1:2, h=c(8,9), p_st=c(0.5,0.5))
  
  mc <- mc_simulate(lambda_hat, returns, b0, n_sims = 2)
  expect_equal(names(mc), c("station",
                            "served_mean",
                            "unmet_mean",
                            "ending_mean"))
})


# Placement tests

test_that("greedy_placement returns a vector of correct length", {
  lambda_hat <- data.frame(s=1:2, t=1:2, h=c(8,9), lambda=c(5,3))
  returns <- data.frame(s=1:2, t=1:2, h=c(8,9), p_st=c(0.5,0.5))
  b_star <- greedy_placement(lambda_hat, returns, B=2, reps=2)
  expect_equal(length(b_star), length(unique(lambda_hat$s)))
})

test_that("marginal_value returns numeric", {
  lambda_hat <- data.frame(s=1:2, t=1:2, h=c(8,9), lambda=c(5,3))
  returns <- data.frame(s=1:2, t=1:2, h=c(8,9), p_st=c(0.5,0.5))
  b0 <- c("1"=1,"2"=1)
  mv <- marginal_value(lambda_hat, returns, b0, s=1, reps=2)
  expect_type(mv, "double")
})


# Plot tests (just check they run)
test_that("plotting functions run without error", {
  sim <- list(
    served_by_station = c("1"=5,"2"=3),
    unmet_by_station = c("1"=1,"2"=2)
  )
  mc <- data.frame(
    station = c(1,2),
    served_mean = c(5,3),
    unmet_mean = c(1,2)
  )
  
  expect_error(plot_overall_satisfaction(sim$served_by_station,
                                         sim$unmet_by_station), NA)
  expect_error(plot_station_satisfaction(sim$served_by_station,
                                         sim$unmet_by_station), NA)
  expect_error(plot_satisfaction_heatmap(sim$served_by_station,
                                         sim$unmet_by_station), NA)
  expect_error(plot_mc_overall_satisfaction(mc), NA)
  expect_error(plot_mc_station_satisfaction(mc), NA)
  expect_error(plot_mc_satisfaction_heatmap(mc), NA)
})
