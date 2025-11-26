#' Marginal value of +1 bike at a station
#'
#' @param lambda_hat data.frame {s,t,h,lambda}.
#' @param returns data.frame {s,t,h,p_st}.
#' @param b0 named integer vector of current allocation.
#' @param s integer station id to receive +1 bike.
#' @param reps integer Monte Carlo repetitions.
#' @param seed optional seed.
#' @return numeric: drop in expected unmet pickups by adding +1 at station s
marginal_value <- function(lambda_hat, returns, b0, s, reps = 50, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  base <- mc_simulate(lambda_hat, returns, b0, n_sims = reps)
  b1 <- b0
  s_name <- as.character(s)
  if (is.na(b1[s_name])) b1[s_name] <- 0L
  b1[s_name] <- b1[s_name] + 1L
  plus <- mc_simulate(lambda_hat, returns, b1, n_sims = reps)
  sum(base$unmet_mean) - sum(plus$unmet_mean)
}


#' Greedy placement
#'
#' This function allocates bikes to stations using a greedy
#' algorithm. Starting from an initial allocation (which is all zeros), it adds
#' bikes one at a time. At each step, it evaluates every station by simulating
#' the expected unmet demand if one additional bike were placed there. The bike
#' is then assigned to the station that yields the largest reduction in expected
#' unmet pickups.
#'
#' This repeats until all B bikes have been placed.
#'
#' @param lambda_hat data.frame with columns {s, t, h, lambda}; estimated arrival rates.
#' @param returns data.frame with columns {s, t, h, p_st}; return probabilities.
#' @param B integer total number of bikes to allocate.
#' @param init named integer vector of initial allocation (default = all zeros).
#' @param reps integer number of Monte Carlo simulations per marginal evaluation.
#'             Smaller values run faster but with more noise (default: 30).
#' @param seed optional RNG seed for reproducibility.
#'
#' @return named integer vector {b_star} giving recommended bikes per station.

greedy_placement <- function(lambda_hat, returns, B, init = NULL, reps = 30, seed = NULL) {
  
  st <- sort(unique(lambda_hat$s))
  
  # initialize allocation
  if (is.null(init)) {
    b <- integer(length(st))
    names(b) <- as.character(st)
  } else {
    b <- init
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # greedy loop
  for (k in seq_len(B)) {
    gains <- numeric(length(st))
    for (i in seq_along(st)) {
      gains[i] <- marginal_value(lambda_hat, returns, b, st[i], reps = reps)
    }
    s_star <- st[which.max(gains)]
    nm <- as.character(s_star)
    if (is.na(b[nm])) b[nm] <- 0L
    b[nm] <- b[nm] + 1L
  }
  
  b
}


#' Summarize results
#'
#' @param lambda_hat data.frame {s,t,h,lambda}
#' @param returns data.frame {s,t,h,p_st}
#' @param b_star named integer vector (recommended bikes per station)
#' @param n_sims integer Monte Carlo runs for KPIs
#' @param seed optional seed
#' @return data.frame with columns {station, recommended_bikes, served_mean, unmet_mean, ending_mean}.
summarize_results <- function(lambda_hat, returns, b_star, n_sims = 1000, seed = NULL) {
  kpis <- mc_simulate(lambda_hat, returns, b_star, n_sims = n_sims, seed = seed)
  # align vector
  rec <- as.integer(b_star[as.character(kpis$station)])
  rec[is.na(rec)] <- 0L
  out <- data.frame(
    station = kpis$station,
    recommended_bikes = rec,
    served_mean = kpis$served_mean,
    unmet_mean  = kpis$unmet_mean,
    ending_mean = kpis$ending_mean
  )
  # sort by recommended bikes descending, then served
  ord <- order(-out$recommended_bikes, -out$served_mean)
  out[ord, , drop = FALSE]
}
