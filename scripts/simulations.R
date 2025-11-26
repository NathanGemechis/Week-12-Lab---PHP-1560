# Simulation Script
#' Simulate one day under NHPP demand
#'
#' @description
#' For each hour h and origin s, draw total demand D(s,h) ~ Poisson(sum_t lambda(s,t,h)).
#' Served pickups are min(D, inventory). Served trips are routed to t using
#' probabilities proportional to lambda(s,t,h); if missing, falls back to returns P(t|s,h).
#'
#' @param lambda_hat data.frame: columns {s, t, h, lambda}
#' @param returns data.frame: columns {s, t, h, p_st}
#' @param b0 named integer vector of starting bikes per station (names are station ids)
#' @param hours integer vector of hours to simulate (default 0:23)
#' @param seed optional integer RNG seed
#' @return list with ending_inventory, served_by_station, unmet_by_station

 
 
# Default seed
seed <- 123
simulate_day_nhpp <- function(lambda_hat, returns, b0, hours = 0:23, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  stations <- sort(unique(as.integer(c(lambda_hat$s, lambda_hat$t, as.integer(names(b0))))))
  inv <- integer(length(stations)); names(inv) <- as.character(stations)
  # initialize inventory from b0 (missing -> 0)
  inv[names(b0)] <- as.integer(b0)
  
  served <- integer(length(stations)); names(served) <- names(inv)
  unmet <- integer(length(stations)); names(unmet)  <- names(inv)
  
  lam_sh <- aggregate(lambda_hat$lambda,
                      by = list(s = lambda_hat$s, h = lambda_hat$h),
                      FUN = sum, na.rm = TRUE)
  names(lam_sh)[3] <- "lam_total"
  
  for (hh in hours) {
    idx_h <- which(lam_sh$h == hh)
    demand <- integer(length(stations)); names(demand) <- names(inv)
    if (length(idx_h) > 0L) {
      s_vec <- lam_sh$s[idx_h]
      mu <- pmax(lam_sh$lam_total[idx_h], 0)
      draws <- rpois(length(mu), lambda = mu)
      for (i in seq_along(s_vec)) {
        demand[as.character(s_vec[i])] <- draws[i]
      }
    }
    served_now <- pmin(demand, inv[names(demand)])
    unmet_now <- demand - served_now
    
    w_h <- lambda_hat[lambda_hat$h == hh, c("s","t","lambda")]
    # split by s
    key <- split(seq_len(nrow(w_h)), w_h$s)
    w_list <- lapply(key, function(ix) {
      tot <- sum(w_h$lambda[ix], na.rm = TRUE)
      if (tot > 0) {
        probs <- w_h$lambda[ix] / tot
      } else {
        probs <- rep(NA_real_, length(ix))
      }
      data.frame(t = w_h$t[ix], w = probs)
    })
    
    # fill NA weights with returns, ruturns hh
    R_h <- returns[returns$h == hh, , drop = FALSE]
    by_s <- split(seq_len(nrow(R_h)), R_h$s)
    
    for (ss in names(served_now)) {
      k <- as.integer(served_now[[ss]])
      if (k > 0L) {
        inv[[ss]] <- inv[[ss]] - k
        s_int <- as.integer(ss)
        if (!is.null(w_list[[as.character(s_int)]])) {
          wf <- w_list[[as.character(s_int)]]
          if (any(is.na(wf$w))) {
            # replace with returns
            if (!is.null(by_s[[as.character(s_int)]])) {
              rr <- R_h[by_s[[as.character(s_int)]], c("t","p_st")]
              probs <- rr$p_st
              dests <- rr$t
            } else {
              probs <- 1; dests <- s_int
            }
          } else {
            probs <- wf$w
            dests <- wf$t
          }
        } else {
          if (!is.null(by_s[[as.character(s_int)]])) {
            rr <- R_h[by_s[[as.character(s_int)]], c("t","p_st")]
            probs <- rr$p_st
            dests <- rr$t
          } else {
            probs <- 1; dests <- s_int
          }
        }
        probs <- probs / sum(probs)
        draw <- as.vector(rmultinom(1L, size = k, prob = probs))
        for (j in seq_along(dests)) {
          inv[[as.character(dests[[j]])]] <- inv[[as.character(dests[[j]])]] + draw[[j]]
        }
      }
    }
    
    served <- served + served_now
    unmet <- unmet  + unmet_now
  }
  
  list(
    ending_inventory = inv,
    served_by_station = served,
    unmet_by_station = unmet
  )
}

#' Monte Carlo evaluation of an allocation
#'
#' @param lambda_hat data.frame {s,t,h,lambda}
#' @param returns data.frame {s,t,h,p_st}
#' @param b0 named integer vector (start bikes by station)
#' @param n_sims integer number of simulated days
#' @param seed optional integer seed
#' @return data.frame with columns {station, served_mean, unmet_mean, ending_mean}
mc_simulate <- function(lambda_hat, returns, b0, n_sims = 200, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stations <- sort(unique(as.integer(c(lambda_hat$s, lambda_hat$t, as.integer(names(b0))))))
  S <- matrix(0, nrow = n_sims, ncol = length(stations)); colnames(S) <- as.character(stations)
  U <- S; E <- S
  
  for (i in seq_len(n_sims)) {
    sim <- simulate_day_nhpp(lambda_hat, returns, b0)
    S[i, ] <- as.integer(sim$served_by_station[as.character(stations)])
    U[i, ] <- as.integer(sim$unmet_by_station[as.character(stations)])
    E[i, ] <- as.integer(sim$ending_inventory[as.character(stations)])
  }
  data.frame(
    station = stations,
    served_mean = colMeans(S),
    unmet_mean = colMeans(U),
    ending_mean = colMeans(E)
  )
}


