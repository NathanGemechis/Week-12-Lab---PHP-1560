#' Clean bike-share trip logs (base R)
#'
#' @description
#' Standardizes trip fields and removes rebalancing rows (where stations are "R").
#'
#' return data.frame with typed times, integer station ids, date, and integer hour (0–23).


#bike <- read_csv("sample_bike.csv")

clean_trips <- function(trips) {
  x <- trips
  # drop rebalancing rows
  keep <- (x$start_station != "R") & (x$end_station != "R")
  x <- x[keep, , drop = FALSE]
  x$start_station <- as.integer(x$start_station)
  x$end_station   <- as.integer(x$end_station)
  st_lt <- as.POSIXlt(x$start_time, tz = "UTC")
  x$date <- as.Date(x$start_time, tz = "UTC")
  x$hour <- as.integer(st_lt$hour)
  
  x
}

#' Aggregate hourly OD counts and pickups (base R)
#'
#' param trips cleaned trips from clean_trips
#' return list with elements: od (start, end, hour, count), pickups (station, hour count), n_days (integrer of distinct days)

aggregate_trips <- function(trips) {
  n_days <- length(unique(trips$date))
  
  od_tab <- aggregate(
    x = list(count = rep(1L, nrow(trips))),
    by = list(start_station = trips$start_station,
              end_station   = trips$end_station,
              hour          = trips$hour),
    FUN = sum
  )
  od_tab$count <- od_tab$count / max(1L, n_days)
  
  # pickups per (s,h)
  pk_tab <- aggregate(
    x = list(count = od_tab$count),
    by = list(station = od_tab$start_station, hour = od_tab$hour),
    FUN = sum
  )
  
  list(od = od_tab, pickups = pk_tab, n_days = n_days)
}

#' Proxy availability share alpha_hat(s,h) (base R)
#'
#' @description: Approximates availability as the share of days with at least one departure at station s in hour h.
#'
#' @param trips cleaned trips.
#' return data.frame with columns {station, hour, alpha} in [0,1].

estimate_alpha_proxy <- function(trips) {
  # per (date,s,h) departures
  dep_tab <- aggregate(
    x = list(n = rep(1L, nrow(trips))),
    by = list(date = trips$date, station = trips$start_station, hour = trips$hour),
    FUN = sum
  )
  dep_tab$any_dep <- dep_tab$n > 0
  
  alpha_tab <- aggregate(
    x = list(alpha = dep_tab$any_dep),
    by = list(station = dep_tab$station, hour = dep_tab$hour),
    FUN = function(z) mean(as.numeric(z))
  )
  alpha_tab
}

#' Estimate lambda_hat(s,t,h) = x_hat / alpha_hat (base R)
#'
#' @param od data.frame {start_station, end_station, hour, count}.
#' @return data.frame {s, t, h, p_st}.

estimate_returns <- function(od) {
  # Split by (s,h)
  key <- paste(od$start_station, od$hour, sep = "_")
  split_idx <- split(seq_len(nrow(od)), key)
  
  res_list <- lapply(split_idx, function(idx) {
    tmp <- od[idx, , drop = FALSE]
    s <- tmp$start_station[1]
    h <- tmp$hour[1]
    rs <- sum(tmp$count)
    if (rs > 0) {
      p <- tmp$count / rs
    } else {
      p <- rep(0, nrow(tmp))
      stay <- which(tmp$end_station == s)
      if (length(stay) == 0L) {
        tmp <- rbind(tmp, data.frame(start_station = s, end_station = s, hour = h, count = 0))
        p <- c(p, 1)
      } else {
        p[stay[1]] <- 1
      }
    }
    data.frame(s = s, t = tmp$end_station, h = h, p_st = p)
  })
  
  do.call(rbind, res_list)
}

#' Full parameter estimation wrapper (base R)
#'
#' @param trips cleaned trips from clean_trips
#' @return list with lambda_hat and returns (P(t|s,h)).

estimate_params <- function(trips) {
  ag <- aggregate_trips(trips)
  alpha <- estimate_alpha_proxy(trips)
  lam <- estimate_lambda(ag$od, alpha)
  ret <- estimate_returns(ag$od)
  list(lambda_hat = lam, returns = ret)
}
# Estimation script
