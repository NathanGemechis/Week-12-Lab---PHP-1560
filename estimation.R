library(dplyr)
library(tidyr)
library(lubridate)


#' Estimate arrival rates for bike-share trips
#'
#' @description
#' Computes the expected number of trips per hour between each pair of stations,
#' accounting for station availability. Rebalancing trips (start or end at "R") are ignored.
#'
#' @param data data.frame of bike-share trips containing columns: start_station, end_station, start_time, end_time
#' @return data.frame with columns:
#' \describe{
#'   \item{start_station}{integer or character station id of trip start}
#'   \item{end_station}{integer or character station id of trip end}
#'   \item{hour}{integer hour of the day (0–23)}
#'   \item{avg_trips}{average number of trips per hour between start and end station}
#'   \item{avg_avail}{average availability of bikes at start station during that hour}
#'   \item{mu_hat}{estimated arrival rate, defined as avg_trips / avg_avail}
#' }
estimate_arrival_rates <- function(data) {
  data <- data %>%
    mutate(
      start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
      end_time   = as.POSIXct(end_time,   format = "%Y-%m-%d %H:%M:%S")
    )
  
  # compute the average number of trips per hour between each pair
  x_hat <- data %>%
    mutate(hour = hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarise(avg_trips = n() / n_distinct(as_date(start_time)), 
              .groups = "drop")
  
  # pivot longer to get change in count 
  data$end_station <- as.character(data$end_station)
  
  trips_long <- data %>%
    pivot_longer(
      cols = c("start_station", "start_time", "end_station", "end_time"),
      names_to = c("type", ".value"),
      names_pattern = "(start|end)_(.*)"
    ) %>%
    mutate(
      change = ifelse(type == "start", -1, 1),
      hour = hour(time)
    ) %>%
    select(station, time, hour, change)
  
  # add hour markers so we can get cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1), seq(0,23,1) + 0.9999999)
  stations <- unique(trips_long$station)
  
  hr_pts <- expand.grid(
    time = dates,
    hour = hours,
    station = stations
  ) %>%
    mutate(
      time = as.POSIXct(time) + hour*60*60,
      hour = hour(time)
    )
  
  hr_pts$change <- 0
  trips_long <- rbind(trips_long, hr_pts)
  
  # find average availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>% 
    mutate(
      count = cumsum(change),
      date = as_date(time)
    ) %>%
    group_by(station, hour, date) %>%
    summarize(
      time_avail = sum(
        difftime(time, lag(time), units="hours") * (count > 0),
        na.rm = TRUE
      )
    ) %>%
    summarize(avg_avail = mean(time_avail)) %>%
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4)) %>%
    ungroup()
  
  # join the data and compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  return(mu_hat)
}

#' Clean bike-share trip logs
#'
#' @description
#' Standardizes trip fields and removes rebalancing rows (where stations are "R").
#'
#' @param trips data.frame containing at least start_station, end_station, start_time, end_time
#' @return data.frame with typed times, integer station ids, date, and integer hour (0–23)
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


#' Convert arrival rates to lambda_hat
#'
#' @description
#' Renames and selects columns from mu_hat to standard lambda format.
#'
#' @param mu_hat data.frame output of estimate_arrival_rates
#' @return data.frame with columns {s, t, h, lambda}
estimate_lambda <- function(mu_hat) {
  mu_hat %>%
    rename(
      s = start_station,
      t = end_station,
      h = hour,
      lambda = mu_hat
    ) %>%
    select(s, t, h, lambda)
}


#' Estimate return probabilities P(t | s, h)
#'
#' @description
#' Computes empirical probabilities of trips ending at each station t given start s and hour h.
#'
#' @param cleaned_trips data.frame output of clean_trips
#' @return data.frame with columns {s, t, h, p_st} containing return probabilities
estimate_return_probs <- function(cleaned_trips) {
  cleaned_trips %>%
    mutate(h = hour(start_time)) %>%
    group_by(start_station, end_station, h) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(start_station, h) %>%
    mutate(p_st = n / sum(n)) %>%
    ungroup() %>%
    rename(
      s = start_station,
      t = end_station,
      h = h
    ) %>%
    select(s, t, h, p_st)
}


#' Wrapper function to run all estimation steps
#'
#' @description
#' Cleans trips, estimates arrival rates, converts to lambda_hat, and computes return probabilities.
#'
#' @param trips data.frame of raw bike-share trips
#' @return list containing:
#' \describe{
#'   \item{lambda_hat}{data.frame with columns {s, t, h, lambda}}
#'   \item{returns}{data.frame with columns {s, t, h, p_st}}
#' }
estimate_all <- function(trips) {
  cleaned <- clean_trips(trips)
  mu_hat  <- estimate_arrival_rates(cleaned)
  lambda_hat <- estimate_lambda(mu_hat)
  returns    <- estimate_return_probs(cleaned)
  
  list(
    lambda_hat = lambda_hat,
    returns = returns
  )
}

