# Estimation Script
library(dplyr)
library(tidyr)
library(lubridate)

#' Estimate arrival rates for bike-share trips
#'
#' @description
#' Computes the expected number of trips per hour between each pair
#' of stations. Rebalancing trips (where start or end is "R") are
#' ignored. Availability is computed as the *fraction of days a station
#' had at least 1 bike available during each hour*.
#'
#' @param data data.frame containing at least:
#'   start_station, end_station, start_time, end_time
#'
#' @return data.frame with columns:
#'   \describe{
#'     \item{start_station}{integer start station}
#'     \item{end_station}{integer end station}
#'     \item{hour}{hour of the day (0–23)}
#'     \item{avg_trips}{average hourly trips}
#'     \item{avg_avail}{fraction of days with ≥1 bike available}
#'     \item{mu_hat}{estimated arrival rate}
#'   }
estimate_arrival_rates <- function(data) {
  
  # Standardize timestamps
  data <- data %>%
    mutate(
      start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
      end_time   = as.POSIXct(end_time,   format = "%Y-%m-%d %H:%M:%S")
    )
  
  # Numeric station IDs
  # (Rebalancing rows removed later)
  data$start_station <- suppressWarnings(as.integer(data$start_station))
  data$end_station   <- suppressWarnings(as.integer(data$end_station))
  
  # Compute avg trips per hour  (x_hat)
  x_hat <- data %>%
    filter(!is.na(start_station), !is.na(end_station)) %>%
    mutate(
      hour = hour(start_time),
      date = as_date(start_time)
    ) %>%
    group_by(start_station, end_station, hour) %>%
    summarise(
      avg_trips = n() / n_distinct(date),
      .groups = "drop"
    )
  
  # Availablity:
  # A station is available during hour h on a given day
  # if any trip starts or ends at that station in that hour.
  #
  # Availability = fraction of days station was non-empty during hour h.
  
  availability <- data %>%
    mutate(
      hour = hour(start_time),
      date = as_date(start_time)
    ) %>%
    filter(!is.na(start_station)) %>%
    group_by(start_station, date, hour) %>%
    summarise(
      avail = 1,        # if any activity happens, station had ≥1 bike
      .groups = "drop"
    ) %>%
    group_by(start_station, hour) %>%
    summarise(
      avg_avail = mean(avail),   # fraction of days
      .groups = "drop"
    ) %>%
    rename(station = start_station)
  
  # Join x_hat and availability to compute mu_hat
  mu_hat <- x_hat %>%
    left_join(availability, by = c("start_station" = "station", "hour")) %>%
    mutate(
      avg_avail = replace_na(avg_avail, 0),
      mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, 0)
    )
  
  return(mu_hat)
}



#' Clean bike-share trip logs
#'
#' @description
#' Removes rebalancing rows, standardizes station IDs and extracts date/hour.
#'
#' @param trips raw trip data frame
#'
#' @return cleaned data.frame with:
#'   start_station, end_station, start_time, date, hour

clean_trips <- function(trips) {
  
  x <- trips
  
  # Remove rebalancing rows
  keep <- (x$start_station != "R") & (x$end_station != "R")
  x <- x[keep, , drop = FALSE]
  
  # Convert station IDs
  x$start_station <- as.integer(x$start_station)
  x$end_station   <- as.integer(x$end_station)
  
  # Extract date + hour
  st_lt <- as.POSIXlt(x$start_time, tz = "UTC")
  
  x$date <- as.Date(x$start_time, tz = "UTC")
  x$hour <- as.integer(st_lt$hour)
  
  x
}


#' Convert mu_hat into lambda_hat
#'
#' @description Converts the estimated rates into format {s,t,h,lambda}.
#'
#' @param mu_hat output of estimate_arrival_rates()
#'
#' @return data.frame with columns s, t, h, lambda

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
#' @description Empirical probabilities of ending station given start/hour.
#'
#' @param cleaned_trips cleaned output of clean_trips()
#'
#' @return data.frame {s, t, h, p_st}

estimate_return_probs <- function(cleaned_trips) {
  
  cleaned_trips %>%
    mutate(h = hour(start_time)) %>%
    group_by(start_station, end_station, h) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(start_station, h) %>%
    mutate(p_st = n / sum(n)) %>%
    ungroup() %>%
    rename(s = start_station, t = end_station) %>%
    select(s, t, h, p_st)
}

#' Run all estimation steps
#'
#' @description
#' Cleans trips, estimates arrival rates, converts to λ, and computes
#' empirical return probabilities.
#'
#' @param trips raw trip data.frame
#'
#' @return list(lambda_hat, returns)
estimate_all <- function(trips) {
  cleaned    <- clean_trips(trips)
  mu_hat     <- estimate_arrival_rates(cleaned)
  lambda_hat <- estimate_lambda(mu_hat)
  returns    <- estimate_return_probs(cleaned)
  
  list(lambda_hat = lambda_hat, returns = returns)
}
