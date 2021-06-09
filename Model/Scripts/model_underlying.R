# Functions that estimate the Vasicek model OLS parameters

process_efficient <- function(s, rf, vol, q = 0.0067, per = 4) {
  
  # Input acc. Output gamma values.
  # s: is the initial efficient market price (gamma(0))
  # rf: is the matrix of risk-free rate paths
  # vol: is the process volatility continuously compounded
  # q: is the convenience yield
  # per: is the periodicity: 1 yearly, 4 quarterly, 12 monthly
  # t: is the number of periods to simulate
  # n: is the number of simulations
  eff_delta = 1 / per
  t <- nrow(rf) - 1
  n <- ncol(rf)
  
  # Determine price path
  eff_path <- matrix(rep(0, (t + 1) * n), nrow = t + 1, ncol = n)
  eff_path[1,] <- s
  for (j in 1:n) {
    for (i in 2:(t + 1)) {
      eff_path[i, j] <- eff_path[i - 1, j] * exp(
        eff_delta * (rf[i, j] - q - (vol ^ 2) / 2) + vol * sqrt(eff_delta) * rnorm(1)
      )
    }
  }
  
  # Return paths
  return(eff_path)
  
}

process_underlying <- function(at, rf, dvt_rates, dvt_efficient, dvt_parameters, q = 0.0067, per = 4) {
  
  # Input acc. Output index value.
  # at: are the actual index historical values (a(t))
  # rf: are the actual free-risk rate historical values
  # dvt_rf: are the matrix of risk-free rate paths simulation (r(t))
  # dvt_efficient: are the efficient market price path simulations (gamma(t))
  # dvt_parameters: are the AR(p) model estimated parameters
  # q: is the convenience yield
  # per: is the periodicity: 1 yearly, 4 quarterly, 12 monthly
  # t: is the number of periods to simulate
  # n: is the number of simulations
  rf <- as.numeric(rf)
  at <- as.numeric(at)
  a_delta = 1 / per
  t <- nrow(dvt_efficient) - 1
  n <- ncol(dvt_efficient)
  p <- length(dvt_parameters[[2]])
  
  # Processes update
  upd_p <- function(p0, rt, m) {
    tt <- length(rt)
    p0 * exp(sum(rt[tt:(tt - m)] * a_delta) - q * m)
  }
  
  # Index values simulation
  K <- dvt_parameters[[1]]
  a_path <- matrix(rep(0, (t + 1) * n), nrow = t + 1, ncol = n)
  a_path[1, ] <- tail(at, n = 1)
  for (j in 1:n) {
    # Equation (17)
    at_p <- at
    at_t <- length(at_p)
    rf_p <- rf
    rf_t <- length(rf_p)
    for (i in 2:(t + 1)) {
      # browser()
      at_p <- c(at_p, dvt_efficient[i, j])
      at_t <- at_t + 1
      rf_p <- c(rf, dvt_rates[i, j])
      rf_t <- rf_t + 1
      fvalue <- 0
      for (k in 1:p) {
        fvalue <- fvalue + (1 - K) ^ (k - 1) * upd_p(at_p[at_t - k + 1], rf_p, k - 1)
      }
      fvalue <- K * fvalue + upd_p(at_p[at_t - p], rf_p, p) * (1 - K) ^ p
      a_path[i, j] <- fvalue
    }
  }
  return(a_path)
}
