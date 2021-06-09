# A function that simulate short rates using Vasicek's model with OLS parameter estimation

process_vasicek <- function(rf, t = 12, per = 4, n = 1) {
  
  require(tidyverse)
  require(readxl)
  require(lubridate)
  
  # Input acc. Output acc.
  # rf: is the APR risk-free rate atomic vector of observations
  # t: is the number of periods to simulate
  # per: is the periodicity: 1 yearly, 4 quarterly, 12 monthly
  # n: is the number of simulations
  vas_delta = 1 / per
    
  # Dataset for the model
  rf <- as.numeric(rf)
  vas_df <- tibble(
    actuals = rf[-1],
    lagged = lag(rf)[-1]
  )
  
  # OLS linear regression
  vas_ols <- lm(actuals ~ lagged, data = vas_df)
  vas_b <- unname(vas_ols[[1]][1])
  vas_a <- unname(vas_ols[[1]][2])
  vas_sd <- sd(vas_ols[[2]])
  
  # Vasicek parameters estimation
  vas_lambda <- -log(vas_a) / vas_delta
  vas_mu <- vas_b / (1 - vas_a)
  vas_sigma <- vas_sd * sqrt(- 2 * log(vas_a) / (vas_delta * (1 - vas_a ^ 2)))
  
  # Create random paths
  rm(vas_df, vas_ols, vas_a, vas_b, vas_sd)
  vas_path <- matrix(vector("double", length = (t + 1) * n), nrow = t + 1)
  vas_path[1, ] <- rep(rf[length(rf)], n)
  for (j in 1:n) {
    for(i in 2:(t + 1)) {
      vas_path[i, j] <- vas_path[i - 1, j] * exp(-vas_lambda * vas_delta) +
        vas_mu * (1 - exp(-vas_lambda * vas_delta)) +
        vas_sigma * sqrt((1 - exp(-2 * vas_lambda * vas_delta)) / (2 * vas_lambda)) * rnorm(1)
      }
  }
  
  # Return paths
  return(vas_path)
}

# ----- Test case -------
# rf <- read_csv("Model/Source/SourceFiles/NCREIF/TB3MS.csv") %>%
#   transmute(Date = DATE %m+% days(-1), rf = 4 * log(1 + TB3MS / 400)) %>%
#   `[`( , 2, drop = TRUE) %>%
#   ts(start = c(1933, 12), frequency = 12) %>%
#   process_vasicek(t = 1, n = 100)
# 
# as_tibble(rf, .name_repair = ~make.names(., unique = TRUE)) %>%
#   mutate(x_axis = row_number()) %>%
#   pivot_longer(cols = X:X.99, names_to = "Series", values_to = "Rates") %>%
#   ggplot(mapping = aes(x = x_axis, y = Rates, color = Series)) +
#   geom_line() +
#   geom_point()
# 
# rm(rf_data, ts, process_rates)
