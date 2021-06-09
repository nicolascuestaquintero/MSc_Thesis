# Functions to price real estate derivatives and efficient market price

# Derivatives pricing -----------------------------------------------------

dvt_price <- function(at, rat, rf, payoff, period = 4, pte = 12, N = 10000) {
  
  # Input APR. Output dvt price

  # 1. Determine parameters (Eq. 12) --------------------------------------
  source("Model/Scripts/model_parameters.R")
  dvt_parameters <- process_params(log(1 + rat / period))
  
  # 2. Simulate short rates (OLS Vasicek) ---------------------------------
  
  source("Model/Scripts/model_rates.R")
  dvt_rates <- process_vasicek(period * log(1 + rf / period), t = pte, n = N)
  
  # 3. Simulate underlying asset (Eqs 17 and 22) --------------------------
  
  source("Model/Scripts/model_underlying.R")
  dvt_efficient <- process_efficient(tail(at, n = 1), dvt_rates, sd(period * log(1 + rat/ period)))
  dvt_underlying <- process_underlying(at, rf, dvt_rates, dvt_efficient, dvt_parameters)
  
  # 4. Monte Carlo valuation ----------------------------------------------
  
  ifelse(
    dim(dvt_rates)[1] == 2, 
    dvt_rates <- matrix(dvt_rates[-1, ], nrow = 1), 
    dvt_rates <- dvt_rates[-1, ]
  )
  
  mean(
    sapply(tail(dvt_underlying, n = 1), function(x) payoff(x)) *
      tail(apply(exp(dvt_rates * 1 / period), 2, cumprod), n = 1)
  ) 
}


# Test case ---------------------------------------------------------------
# rat <- (read_xlsx(
#   path = "Model/Source/SourceFiles/NCREIF/NPI Returns 3-9-21.xlsx",
#   sheet = "NPI - National"
# )[ , 11, drop = TRUE] * 4) %>%
#   ts(start = c(1978, 1), frequency = 4)
# 
# at <- 100 * cumprod(1 + rat / 4) %>% 
#   ts(start = c(1978, 1), frequency = 4)
# 
# rf <- read_csv("Model/Source/SourceFiles/NCREIF/TB3MS.csv") %>%
#   transmute(Date = DATE %m+% days(-1), rf = TB3MS / 100) %>%
#   mutate(Month = month(Date)) %>%
#   filter(Month %in% c(3, 6, 9, 12)) %>%
#   `[`(, 2, drop = TRUE) %>%
#   ts(start = c(1933, 4), frequency = 4)
# 
# dvt_price(at, rat, rf, function(x) payoff_put(x, 3900), N = 10000)

