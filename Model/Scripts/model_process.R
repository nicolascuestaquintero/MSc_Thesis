# Important functions for empirical study replication

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

# Pricing real estate derivatives -----------------------------------------

# Function that prices all possible derivatives given a payoff function
main_prices <- function(ncf_df, ncf_rf, payoff, name = "prices_", itm = 0, years = 2000:2020) {
  
  source("Model/Scripts/model_pricing.R")
  
  # Generate grid for valuation inputs
  emp_prices <- expand.grid(
    Year = years,
    Quarter = 1:4,
    QTM = seq(12, 0, -1)
  ) %>% 
    arrange(
      Year, 
      Quarter, 
      desc(QTM)
    ) %>% 
    mutate(
      Expiration = as.Date(str_c(Year, "-", Quarter * 3 , "-1")) %m+% 
        months(1) %m+% days(-1),
      Valuation = Expiration %m+% 
        days(1) %m+% months(-(QTM * 3)) %m+% days(-1)
    ) %>% 
    left_join(
      select(ncf_df, Date, Index),
      by = c("Valuation" = "Date")
    ) %>% 
    mutate(
      Strike = as.integer(!((QTM + 1) %% 13 > 0)) * Index * (1 + itm / 100), # ATM vs ITM
      Strike = map_dbl(Strike, function(x) ifelse(x == 0, NA, x))
    ) %>% 
    fill(Strike) 
  
  # Use dvt_price to price derivatives
  emp_prices %<>% 
    mutate(
      Price = pmap_dbl(
        list(Index, Strike, QTM, Valuation, Expiration),
        function(S, K, m, d1, d2) {
          if (m > 0) {
            at <- filter(ncf_df, Date <= d1)[["Index"]]
            rat <- 4 * filter(ncf_df, Date <= d1)[["All"]]
            rf <- filter(ncf_rf, Date <= d1)[["rf"]]
            val <- dvt_price(at, rat, rf, function(x) payoff(x, K), pte = m)
          } else {
            val <- payoff_put(S, K)
          }
          cat(
            "Expiration at: ", d2, 
            ". Valuation at:", d1, 
            ". Strike: ", K, 
            ". Spot: ", S, 
            ". QTM: ", m, 
            ". Price: ", val,
            ".\n"
          )
          return(val)
        }
      )
    ) 
  
  # Save results
  emp_prices %>% 
    write_csv(str_c("Model/Output/Model Output/", name, ".csv"))
  
}

# Build portfolios to be hedged -------------------------------------------

# Function that use a grid of weights and determine the portfolios returns
main_portfolios <- function(wA, wI, wO, wR, wH) {
  
  # Calculation portfolio-wise
  n <- length(wA)
  v <- vector("list", n)
  for (k in 1:n) {
    v[[k]] <- ncf_df %>%
      mutate(rp = A * wA[k] + I * wI[k] + O * wO[k] + R * wR[k] + H * wH[k]) %>% 
      select(Date, rp)
  }
  
  # Results
  return(v)
}

# Derivatives deltas ------------------------------------------------------

# Function that determines the deltas of the derivatives
main_deltas <- function(main_cport, prices_name, output_name = "deltas_", years = 2000:2020, calc_prices = FALSE, drop_intrinsic = TRUE) {
  
  if (calc_prices) {
    main_prices(ncf_df, ncf_rf, payoff, name = prices_name, years = years)
  }
  
  sim_price <- read_csv(str_c("Model/Output/Model Output/", prices_name, ".csv"))  %>%
    select(Expiration, Valuation, QTM, Strike, Price) %>% # Price is the derivative price, not the spot
    left_join(select(ncf_df, Date, Index), by = c("Valuation" = "Date"))
  
  main_integrate <- function(dsp, x, drop_intrinsic = TRUE) {
    
    ff <- ifelse(drop_intrinsic, 1, 0:1)
    dsp %>% 
      left_join(x, by = c("Valuation" = "Date")) %>% 
      mutate(Spot = as.integer(!((QTM + 1) %% 13 > 0)) * Index) %>%
      group_by(Expiration) %>%
      mutate(Spot = c(1, cumprod(1 + rp[-1])) * Spot[1]) %>%
      transmute(
        basis_key = row_number(), 
        Expiration = Expiration, 
        Valuation = Valuation, 
        dS = c(NA, diff(Spot)), 
        dF = c(NA, diff(Price)),
        filter_by = ifelse(
          Price[13] == 0, 
          rep(0, 13),
          rep(1, 13)
        )
      ) %>% 
      filter(filter_by %in% ff) %>% 
      select(-c("filter_by")) %>% 
      drop_na() %>% 
      ungroup()
  }
  
  main_cport %>% 
    mutate(basis_df = map2(port_key, Portfolio, function(key, j) {
      cat("Deltas: Portfolio ", key, ".\n")
      main_integrate(dsp = sim_price, j)
      })) %>% 
    select(port_key, basis_df) %>% 
    unnest(basis_df) %>% 
    write_csv(str_c("Model/Output/Model Output/", output_name, ".csv"))
  
}

# Consolidate deltas and masure basis risk --------------------------------

# Function that consolidates data and measure basis risk
main_pvalues <- function(main_cport, prices_name, deltas_name, pvalues_name, payoff, calc_deltas = FALSE) {

  if (calc_deltas) {
    main_deltas(main_cport, prices_name = prices_name, output_name = deltas_name)
  }
  
  source("Model/Scripts/model_basis.R")
  
  read_csv(str_c("Model/Output/Model Output/", deltas_name, ".csv")) %>% 
    group_by(port_key) %>% 
    nest() %>% 
    `colnames<-`(c("port_key", "port_deltas")) %>% 
    ungroup() %>% 
    mutate(stg_deltas = map(port_deltas, function(x) {
      x %>% 
        group_by(Expiration) %>% 
        nest()
    })) %>% 
    select(-c("port_deltas")) %>% 
    mutate(stg_test = map(stg_deltas, function(x) {
      x %>% 
        mutate(basis_pval = map(data, function(y) {
          basis_risk(y$dS, y$dF)$pvalue
        })) %>% 
        select(Expiration, basis_pval) %>% 
        unnest(basis_pval)
    })) %>% 
    select(-c(stg_deltas)) %>% 
    unnest(stg_test) %>% 
    write_csv(str_c("Model/Output/Model Output/", pvalues_name, ".csv"))
  
}


