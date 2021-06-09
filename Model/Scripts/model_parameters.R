# A function that determines the best AR(p) model using the Akaike information criterion

process_params <- function(rat, max_lag = 13) {
  
  # Input period-cc. Output period-cc 
  
  # AR(p) estimation and selection
  p_sele <- ar.ols(rat, aic = TRUE, order.max = max_lag)
  
  # Parameters selection
  Kpi = p_sele$x.intercept
  w = p_sele$ar[ , 1, 1, drop = TRUE]
  K = 1 - sum(w)
  pi = Kpi / K
  sigma = sd(p_sele$resid, na.rm = TRUE)
  Ksigma = K * sigma
  
  # Return list of parameters
  return(list(K, w, pi, Kpi, sigma, Ksigma))
  
}

# ----- Test case -------
# rat <- read_xlsx(
#   path = "Model/Source/SourceFiles/NCREIF/NPI Returns 3-9-21.xlsx",
#   sheet = "NPI - National"
# ) %>%
#   mutate(`Total Return` = 4 * log(1 + `Total Return`)) %>%
#   `[`( , 11, drop = TRUE) %>%
#   ts(start = c(1978, 1), frequency = 4)
# 
# process_params(rat)
