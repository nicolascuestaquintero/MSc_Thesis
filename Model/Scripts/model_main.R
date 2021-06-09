# Main run of the empirical study

source("Model/Scripts/model_process.R")

# 1. Source empirical data ------------------------------------------------

# NCREIF Index

ncf_source <- "Model/Source/SourceFiles/NCREIF/NPI Returns 3-9-21.xlsx"

ncf_df <- map_dfr(c("NPI - National", "NPI - Property Type"),
                  function(x) {
                    read_xlsx(path = ncf_source, sheet = x)
                  }) %>% 
  transmute(
    Date = yq(str_c(Year, "0", Quarter)) %m+% months(3) %m+% days(-1),
    PropertyType = factor(replace_na(PropertyType, "All"), levels = c("All", "A", "H", "I", "O", "R")),
    y = `Income Return`,
    g = `Capital Return`,
    r = `Total Return`
  ) %>% 
  select(Date, PropertyType, g) %>% 
  pivot_wider(names_from = PropertyType, values_from = g) %>% 
  drop_na() %>% 
  left_join(
    read_excel(ncf_source, sheet = "NPI - National") %>% 
      transmute(Date = yq(str_c(Year, "0", Quarter)) %m+% months(3) %m+% days(-1), Index),
    by = c("Date")
  )

# Risk free rates 

ncf_rf <- read_csv("Model/Source/SourceFiles/NCREIF/TB3MS.csv") %>%
  transmute(Date = DATE %m+% days(-1), rf = TB3MS / 100) %>% 
  mutate(Month = month(Date)) %>% 
  filter(Month %in% c(3, 6, 9, 12)) %>% 
  filter(Date >= min(ncf_df$Date)) %>% 
  select(Date, rf)


# Define derivatives payoff -----------------------------------------------

payoff_put <- function(Spot, Strike) return(max(Strike - Spot, 0))

main_prices(ncf_df, ncf_rf, payoff_put, "prices_put_atm", itm = 0, years = 1995:2020)
main_prices(ncf_df, ncf_rf, payoff_put, "prices_put_itm1", itm = 1, years = 1995:2020)
main_prices(ncf_df, ncf_rf, payoff_put, "prices_put_itm2", itm = 2, years = 1995:2020)

# 3. Build portfolios to be hedged ----------------------------------------

# Grid of all possible portfolios (No short selling, only real estate)
main_cport <- expand.grid(
  wA = seq(0, 1, 0.10),
  wI = seq(0, 1, 0.10),
  wO = seq(0, 1, 0.10),
  wR = seq(0, 1, 0.10),
  wH = seq(0, 1, 0.10)
) %>% 
  mutate(Filter = wA + wI + wO + wR + wH) %>%
  filter(Filter == 1) %>% 
  mutate(port_key = row_number()) %>% 
  select(port_key, wA:wH) %>% 
  as_tibble() %>% 
  mutate(Portfolio = main_portfolios(wA, wI, wO, wR, wH))

# 4. Measure basis risk ---------------------------------------------------

main_pvalues(main_cport, "prices_put", "deltas_put", "pvalues_put", payoff_put, calc_deltas = TRUE)
