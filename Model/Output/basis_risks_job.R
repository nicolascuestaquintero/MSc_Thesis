basis_risk <- lapply(
  dvt_mktdata %>% 
    filter(
      Date >= as.Date("1990-03-31"),
      # Date <= as.Date("2017-03-31")
      Date <= as.Date("1990-06-30")
    ) %$% Date, 
  function(d) {
    dfp <- dvt_mktdata %>% 
      filter(
        Date >= d,
        Date <= d %m+% months(12 * ytm)
      ) %>% 
      mutate(
        ttm = as.numeric(max(Date) - Date) / 365, 
        strike = rep(NPI[1], 4 * ytm + 1),
        exercise = "Call",
        call = pmap_dbl(list(NPI, strike, rf, ytm, vol, exercise), dvt_bsm)
      ) %>% 
      select(Date, call) %>% 
      transmute(Date, dc = c(NA, diff(call))) 
    
    br_model <- function(dataset) {
      lm(Delta ~ dc, data = dataset)
    }
    
    port_returns %>% 
      mutate(Portfolio = map(Portfolio, function(x) {
        x %>% 
          left_join(dfp, by = c("Date")) %>% 
          drop_na()
      })) %>% 
      mutate(
        linear_model = map(Portfolio, br_model),
        glance_model = map(linear_model, glance)
      ) %>% 
      select(key, linear_model, glance_model) %>% 
      mutate(Expiration = dfp[4 * ytm + 1, 1, drop = TRUE])
    
  })

map_dfr(
  basis_risk,
  function(df) {
    df %>% 
      select(key, Expiration, glance_model) %>% 
      unnest(glance_model)
  }
) %>% 
  write_csv("call_basis_risks.csv")
