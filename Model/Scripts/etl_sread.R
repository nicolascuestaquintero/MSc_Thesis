# search_source = ssource
# search_extension = sext
# search_sheet = ssheet
# search_key = k
# search_name = sname

sread_ipc <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- switch(search_name,
         "IPC (Sin alimentos)" = 7,
         "IPC (Energeticos)" = 6,
         "IPC (Total menos energeticos)" = 7,
         "IPC (Servicios)" = 7,
         "IPC (Bienes durables)" = 7,
         "IPC (Bienes semi durables)" = 7,
         "IPC (Bienes no durables)" = 7
  )
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows) %>% 
    select(1:3) %>% 
    drop_na() %>% 
    `colnames<-`(c("date_year", "Esp", "econ_value")) %>% 
    left_join(meses[,c(1,4)], by = c("Esp")) %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(date_year, Month, 1) %m+% months(1) - 1),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE],    
      econ_delta = c(econ_value[1], diff(econ_value))
    )
  
  # Output
  return(fact)
      
}

sread_ivp <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 10
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)
  
  # Data transformations
  fact %<>% 
    `colnames<-`(c("ColumnaA", str_c("Year", seq(2001, 2001 + ncol(fact) - 2), sep = ""))) %>% 
    filter(ColumnaA == "TOTAL NACIONAL") %>% 
    mutate(Year2001 = as.numeric(Year2001), Year2002 = as.numeric(Year2002)) %>% 
    pivot_longer(cols = starts_with("Year"), names_to = "date_year", values_to = "econ_value") %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(str_sub(date_year, 5, 9), 12, 31)),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE],    
      econ_delta = c(econ_value[1], diff(econ_value))
    )
    
    # Output
    return(fact)
  
}

sread_iccv <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 5
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)
  
  # Data transformations
  new_names <- c("Esp", unlist(fact[1, 1:(which(str_ends(colnames(fact), "corrido")) - 1)])[-1])
  fact <- fact[2:13, 1:(which(str_ends(colnames(fact), "corrido")) - 1)] %>% 
    `colnames<-`(new_names) %>% 
    pivot_longer(cols = starts_with("2"), values_to = "econ_value", names_to = "date_year") %>% 
    drop_na() %>% 
    left_join(meses[,c(1,4)], by = c("Esp")) %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(as.numeric(date_year), Month, 1) %m+% months(1) - 1),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE]
    ) %>% 
    arrange(date_key) %>% 
    mutate(econ_delta = c(econ_value[1], diff(econ_value)))
  
  # Output
  return(fact)
  
}

sread_chv <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  if (search_name == "CHV (Saldo de capital total creditos de vivienda)") {rm_rows <- 11; sel_col <- 3}
  if (search_name == "CHV (Tipo - VIS)") {rm_rows <- 9; sel_col <- 3}
  if (search_name == "CHV (Tipo - NO VIS)") {rm_rows <- 9; sel_col <- 6}
  if (search_name == "CHV (Vencimiento - Vigente)") {rm_rows <- 9; sel_col <- 3}
  if (search_name == "CHV (Vencimiento - Vencida)") {rm_rows <- 9; sel_col <- 6}
  if (search_name == "CHV (Moneda - UVR)") {rm_rows <- 9; sel_col <- 3}
  if (search_name == "CHV (Moneda - PESOS)") {rm_rows <- 9; sel_col <- 6}
  if (search_name == "CHV (Numero de creditos)") {rm_rows <- 10; sel_col <- 3}
  if (search_name == "CHV (Acreedor - Cooperativas y fondos)") {rm_rows <- 9; sel_col <- 3}
  if (search_name == "CHV (Acreedor - Establecimientos de Credito)") {rm_rows <- 9; sel_col <- 6}
  if (search_name == "CHV (Acreedor - CCF)") {rm_rows <- 9; sel_col <- 9}
  if (search_name == "CHV (Acreedor - Otros)") {rm_rows <- 9; sel_col <- 12}
  if (search_name == "CHV (Deudor - Propia en balance)") {rm_rows <- 9; sel_col <- 3}
  if (search_name == "CHV (Deudor - Propia fuera de balance)") {rm_rows <- 9; sel_col <- 6}
  if (search_name == "CHV (Deudor - Propia Cisa)") {rm_rows <- 9; sel_col <- 9}
  if (search_name == "CHV (Deudor - Titularizada)") {rm_rows <- 9; sel_col <- 12}
  if (search_name == "CHV (Deudor - Adm Fogafin)") {rm_rows <- 9; sel_col <- 15}
  if (search_name == "CHV (Deudor - Adm Patrimonio autonomo)") {rm_rows <- 9; sel_col <- 18}
  if (search_name == "CHV (Deudor - Adm Otros)") {rm_rows <- 9; sel_col <- 21}
  quarter_map <- tibble(
    trimestre = c("Enero - marzo", "Abril - junio", "Julio - septiembre", "Octubre - diciembre"),
    date_eoq = c(3, 6, 9, 12)
  )
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)[, c(1:2, sel_col)] %>% 
    `colnames<-`(c("date_year", "trimestre", "econ_value")) %>% 
    fill(date_year) %>% 
    drop_na() %>% 
    left_join(quarter_map, by = c("trimestre")) %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey((make_date(as.numeric(date_year), date_eoq, 1) %m+% months(1)) - 1),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE],    
      econ_delta = c(econ_value[1], diff(econ_value))
    )
    
  # Output
  return(fact)
  
}

sread_ipvn <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 8
  sel_col <- switch(search_name,
                    "IPVN (Agregado)" = 2,
                    "IPVN (Bogota)" = 4,
                    "IPVN (Cali)" = 5,
                    "IPVN (Medellin)" = 6
  )
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)[, c(1, sel_col)] %>% 
    `colnames<-`(c("Fecha", "econ_value")) %>% 
    drop_na() %>% 
    transmute(
      date_year = as.numeric(str_sub(Fecha, 1, 4)),
      date_month = as.numeric(str_sub(Fecha, 5, 6)),
      econ_value = econ_value
    )  %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(date_year, date_month, 1) %m+% months(1) - 1),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE]
    ) %>% 
    arrange(date_key) %>% 
    mutate(econ_delta = c(econ_value[1], diff(econ_value)))
  
  # Output
  return(fact)
  
}

sread_ipvuy <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 12
  sel_col <- switch(search_name,
                    "IPVU (Anual nominal - Agregado)" = 2, 
                    "IPVU (Anual nominal - Bogota)" = 3, 
                    "IPVU (Anual nominal - Medellin)" = 4, 
                    "IPVU (Anual nominal - Cali)" = 5, 
                    "IPVU (Anual nominal - VIS)" = 7, 
                    "IPVU (Anual nominal - NO VIS)" = 8, 
                    "IPVU (Anual real - Agregado)" = 9, 
                    "IPVU (Anual real - Bogota)" = 10, 
                    "IPVU (Anual real - Medellin)" = 11, 
                    "IPVU (Anual real - Cali)" = 12, 
                    "IPVU (Anual real - VIS)" = 14, 
                    "IPVU (Anual real - NO VIS)" = 15
  )
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)[, c(1, sel_col)] %>% 
    `colnames<-`(c("Fecha", "econ_value")) %>% 
    drop_na() %>% 
    transmute(
      date_year = parse_double(Fecha),
      econ_value = econ_value
    )  %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(date_year, 12, 31)),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE]
    ) %>% 
    arrange(date_key) %>% 
    mutate(econ_delta = c(econ_value[1], diff(econ_value)))
  
  # Output
  return(fact)
  
}

sread_ipvuq <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 10
  sel_col <- switch(search_name,
                    "IPVU (Trimestral nominal - Agregado)" = 2,
                    "IPVU (Trimestral real - Agregado)" = 3
  )
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)[, c(1, sel_col)] %>% 
    `colnames<-`(c("Fecha", "econ_value")) %>% 
    drop_na() %>% 
    transmute(
      date_year = as.numeric(str_sub(Fecha, 1, 4)),
      date_quarter = sapply(Fecha, function(x) {
        dq <- str_remove(x, "\\d\\d\\d\\d-")
        if (dq == "I") return(3)
        if (dq == "II") return(6)
        if (dq == "III") return(9)
        if (dq == "IV") return(12)
      }),
      econ_value = econ_value
    )  %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(date_year, date_quarter, 1) %m+% months(1) - 1),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE]
    ) %>% 
    arrange(date_key) %>% 
    mutate(econ_delta = c(econ_value[1], diff(econ_value)))
  
  # Output
  return(fact)
  
}

sread_colcap <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 7
  sel_col <- 2
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)[, c(1, sel_col)] %>% 
    `colnames<-`(c("Fecha", "econ_value")) %>% 
    drop_na() %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(Fecha),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE]
    ) %>% 
    arrange(date_key) %>% 
    mutate(econ_delta = c(econ_value[1], diff(econ_value)))
  
  # Output
  return(fact)
  
  
}

sread_tes <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 7
  sel_col <- switch(search_name,
                    "TES PESOS 0C 1Y" = 2,	
                    "TES PESOS 0C 5Y" = 3,	
                    "TES PESOS 0C 10Y" = 4,	
                    "TES UVR 0C 1Y" = 2,	
                    "TES UVR 0C 5Y" = 3,	
                    "TES UVR 0C 10Y" = 4
  )
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)[, c(1, sel_col)] %>% 
    `colnames<-`(c("Fecha", "econ_value")) %>% 
    drop_na() %>% 
    mutate(Fecha = as.Date.numeric(parse_double(Fecha), origin = "1899-12-30")) %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(year(Fecha), month(Fecha), 1) %m+% months(1) - 1),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE]
    ) %>% 
    arrange(date_key) %>% 
    mutate(econ_delta = c(econ_value[1], diff(econ_value)))
  
  # Output
  return(fact)
  
}

sread_desempleo <- function(search_source, search_extension, search_sheet, search_key, search_name) {
  
  # Preliminary
  rm_rows <- 8
  sel_col <- 3
  
  # Data transformations
  fact <- sread_xl(search_source, search_extension, search_sheet, rm_rows)[, c(1, sel_col)] %>% 
    `colnames<-`(c("Fecha", "econ_value")) %>% 
    drop_na() %>% 
    transmute(
      date_year = as.numeric(str_sub(Fecha, 1, 4)),
      date_month = as.numeric(str_sub(Fecha, 6, 7)),
      econ_value = econ_value
    )  %>% 
    transmute(
      econ_key = 1,    
      index_key = search_key,    
      date_key = sread_datekey(make_date(date_year, date_month, 1) %m+% months(1) - 1),   
      econ_value = econ_value * dim_economic[k, 5, drop = TRUE]
    ) %>% 
    arrange(date_key) %>% 
    mutate(econ_delta = c(econ_value[1], diff(econ_value)))
  
  # Output
  return(fact)
  
}
