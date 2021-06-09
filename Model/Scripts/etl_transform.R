update_factEconomic <- function() {
  
  require(tidyverse)
  require(lubridate)
  require(magrittr)
  require(readxl)
  
  # Data sources
  dim_source <- read_csv("Model/Source/StarSchema/dim_source.csv")
  dim_economic <- read_csv("Model/Source/StarSchema/dim_economic.csv")
  dim_date <- read_csv("Model/Source/StarSchema/dim_date.csv")
  
  # Bring all necessary functions from etl_sread
  source("Model/Scripts/etl_sread_helpers.R", local = TRUE)
  sread <- sread_aslist("Model/Scripts/etl_sread.R")
  
  # Additional data required
  meses <- tibble(
    Month = 1:12,
    Eng = month.name,
    Mes = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    Esp = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  )
  
  # Memory to store data
  fact_economic <- tibble(
    econ_key = vector("double"),    
    index_key = vector("double"),    
    date_key = vector("double"),    
    econ_value = vector("double"),    
    econ_delta = vector("double")
  )
  
  # Read data for each
  for (k in dim_economic$index_key) {

    # Parameters
    skey <- dim_economic[k, 6, drop = TRUE]
    ssource <- str_c(dim_source[skey, 7], ".", dim_source[skey, 6])
    sext <- dim_source[skey, 6, drop = TRUE]
    ssheet<- dim_economic[k, 7, drop = TRUE]
    sname <- dim_economic[k, 2, drop = TRUE]
    sfunction <- dim_economic[k, 8, drop = TRUE]
    
    tryCatch(
      {
        # Particular unction call
        fact_pivot <- sread[[sfunction]](ssource, sext, ssheet, k, sname)
        # Append to fact_economic
        fact_economic %<>% bind_rows(fact_pivot)
        cat("OK: Index '" , sname, "' succesfully loaded to fact table.\n")
      }, error = function(c) {
        cat(str_c("FAIL: '", sname, "' could not be loaded.\n"))
      }
    )
    
  }
  
  # I am replacing fact since incremental changes make no sense (double effort)
  fact_economic %>% 
    mutate(econ_key = row_number()) %>% 
    write_csv("Model/Source/StarSchema/fact_economic.csv")
  
  return(0)

}

update_factItem <- function() {
  
  require(tidyverse)
  require(lubridate)
  require(magrittr)
  require(readxl)

  # Bring all necessary functions from etl_sread
  source("Model/Scripts/etl_sread_helpers.R", local = TRUE)
  mapping_institution <- read_csv("Model/Source/StarSchema/mapping_institution.csv")
  
  # sread_item("Model/Source/SourceFiles/DatosGov/EstadosFinancieros.csv") to create dim_item
  
  # DATA GOV ----------------------------------------------------------------
  sread_xl("Model/Source/SourceFiles/DatosGov/EstadosFinancieros.csv", "csv") %>% 
    mutate(
      COD_CLASE = as.character(COD_CLASE),
      COD_GRUPO = as.character(COD_GRUPO),
      COD_CUENTA = as.character(COD_CUENTA),
      SUBCUENTA = as.character(SUBCUENTA),
      COD_CUENTA = map_chr(COD_CUENTA, function(x) ifelse(str_length(x) == 1, str_c("0", x), x)),
      SUBCUENTA = map_chr(SUBCUENTA, function(x) ifelse(str_length(x) == 1, str_c("0", x), x))
    ) %>% 
    mutate(
      `MONEDA_TOTAL(0)` = map2_dbl(`MONEDA_TOTAL(0)`, SIGNO_VALOR, function(x, p) {
        if (p == "+") {
          return(x)
        } else {
          return(-x)
        }
      })
    ) %>% 
    transmute(
      account_key = 1,
      inst_from = sread_tildes(NOMBRE_ENTIDAD),
      item_key = as.numeric(str_c(COD_CLASE, COD_GRUPO, COD_CUENTA, SUBCUENTA)),
      date_key = sread_datekey(parse_date(FECHA_CORTE, "%d/%m/%Y")),
      ccy_key = 31,
      account_value = `MONEDA_TOTAL(0)`
    ) %>% 
    inner_join(mapping_institution[, c(3, 5)], by = c("inst_from")) %>% 
    select(-c("inst_from")) %>% 
    arrange(item_key, inst_key, date_key) %>% 
    group_by(item_key, inst_key) %>% 
    mutate(account_delta = c(account_value[1], diff(account_value))) %>% 
    ungroup() %>% 
    mutate(account_key = row_number()) %>% 
    select(account_key, date_key, ccy_key, item_key, inst_key, account_value, account_delta) %>% 
    write_csv("Model/Source/StarSchema/fact_item.csv")
  
  print("FACT_ITEM succesfully updated.")
  
  # Upgrade fact_item to:
  # 1. Do incremental changes in fact (need to verify if Gov is uploading new data or deleting old records)
  # 2. Include SuperFin data
    
}
