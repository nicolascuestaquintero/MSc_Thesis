sread_aslist <- function(path) {
  # To create a list of functions based on a script of functions
  e <- new.env()
  source(path, local = e)
  as.list(e)
}

sread_xl <- function(spath, sext, stab = NULL, srows = NULL) {
  # To read both csv, xls and xlsx in one function
  if (sext == "csv") {
    read_csv(spath)
  } else {
    read_excel(spath, stab, skip = srows)
  }
}

sread_datekey <- function(a_date) {
  # To create datekey given a date
  a_year <- year(a_date)
  a_month <- month(a_date)
  a_day <- day(a_date)
  as.numeric(str_c(
    as.character(a_year),
    ifelse(a_month > 9, as.character(a_month), str_c("0", as.character(a_month))),
    ifelse(a_day > 9, as.character(a_day), str_c("0", as.character(a_day)))
  ))
}

sread_tildes <- function(x) {
  # To remove Spanish special characters
  x <- str_replace_all(x, "á", "a")
  x <- str_replace_all(x, "é", "e")
  x <- str_replace_all(x, "í", "i")
  x <- str_replace_all(x, "ó", "o")
  x <- str_replace_all(x, "ú", "u")
  x <- str_replace_all(x, "Á", "A")
  x <- str_replace_all(x, "É", "E")
  x <- str_replace_all(x, "Í", "I")
  x <- str_replace_all(x, "Ó", "O")
  x <- str_replace_all(x, "Ú", "U")
  x <- str_replace_all(x, "ñ", "n")
  x <- str_replace_all(x, "Ñ", "N")
  return(x)
}

sread_item <- function(path) {
  # To create PUC accounts based on DataGov data
  sread_xl(path, "csv") %>% 
    mutate(across(where(is.character), sread_tildes)) %>% 
    mutate(SIGNO_VALOR = map_chr(SIGNO_VALOR, function(x) {ifelse(x == "+", 1, -1)})) %>% 
    distinct(COD_CLASE, COD_GRUPO, COD_CUENTA, SUBCUENTA, NOMBRE_CUENTA_PUC, SIGNO_VALOR) %>% 
    mutate_all(as.character) %>% 
    mutate(
      COD_CUENTA = map_chr(COD_CUENTA, function(x) ifelse(str_length(x) == 1, str_c("0", x), x)),
      SUBCUENTA = map_chr(SUBCUENTA, function(x) ifelse(str_length(x) == 1, str_c("0", x), x))
    ) %>% 
    transmute(
      item_key = as.numeric(str_c(COD_CLASE, COD_GRUPO, COD_CUENTA, SUBCUENTA)),
      item_name = NOMBRE_CUENTA_PUC,
      item_subaccount = SUBCUENTA,
      item_account = COD_CUENTA, 
      item_group = COD_GRUPO,
      item_class = COD_CLASE,
      item_sign = as.numeric(SIGNO_VALOR)
    ) %>% 
    arrange(item_key) %>% 
    distinct(item_key, .keep_all = TRUE) %>% 
    write_csv("4dim_item.csv")
  print("File 4dim_item.csv created at project home.")
}
