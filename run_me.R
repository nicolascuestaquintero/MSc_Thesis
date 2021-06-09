library(tidyverse)
library(lubridate)
library(magrittr)
library(readxl)
library(httr)


# 0. Colombian Financial Data ---------------------------------------------

##### Update date sources #####

source("Model/Scripts/etl_extract.R")
download_source()

##### Update Start Schema #####

source("Model/Scripts/etl_transform.R")
update_factEconomic()
update_factItem()

# Basis Risk (NCREIF) -----------------------------------------------------

source("Model/Scripts/model_basis.R")
