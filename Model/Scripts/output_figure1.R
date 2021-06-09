library(tidyverse)
library(readxl)
library(lubridate)

# Read data
ncf_source <- "Model/Source/SourceFiles/NCREIF/NPI Returns 3-9-21.xlsx"
map_dfr(c("NPI - National", "NPI - Property Type"),
        function(x) {
          read_xlsx(path = ncf_source, sheet = x)
        }) %>% 
  transmute(
    Date = yq(str_c(Year, "0", Quarter)) %m+% months(3) %m+% days(-1),
    PropertyType = factor(replace_na(PropertyType, "All"), levels = c("All", "A", "H", "I", "O", "R")),
    y = `Income Return`,
    g = `Capital Return`,
    r = `Total Return`
  ) -> ncf_dataset

# Create dataset
returns <- ts(
  (ncf_dataset %>% 
    filter(
      PropertyType == "All", 
      Date <= as.Date("2020-12-31")
    ))$r, 
  start = 1978, 
  frequency = 4
)

# Create plot
plot <- forecast::ggtsdisplay(
  returns, 
  lag.max = 30, 
  plot.type = "scatter", 
  theme = theme_bw(),
  ylab = "NPI Total Returns (QoQ)", 
  xlab = "Year"
)
