library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

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

# Transform and plot
ncf_dataset %>% 
  select(Date, PropertyType, g) %>% 
  pivot_wider(names_from = PropertyType, values_from = g) %>% 
  select(-c("Date")) %>% 
  drop_na() %>%
  `colnames<-`(c("Nation", "Apartments", "Industrial", "Office", "Retail", "Hospitality")) %>% 
  GGally::ggpairs() +
  scale_x_continuous(labels = percent) +
  theme_bw() +
  theme(
    axis.text.x=element_text(angle = 90, vjust = 0.5, size = 0.75),
    strip.background = element_blank(),
    strip.placement = "outside"
  )
