library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(gganimate)

# Read NCREIF data
map_dfr(c("NPI - National", "NPI - Property Type"),
        function(x) {
          read_xlsx(path = str_c("Model/Source/SourceFiles/NCREIF/NPI Returns 3-9-21.xlsx"), sheet = x)
        }) %>%
  transmute(
    Date = yq(str_c(Year, "0", Quarter)) %m+% months(3) %m+% days(-1),
    PropertyType = factor(replace_na(PropertyType, "All"), levels = c("All", "A", "H", "I", "O", "R")),
    y = `Income Return`,
    g = `Capital Return`,
    r = `Total Return`
  ) -> ncf_dataset

# Read 3 Month T-Bills data
rf_dataset <- read_csv(str_c("Model/Source/SourceFiles/NCREIF/TB3MS.csv")) %>% 
  transmute(
    Date = DATE %m+% days(-1),
    rf = TB3MS / 100
  )

# Read S&P 500 data
spx_dataset <- read_xlsx(str_c("Model/Source/SourceFiles/NCREIF/spx.xlsx")) %>% 
  mutate(Date = as.Date(Date)) %>% 
  arrange(Date) %>% 
  select(Date, Close = `Close*`)

plot_df <- ncf_dataset %>% 
  select(Date, PropertyType, r) %>% 
  pivot_wider(names_from = PropertyType, values_from = r) %>% 
  select(
    Date,
    NPI = All,
    Apartments = A,
    Industrial = I,
    Office = O,
    Retail = R,
    Hospitality = H
  ) %>% 
  left_join(rf_dataset %>% 
              transmute(Date, `3M-TB` = rf / 4), 
            by = c("Date")) %>% 
  left_join(spx_dataset %>% 
              transmute(
                Date = Date - 1,
                SP500 = c(NA, Close[-1] / Close[-nrow(spx_dataset)] - 1)), 
            by = c("Date")) %>% 
  drop_na() %>% 
  mutate(Year = year(Date)) %>% 
  filter(Year != 1987) %>% 
  pivot_longer(cols = NPI:SP500, names_to = "Asset", values_to = "Return") %>% 
  arrange(Asset, Date) %>%
  mutate(Returnp = 1 + Return) %>% 
  select(-c("Date")) %>% 
  group_by(Year, Asset) %>% 
  summarise(
    Risk = sd(Return) * sqrt(4),
    Return = prod(Returnp) - 1
  ) %>% 
  ungroup() %>% 
  mutate(Category = map_chr(Asset, function(c) {
    if (c == "3M-TB") {
      return("Fixed Income")
    } else {
      if (c == "SP500") {
        return("Equity")
      } else {
        return("Real Estate")
      }
    }
  })) 

anim <- plot_df %>% 
  ggplot(mapping = aes(x = Risk, y = Return, color = Asset, shape = Category)) +
  geom_point(size = 2) +
  ggrepel::geom_label_repel(mapping = aes(label = Asset), size = 3) +
  transition_states(states = Year, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(title = 'Year: {closest_state}', #'k: {frame_time}', 
       x = 'Risk', 
       y = 'Return'
   ) +
  theme(legend.position = "none") +
  theme_bw()

animate(anim, fps = 2, nframes = 120)
#transition_states(states = Year)



