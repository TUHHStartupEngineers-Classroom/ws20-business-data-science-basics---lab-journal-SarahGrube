# 1.0 Library
library(tidyverse)
library(ggrepel)

#2.0 Load + merge Data
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>%
  select(month, year, day, countriesAndTerritories, starts_with("Cumulative_number_for"),dateRep) %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  )) %>%
  filter(countriesAndTerritories %in% c("Germany", "UK", "France", "Spain", "USA")) %>%
  filter(year == 2020) %>%
  filter(month < 12) %>%
  group_by(countriesAndTerritories, month) %>%
  ungroup()

names(covid_data_tbl)[5] <- "Cumulative_number"

# variables for labeling the newest COVID-number
xy <- covid_data_tbl %>%
  filter(dateRep == "30/11/2020")

label = xy$Cumulative_number

y <- xy$Cumulative_number
x<- as.Date(lubridate::dmy(xy$dateRep))
  
# 3.0 Create Graph
covid_data_tbl %>%
  ggplot(aes(as.Date(lubridate::dmy(dateRep)), Cumulative_number, color = countriesAndTerritories)) +
  geom_line(size = 1) +

labs(
  title = str_glue("Cumulative COVID-19 cases per wodlwide"),
  subtitle = str_glue(
    "Start: {(min(lubridate::dmy(covid_data_tbl$dateRep)))}
               End:  {(max(lubridate::dmy(covid_data_tbl$dateRep)))}"),
  x = "Year 2020",
  y = "Cumulative Cases"
) +
  
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 month")) +
  
  scale_color_manual("Countries:", values=c("red", "blue", "green", "purple", "black"), labels = waiver())+
  guides(col = guide_legend(nrow = 2))+
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic", color = "blue4")
  ) +
  geom_label_repel(aes(x = x, y = y, label = label), data = xy)
