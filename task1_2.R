library(tidyverse)
library(readxl) # excel-files
library(magrittr)
library(writexl)
library(lubridate)

# 6.2 Sales by State and Year ----

# Step 1 - Manipulate
sales_by_year_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order.date, total_price, state) %>%
  mutate(year = year(order.date)) %>%
  
  # Group by state and summarize year and main catgegory
  group_by(state, year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_year_state_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y =sales, fill = state)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(legend.position = "none")+
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by state and year",
    fill = "Main category" # Changes the legend name
  )