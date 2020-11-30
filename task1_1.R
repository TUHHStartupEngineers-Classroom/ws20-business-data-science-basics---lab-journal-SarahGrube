# 1.0 Load Libraries
library(tidyverse)
library(readxl) # excel-files
library(magrittr)
library(writexl)
library(lubridate)

# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 4.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl

# 4.1 Separate category name
bike_orderlines_wrangled_tbl <-  separate(bike_orderlines_wrangled_tbl, 
                                          col    = location,
                                          into   = c("city", "state"),
                                          sep    = ", ")

# 4.2 Add the total price (price * quantity) 
bike_orderlines_wrangled_tbl <- mutate(bike_orderlines_wrangled_tbl, total_price = price * quantity)

# 4.3.4 You can reorder the data by selecting the columns in your desired order.
bike_orderlines_wrangled_tbl <- select(bike_orderlines_wrangled_tbl, city, state, total_price, everything()) 

# 5.0 Business Insights ----
# 5.1 Sales by State ----

# Step 1 - Manipulate
sales_by_location_tbl <- bike_orderlines_wrangled_tbl

# SALES BY STATE
# Grouping by state and summarizing sales
sales_by_state_tbl <- group_by(sales_by_location_tbl, state) 
sales_by_state_tbl<- summarize(sales_by_state_tbl, sales = sum(total_price))

sales_by_state_tbl <- mutate(sales_by_state_tbl, sales_text = scales::dollar(sales, big.mark = ".", 
                                                                             decimal.mark = ",",prefix = "", suffix = " €"))

# Step 2 - Visualize
sales_by_state_tbl %>%
  # Setup canvas with the columns state (x-axis) and sales (y-axis)
  # {r plot, fig.width=10, fig.height=7}
  ggplot(aes(x = state, y = sales)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue per state",
    #subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )