# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl) # excel-files
library(magrittr)
#library(readr)
#library(lubridate)

# 2.0 Importing Files ----

bikes_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
#left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----
# have a look at category
bike_orderlines_joined_tbl$category
# print all entries that start with Mountain
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()
# All actions are chained with the pipe already. 
# You can perform each step separately and use glimpse() or View() to validate your code. 
# Store the result in a variable at the end of the steps.
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl

  # 5.1 Separate category name
bike_orderlines_wrangled_tbl <-  separate(bike_orderlines_wrangled_tbl, 
           col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ")

# 5.2 Add the total price (price * quantity) 
# Add a column to a tibble that uses a formula-style calculation of other columns
bike_orderlines_wrangled_tbl <- mutate(bike_orderlines_wrangled_tbl, total.price = price * quantity)

# 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
# 5.3.1 by exact column name
#The first column of the file orderliness.xlsx has no name. R names it then automatically to ...1.
#select(-...1): This command removes (because of the minus in front) that column from the current selection.
bike_orderlines_wrangled_tbl <- select(bike_orderlines_wrangled_tbl, -...1, -gender) 
bike_orderlines_wrangled_tbl <-   select(bike_orderlines_wrangled_tbl, -ends_with(".id"))

# 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
#bike_orderlines_wrangled_tbl <- bind_cols(bike_orderlines_joined_tbl)
#bike_orderlines_wrangled_tbl <- select(bike_orderlines_wrangled_tbl, order.id)
bike_orderlines_wrangled_tbl <- mutate(bike_orderlines_wrangled_tbl, bind_cols(bike_orderlines_joined_tbl %>% select(order.id)))

# 5.3.4 You can reorder the data by selecting the columns in your desired order.
# You can use select_helpers like contains() or everything()
bike_orderlines_wrangled_tbl <- select(bike_orderlines_wrangled_tbl, order.id, contains("order"), contains("model"), contains("category"),
       price, quantity, total.price,
       everything()) 

# 5.4 Rename columns because we actually wanted underscores instead of the dots
# (one at the time vs. multiple at once)
bike_orderlines_wrangled_tbl <- rename(bike_orderlines_wrangled_tbl, bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
library(lubridate)
# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl
  
  # Select columns
sales_by_year_tbl <- select(sales_by_year_tbl, order_date, total_price) 
  
  # Add year column
sales_by_year_tbl <- mutate(sales_by_year_tbl, year = year(order_date))
  
  # Grouping by year and summarizing sales
sales_by_year_tbl <- group_by(sales_by_year_tbl, year) 
sales_by_year_tbl<- summarize(sales_by_year_tbl, sales = sum(total_price))
  
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
sales_by_year_tbl <- mutate(sales_by_year_tbl, sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_tbl

# Step 2 - Visualize
sales_by_year_tbl %>%
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
  

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_cat_1_tbl  

# Step 2 - Visualize
sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")