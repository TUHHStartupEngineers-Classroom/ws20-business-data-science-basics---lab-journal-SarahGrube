# 1.0 Load Libraries
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 2.0 Call the website
# model names and prices for at least one category
url_rennrad          <- "https://www.rosebikes.de/fahrr%C3%A4der/rennrad/race/x-lite-four-disc"
html_rennrad         <- read_html(url_rennrad)

# 3.0 Get the data
# 3.1 Store the names of the bike model
rennrad_tbl <- html_rennrad %>%
  html_nodes(css = ".catalog-category-model__title") %>%
  html_text() %>%
  stringr::str_extract("(?<=\\n).*(?=\\n)")

# 3.2 Store the prices
prices_tbl <- html_rennrad %>%
  html_nodes(css = ".catalog-category-model__price-current-value") %>%
  html_text() %>%
 stringr::str_extract("(?<=\\n).*(?=\\n)")
 # as.numeric()

prices_tbl <- gsub('[???]', '', prices_tbl)
prices_tbl <- gsub('[]', '', prices_tbl)
prices_tbl <- gsub("[[:space:]]", "", prices_tbl) 
prices_tbl <- as.numeric(prices_tbl)


prices_tbl <- mutate(prices_tbl, )
# 4.0 create a list with bike-name and price
list2 <- data.frame(Bike_name=rennrad_tbl,
                   price.in.Euros =prices_tbl)
list2 <- format(list2, justify = "left")
print(list2,right=F)