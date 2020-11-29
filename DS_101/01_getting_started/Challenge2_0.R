library(RSQLite)
library(dplyr)
library(httr)
library(glue)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "DS_101/00_data/02_chinook/Chinook_Sqlite.sqlite")
dbListTables(con)
tbl(con, "Album")
album_tbl <- tbl(con, "Album") %>% collect()
x <- dbGetQuery(con, 'SELECT * FROM Artist')
dbDisconnect(con)
con
#
name <- "Fred"
glue('My name is {name}.')
#
resp <- GET("https://swapi.dev/api/people/1/")
# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp
# convert the raw Unicode into a character vector that resembles the JSON format
rawToChar(resp$content)
# R Lists
data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)
library(jsonlite)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()
#
resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp

token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response
#


#
# Access to Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"


# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()

#
# acces to top rates movies
url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()

# get the ranks
rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()

# get the title
title <-  html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()

# get the year
year <- html %>% 
  html_nodes(".titleColumn .secondaryInfo") %>%
  html_text() %>% 
  # Extract numbers
  stringr::str_extract(pattern = "[0-9]+") %>% 
  as.numeric

#people
people <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_attr("title")

# rating
rating <- html %>% 
  html_nodes(".imdbRating > strong") %>%
  html_text() 
  as.numeric()
  
# number of ratings
  num_ratings <- html %>% 
    html_nodes(css = ".imdbRating > strong") %>% 
    html_attr('title') %>% 
    # Extract the numbers and remove the comma to make it numeric values
    stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
    stringr::str_replace_all(pattern = ",", replacement = "") %>% 
    as.numeric()
  
  # merge everything itnto a tibble
  imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)
