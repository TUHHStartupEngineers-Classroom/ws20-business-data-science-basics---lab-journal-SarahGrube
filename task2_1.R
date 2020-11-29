library(httr)
library(tibble)
library(jsonlite)
library(tidyverse)

res <- fromJSON('http://ergast.com/api/f1/2004/1/results.json')
drivers <- res$MRData$RaceTable$Races$Results[[1]]$Driver
colnames(drivers)

dirvers_list <- drivers[1:8, c("givenName", "familyName", "code", "nationality", "dateOfBirth")]

dirvers_list <- mutate(dirvers_list, yearOfBirth = year(dateOfBirth))
dirvers_list %>%
  arrange(yearOfBirth)

dirvers_list <- select(dirvers_list, yearOfBirth, nationality, everything()) 
dirvers_list

