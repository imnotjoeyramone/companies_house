
library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)

get_company <- function(crn, key) {
  url <- paste0("https://api.company-information.service.gov.uk/company/", crn)
  GET(url, authenticate(key, "")) %>% 
    content("text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
}