########## Initialise

library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)

# This script shows how to use Companies House REST API to retrieve company information from Companies House
# The API is not intended for bulk data extraction - instead, download the bulk data products for full register
# Trying to retrieve too much data will likely result in a cutoff/failure

key <- Sys.getenv("companies_house_api_key") # key saved in .renviron - do not hard code or publish to github!

########## Example A: search single CRN to retrieve company profile

crn <- "00000006" # crn
url <- paste0("https://api.company-information.service.gov.uk/company/", crn) # url for search
response <- httr::GET(url, authenticate(key, "")) # send the request - authentication key, blank password ""
result <- fromJSON(content(response, "text", encoding = "UTF-8")) # extract the response 

########## Example B: search multiple CRNs to get multiple company profiles

company_numbers <- c("00000006", "00000007", "00000008")

get_companies <- function(crn) {
  url <- paste0("https://api.company-information.service.gov.uk/company/", crn)
  res <- GET(url, authenticate(key, ""))
  fromJSON(content(res, "text", encoding = "UTF-8"))
}

records <- lapply(company_numbers, get_companies)
records_df <- do.call(bind_rows, lapply(records, as.data.frame))

########## Example C: Search Company name
# Note that search endpoints have a limit
# Company search limited to 20 results per page, and 1000 results in total
# Therefore write function so that multiple pages of results can be retrieved to the max
# Otherwise only first 20 results will be retrieved 

# Function 1: search
fetch_search_page <- function(query, start_index = 0) {
  url <- paste0(
    "https://api.company-information.service.gov.uk/search/companies?",
    "q=", query,
    "&start_index=", start_index
  )
  
  GET(url, authenticate(key, "")) %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE)
}

# Function 2: retrieve results as df
search_companies_df <- function(query, max_pages = 50) {
  seq(0, by = 20, length.out = max_pages) %>% # create page indexes
    map(~ fetch_search_page(query, start_index = .x)) %>% # repeats query for each index
    keep(~ length(.x$items) > 0) %>% # only keep if results exists - so search ends if no further results
    map("items") %>% # retrieve the items list
    list_rbind() %>%  # bind the elements of the list together
    clean_names() # tidy up col names
}

# Example
company_df <- search_companies_df("tesco")

