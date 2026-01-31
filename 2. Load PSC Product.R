######### Initialise ########## 

library(jsonlite)
library(tidyverse)
library(janitor)
library(assertthat)

######## Load the data ########## 

# Data downloaded from https://download.companieshouse.gov.uk/en_pscdata.html
# The PSC file is an array of json objects - it is not a single json file, so need to use jsonlite::stream_in().
# readLines allows us to set a limit if we want - recommended for testing as the dataset is very large 
# Ensure flatten = TRUE as there are nested columns 

lines <- readLines("data/persons-with-significant-control-snapshot-2026-01-21.txt", n = 100000)
psc_data <- 
  stream_in(textConnection(lines), flatten = TRUE) %>% 
  as_tibble() %>% 
  rename_with(~ sub("^[^.]+\\.", "", .x)) %>% 
  clean_names() 

# Note that natures_of_control column contains a list of values, if the individual has multiple share types. 
# Further tidying needed if analysing this.

######## Exploration #########
# 1. Identity verification overdue 

source("functions/iso_8601_converter.r")

verification_subset <- 
  psc_data %>% 
  select(company_number, name, ceased, description, is_sanctioned, address_region, contains("identi")) %>% 
  filter(rowSums(!is.na(across(everything()))) > 3) %>% # drop anything that has mostly missing data
  convert_iso8601_cols()

verified_population <- 
  verification_subset %>% 
  filter(!is.na(identification_registration_number))

unverified_population <- 
  anti_join(verification_subset, verified_population, by = c("company_number", "name"))

assert_that(nrow(verified_population) + nrow(unverified_population) == nrow(verification_subset)) # test logic 
