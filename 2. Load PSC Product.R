######### Initialise ########## 

library(jsonlite)
library(tidyverse)
library(janitor)

######## Load the data ########## 

# Data downloaded from https://download.companieshouse.gov.uk/en_pscdata.html
# The PSC file is an array of json objects - it is not a single json file, so need to use jsonlite::stream_in().
# readLines allows us to set a limit if we want - recommended for testing as the dataset is very large 
# Ensure flatten = TRUE as there are nested columns 

lines <- readLines("data/persons-with-significant-control-snapshot-2026-01-21.txt", n = 1000000)

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
sic_master <- readRDS("data/crn_sic_master.rds")

# Subset to relevant cols
verification_subset <- 
  psc_data %>% 
  select(company_number, name, kind, ceased, description, country_of_residence, is_sanctioned, address_region, contains("identi"), contains("sic")) %>% 
  filter(rowSums(!is.na(across(everything()))) > 3) %>% # drop anything that has mostly missing data
  convert_iso8601_cols() 

# Filter for verified population
verified_population <- 
  verification_subset %>% 
  filter(!is.na(identity_verification_details_identity_verified_on)) %>% 
  add_count(identification_country_registered, name = "country_count") 
  
top_verfied_countries <-
  verified_population %>% 
  select(identification_country_registered, country_count) %>% 
  unique() %>% 
  arrange(desc(country_count)) %>% 
  head(10)

ggplot(top_verfied_countries, aes(x=identification_country_registered, y= country_count))+
  geom_col()

# Filter for unverified population
unverified_population <- 
  anti_join(verification_subset, verified_population, by = c("company_number", "name")) 


