######### Initialise ########## 

library(tidyverse)
library(janitor)
library(vroom)
library(sf)
library(leaflet)

source("functions/dmy_converter.r")

# company pcde list created in 1. Basic Company Data Scoping
company_pcde_list  <- vroom("data/BasicCompanyData.csv") %>% 
  clean_names() %>% 
  select(company_name, company_number, reg_address_post_code, incorporation_date)
  
# uprns <- vroom("data/osopenuprn_202601.csv") %>% clean_names()

# ONS postcode database
ons_pcde <- vroom("data/ONSPD_AUG_2025_UK.csv") %>% clean_names()

get_coords <- 
  company_pcde_list %>% 
  inner_join(ons_pcde, by = c("reg_address_post_code" = "postcode_7_char")) %>% 
  select(company_name, company_number, reg_address_post_code, incorporation_date, latitude, longitude) %>% 
  filter(incorporation_date >= as.Date("01/01/2025")) %>% 
  slice_sample(n=1000) %>% 
  unique() 
  
leaflet(get_coords) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude)