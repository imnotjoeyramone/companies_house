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

psc_pcde_list <- read_rds("data/psc_pcde_lkp.rds") 

psc_plus_company_pcde_list <- 
  company_pcde_list %>% 
  inner_join(psc_pcde_list, by = "company_number") %>% 
  mutate(pcde_same = replace_na(ifelse(reg_address_post_code == address_postal_code, TRUE, FALSE), FALSE))

sum(psc_plus_company_pcde_list$pcde_same/nrow(psc_plus_company_pcde_list))
  
# uprns <- vroom("data/osopenuprn_202601.csv") %>% clean_names()

# ONS postcode database
ons_pcde <- vroom("data/ONSPD_AUG_2025_UK.csv") %>% clean_names()

get_coords <- 
  psc_plus_company_pcde_list %>% 
  left_join(ons_pcde, by = c("reg_address_post_code" = "postcode_7_char")) %>% 
  select(company_name, company_number, reg_address_post_code, address_postal_code, incorporation_date, lat_reg_add = latitude, long_reg_add = longitude) %>%
  left_join(ons_pcde, by = c("address_postal_code" = "postcode_7_char")) %>% 
  rename(lat_psc_add = latitude, lon_psc_add = longitude) %>% 
  filter(incorporation_date >= as.Date("01/01/2025")) %>% 
  slice_sample(n=1000) %>% 
  unique() 
  
leaflet(get_coords) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude)

