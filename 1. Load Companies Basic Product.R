######### Initialise ########## 

library(tidyverse)
library(janitor)
library(data.table)

# Data is in csv format, use fread
company_data <- data.table::fread("data/BasicCompanyDataAsOneFile-2026-01-01.csv")
