######### Initialise ########## 

library(tidyverse)
library(janitor)
library(govstyle)
library(vroom)

source("functions/iso_8601_converter.r")
source("functions/dmy_converter.r")

# Data is in csv format, use vroom for speed
company_data <- 
  vroom("data/BasicCompanyDataAsOneFile-2026-01-01.csv", na = c("", "NA", "NULL"), guess_max = 100000) %>% 
  clean_names() 

# The data is quite large, so let's take a sample
company_sample <- 
  company_data %>% 
  slice_sample(n = 100000) %>% 
  convert_dmy_cols()

##### Exploration 1: Incorporation Date #####

# Drop columns we don't need
incorporation_subset <-
  company_sample %>% 
  select(company_name, company_number, incorporation_date, country_of_origin, company_category) %>% 
  mutate(incorporation_year = floor_date(incorporation_date, "year"), .after = incorporation_date) 

# Line plot of incorporations
incorp_count_yr <- 
  incorporation_subset %>% 
  add_count(incorporation_year, name = "n_incorp_year")

plot_incorp_yr <- 
  incorp_count_yr %>% 
  ggplot(aes(x=incorporation_year, y = n_incorp_year))+
  geom_line()+
  theme_gov()+
  xlab("Year of Incorporation")+
  ylab("Count") 

plot_incorp_yr
# This is okay - as the register is live companies, most companies have been registered within the last 10 years 
# Smaller proportions of older companies exist
# but it's hard to see finer trends due to the large span of time covered in the dataset.

# So let's look at post-2000s registrations

plot_millenial_cos <- 
incorp_count_yr %>% 
  filter(incorporation_date >= as.Date("2000-01-01")) %>% 
  ggplot(aes(x=incorporation_year, y = n_incorp_year))+
  geom_line()+
  theme_gov()+
  xlab("Year of Incorporation")+
  ylab("Count")

plot_millenial_cos

# Unsurprisingly, a steady upward trend with a slight dip for 2020.

# What about overseas entities? 
overseas_ents <- 
incorp_count_yr %>% 
  filter(company_category == "Overseas Entity") %>% 
  add_count(incorporation_year, name = "n_incorp_year")

overseas_ents %>% 
  ggplot(aes(x=incorporation_year, y = n_incorp_year))+
  geom_line()+
  theme_gov()+
  xlab("Year of Incorporation")+
  ylab("Count")

# This suggests that overseas entity only became a category in 2022 - however, a sharp upward trend since.