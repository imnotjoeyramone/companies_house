######### Initialise ########## 

library(tidyverse)
library(xml2)
library(purrr)

######################################################################
### NB. This is under-development, and not functioning as I'd like ###
######################################################################

# I have saved a sample of accounts data in the below folder - get a list of all html files in there
# Set full.names to TRUE otherwise full filepaths not retained and mapping will fail 
files <- list.files("data/SAMPLE_Accounts_Monthly_Data-December2025", full.names = TRUE, recursive = TRUE)

# test
doc <- xml2::read_xml(files[1])
xml2::xml_ns(doc)
xml2::xml_find_all(doc, ".//*") |> head(50)

# extract the files using read_xml to a list
docs <- map(files, read_xml)

source("functions/xml_extraction.R") # xml text and numeric extraction functions

# create a tibble: original filepath, xml doc, extracted numeric values, extracted text values
accounts <- tibble::tibble(
  file = files,
  xml = docs,
  numeric = map(xml, extract_numeric),
  text = map(xml, extract_text))
