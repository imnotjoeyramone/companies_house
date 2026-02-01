library(xml2)
library(purrr)
library(dplyr)
library(tibble)

extract_fact <- function(doc, tag) {
  # if empty return NA
  if (is.null(doc)) return(NA_real_)
  
  # look for the first instance of the tag
  node <- xml2::xml_find_first(
    doc,
    paste0(".//*[local-name()='nonFraction' and @name='", tag, "']")
  )
  
  # if nothing found, return NA
  if (is.na(node)) return(NA_real_)
  
  # otherwise return value
  suppressWarnings(as.numeric(xml2::xml_text(node)))
}