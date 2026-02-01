library(lubridate)
library(tidyverse)

# Detect if column is dmy format
is_dmy <- is_dmy <- function(x) {
  if (!is.character(x)) return(FALSE)
  
  # get all non-missing values
  vals <- x[!is.na(x)]
  
  # if no non-missing values, don't treat as date
  if (length(vals) == 0) return(FALSE)
  
  # test the *first* non-missing value
  first_val <- trimws(vals[1])
  
  grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", first_val)
}

# Convert all dmy using lubridate
convert_dmy_cols <- function(df) {
  df %>% mutate(across(where(is_dmy), ~ lubridate::dmy(.x, quiet = TRUE)))
}