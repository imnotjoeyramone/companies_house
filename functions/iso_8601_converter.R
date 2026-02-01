library(tidyverse)
library(lubridate)

# Detect whether a column is ISO-8601 datetime
is_iso8601 <- function(x) {
  if (!is.character(x)) return(FALSE)
  
  # get all non-missing values
  vals <- x[!is.na(x)]
  
  # if no non-missing values, don't treat as date
  if (length(vals) == 0) return(FALSE)
  
  # test the first non-missing value
  first_val <- trimws(vals[1])

  grepl("^\\d{4}-\\d{2}-\\d{2}T", first_val)
}

# Convert all ISO-8601 columns using lubridate
convert_iso8601_cols <- function(df) {
  df %>% mutate(across(where(is_iso8601), ~ lubridate::ymd_hms(.x, quiet = TRUE)))
}