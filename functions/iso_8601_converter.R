library(tidyverse)
library(lubridate)

# Detect whether a column is ISO-8601 datetime
is_iso8601 <- function(x) {
  if (!is.character(x)) return(FALSE)
  first_val <- x[which(!is.na(x))[1]]
  if (is.na(first_val)) return(FALSE)
  grepl("^\\d{4}-\\d{2}-\\d{2}T", first_val)
}

# Convert all ISO-8601 columns in a data frame
convert_iso8601_cols <- function(df) {
  df %>% mutate(across(where(is_iso8601), ~ ymd_hms(.x, quiet = TRUE)))
}