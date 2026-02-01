library(tidyverse)
library(xml2)
library(purrr)

extract_numeric <- function(doc) {
  if (is.null(doc)) return(tibble())
  
  # 1. Find all nonFraction nodes, namespace-agnostic
  nodes <- xml2::xml_find_all(doc, ".//*[local-name()='nonFraction']")
  
  # 2. Extract attributes
  names <- xml2::xml_attr(nodes, "name")
  contexts <- xml2::xml_attr(nodes, "contextRef")
  continued <- xml2::xml_attr(nodes, "continuedAt")
  
  # 3. Extract main text
  main_text <- xml2::xml_text(nodes)
  
  # 4. Resolve continuation blocks
  continuation_text <- purrr::map_chr(continued, function(id) {
    if (is.na(id)) return("")
    cont_node <- xml2::xml_find_first(
      doc,
      paste0(".//*[local-name()='continuation' and @id='", id, "']")
    )
    if (is.na(cont_node)) return("")
    xml2::xml_text(cont_node)
  })
  
  # 5. Combine main + continuation
  combined <- paste0(main_text, continuation_text)
  
  # 6. Clean numeric strings
  cleaned <- combined |>
    stringr::str_replace_all("[^0-9\\-\\.]", "") |>
    stringr::str_trim()
  
  # 7. Convert to numeric
  values <- suppressWarnings(as.numeric(cleaned))
  
  tibble(
    name = names,
    context = contexts,
    value = values
  )
}

extract_text <- function(doc) {
  if (is.null(doc)) return(tibble())
  nodes <- xml2::xml_find_all(doc, ".//*[local-name()='nonNumeric']")
  tibble(
    name = xml2::xml_attr(nodes, "name"),
    context = xml2::xml_attr(nodes, "contextRef"),
    value = xml2::xml_text(nodes)
  )
}