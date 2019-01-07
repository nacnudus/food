# Extract the nutritional information from each product's page

library(XML)
library(tidyverse)
library(unpivotr)
library(htmltab)
library(reticulate)
library(fs)
library(here)

nutrients_path_rds <- path(here(), "data", "nutrients.Rds")
nutrients_path_tsv <- path(here(), "data", "nutrients.tsv")
items_dir <- path(here(), "items")

# conda create -n "food" python=3
# pip install quantulum3
use_condaenv("food", conda = "auto", required = TRUE)
parser <- import("quantulum3")$parser
# quant <- parser$parse('I want 22.2 liters of wine')
# quant <- parser$parse('energy (kj) 12')
# quant <- parser$parse('energy kj 12')
# quant <- parser$parse('<1g')
# quant <- parser$parse('12 (kg)')
# quant[[1]]$value
# quant[[1]]$unit$name

# Extract the nutritional information

empty_tibble <- tibble(nutrient = character(0L),
                       portion = character(0L),
                       amount = character(0L))

parse_table <- function(x, filename) {
  if (!length(xmlChildren(x))) {
    return(empty_tibble)
  }
  x %>%
    htmltab(which = 1, rm_nodata_cols = FALSE) %>%
    as_cells(col_names = TRUE) %>%
    behead("NNW", "portion") %>%
    behead("WNW", "nutrient") %>%
    rename(amount = chr) %>%
    select(nutrient, portion, amount)
}

get_nutrition_table <- function(filename) {
  cat(filename, "\n")
  # tables <-
  filename %>%
    htmlParse() %>%
    getNodeSet('//*[contains(concat(" ",normalize-space(@class)," ")," productText ")]//*[contains(concat(" ",normalize-space(@class)," ")," nutritionTable ")]') %>%
    map_df(parse_table) %>%
    bind_rows(empty_tibble)
}

parse_amount <- function(nutrient, amount, stop = FALSE) {
  quant <- parser$parse(amount)
  if (!length(quant)) {
    return(list(nutrient = NA_character_, amount = NA_real_, unit = NA_character_))
  }
  unit <- quant[[1]]$unit$name
  if (stop || unit != "dimensionless") {
    return(list(nutrient = nutrient, amount = quant[[1]]$value, unit = unit))
  }
  # assume the unit is the last word of 'nutrient' and swap it to the last word
  # of 'amount', then try again
  unit_word <- str_extract(nutrient, "[^a-z]?[a-z]+[^a-z]?$")
  nutrient <- str_replace(nutrient, "[^a-z]?[a-z]+[^a-z]?$", "")
  amount <- paste(amount, unit_word)
  parse_amount(nutrient, amount, stop = TRUE)
}

get_nutrition <- function(in_file, out_file) {
  in_file %>%
    get_nutrition_table() %>%
    dplyr::filter(str_detect(portion, "100")) %>% # only keep units per 100
    dplyr::filter(!str_detect(portion, "RI")) %>% # only keep units per 100
    dplyr::filter(!str_detect(portion, regex("recommended intake", ignore_case = TRUE))) %>% # only keep units per 100
    dplyr::filter(str_detect(amount, regex("[0-9]"))) %>% # omit non-data rows
    dplyr::filter(!amount == nutrient) %>% # omit title rows filled across columns
    mutate(nutrient = tolower(nutrient),
           nutrient = str_replace_all(nutrient, "[\\(\\)]", ""), # omit '(', ')'
           nutrient = str_replace_all(nutrient, "(?<=.)/.*", ""), # omit everything after '/'
           amount = tolower(amount),
           amount = str_replace_all(amount, ",", "."), # replace ',' decimal marks
           amount = str_replace_all(amount, "[\\(\\)]", ""),   # omit '(', ')'
           amount = str_replace_all(amount, "(?<=.)/.*", ""), # omit everything after '/'
           amount = if_else(str_detect(amount, "trace"), "0", amount), # trace
           amount = if_else(str_detect(amount, "<"), "0", amount), #     trace
           quant = map2(nutrient, amount, parse_amount),
           nutrient = map_chr(quant, ~ .x$nutrient),
           amount = map_dbl(quant, ~ .x$amount),
           unit = map_chr(quant, ~ .x$unit)) %>%
    dplyr::filter(unit != "dimensionless") %>% # Also filters out NA
    select(-quant) %>%
    mutate(filename = in_file) %>%
    select(filename, nutrient, unit, amount) %>%
    write_tsv(out_file, append = TRUE)
}

files <- dir_ls(items_dir)

write_tsv(tibble(filename = character(),
                 nutrient = character(),
                 unit = character(),
                 amount = numeric()),
          nutrients_path)

nutrients <-
  files %>%
  sort() %>%
  map2_df(nutrients_path, get_nutrition) %>%
  print(n = Inf)

saveRDS(nutrients, nutrients_path_rds)
write_tsv(nutrients, nutrients_path_tsv)
nutrients <- readRDS(nutrients_path_rds)
