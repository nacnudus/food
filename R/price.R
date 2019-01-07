# Extract the price and unit size from each product's page

library(tidyverse)
library(unpivotr)
library(rvest)
library(fs)
library(here)

prices_path_rds <- path(here(), "data", "prices.Rds")
prices_path_tsv <- path(here(), "data", "prices.tsv")
items_dir <- path(here(), "items")
files <- dir_ls(items_dir)

# Extract the prices
prices <-
  tibble(filename = as.character(files)) %>%
  mutate(pricing = map(filename,
                       ~ .x %>%
                         read_html() %>%
                         html_nodes(".productSummary .pricing .pricePerUnit, .productSummary .pricing .pricePerMeasure") %>%
                         html_text())) %>%
  unnest() %>%
  mutate(pricing = pricing %>%
         str_replace_all(fixed("\\n"), "") %>%
         str_trim() %>%
         str_split(fixed("/")) %>%
         map(~ `names<-`(.x, c("price", "unit")) %>%
               enframe() %>%
               spread(name, value))) %>%
  unnest() %>%
  mutate(price = parse_number(price),
         price_per_100g = case_when(unit == "100g" ~ price,
                                    unit == "100ml" ~ price,
                                    unit == "10g" ~ price * 10,
                                    unit == "10ml" ~ price * 10,
                                    unit == "150kg" ~ price / 1500,
                                    unit == "18.7cl" ~ price / 18.7,
                                    unit == "25cl" ~ price / 25,
                                    unit == "70cl" ~ price / 70,
                                    unit == "75cl" ~ price / 75,
                                    unit == "80g" ~ price / 80 * 100,
                                    unit == "g" ~ price * 100,
                                    unit == "kg" ~ price / 10,
                                    unit == "ltr" ~ price / 10,
                                    TRUE ~ NA_real_))

saveRDS(prices, prices_path_rds)
write_tsv(prices, prices_path_tsv)
prices <- readRDS(prices_path_rds)
