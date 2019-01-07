# # On first use, install docker
# sudo pacman -S docker
# sudo tee /etc/modules-load.d/loop.conf <<< "loop"
# sudo modprobe loop

# # Manually install splash, or use install_splash()
# sudo docker pull scrapinghub/splash
# sudo docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash

# # You might need to manually install magick, because it doesn't seem to be
# # managed as a dependency
# install.packages("magick")

options(stringsAsFactors = FALSE)

library(tidyverse)
library(splashr)
library(rvest)
library(stringr)
library(stringi)
library(dtplyr)
library(unpivotr)

splash_vm <- start_splash()

host <- splash("localhost", 8050L)
splash_active(host)

render_save <- function(splash_obj, url, wait) {
  filename <-
    url %>%
    str_extract("groceries[^\"]*") %>%
    str_replace_all("/", "|") %>%
    paste0(".html")
  filename <- paste0("shelves/", filename)
  if (file.exists(filename)) {
    cat("Exists: ", filename, "\n")
    x <- read_html(filename)
  } else {
    cat("Requesting: ", filename, "\n")
    x <- render_html(splash_obj = splash_obj, url = url, wait = wait)
    write_xml(x, filename)
  }
  x
}
render <- partial(render_save, splash_obj = host)

home <- render("http://www.sainsburys.co.uk/shop/gb/groceries", wait = 2)

categories <-
  home %>%
  html_nodes(".subNav a , .subNav strong") %>%
  html_attr("href")

# Filter out non-food categories
nongroceries <- c("http://www.sainsburys.co.uk/shop/gb/groceries/health-beauty",
                  "http://www.sainsburys.co.uk/shop/gb/groceries/baby",
                  "http://www.sainsburys.co.uk/shop/gb/groceries/household",
                  "http://www.sainsburys.co.uk/shop/gb/groceries/pet",
                  "http://www.sainsburys.co.uk/shop/gb/groceries/home")
categories <- categories[!(categories %in% nongroceries)]

# saveRDS(categories, "categories.Rds")

# Takes 2 minutes (13 * 2 + the rest)
products <-
  data_frame(category = categories[!(categories %in% nongroceries)],
             category_html = map(category,
                                   function(x) {
                                     try({
                                       render(x, wait = 2)
                                     })
                                   }),
             department = map(category_html,
                        function(x) {
                          try({
                            x %>%
                              html_nodes(".departments a") %>%
                              html_attr("href")
                          })
                        }))

# saveRDS(products, "products.Rds")

products2 <-
  products %>%
  select(category, department) %>%
  unnest

# Takes 4 minutes (120 * 2 / 60)
products2$department_html <- map(products2$department,
                                 function(x) {try(render(x, wait = 2))})

# saveRDS(products2, "products2.Rds")

# Expand into aisles, where applicable
products3 <-
  products2 %>%
  mutate(expand_department = map_lgl(department_html,
                                     function(x) {
                                       try({
                                         x %>%
                                           html_nodes(".departments span") %>%
                                           length %>%
                                           `>`(0)
                                       })
                                     }),
         aisle = if_else(expand_department,
                         map(department_html,
                             function(x) {
                               x %>%
                                 html_nodes(".aisles a") %>%
                                 html_attr("href")
                             }),
                         map(department_html, ~ NA_character_))) %>%
  select(category, department, aisle) %>%
  unnest

# Put aside departments that don't expand into aisles
departments <- filter(products3, is.na(aisle))
others <- filter(products3, !is.na(aisle))

# There should be 33
departments$department

# Expand into shelves, where applicable
# Takes 20 minutes (and the rest) (740 * 2 / 60)
products4 <-
  others %>%
  mutate(aisle_html = map(aisle, ~ try(render(.x, wait = 2))))

# saveRDS(products4, "products4.Rds")

products5 <-
  products4 %>%
  mutate(expand_aisle = map_lgl(aisle_html,
                                function(x) {
                                  try({
                                    x %>%
                                      html_nodes(".aisles span") %>%
                                      length %>%
                                      `>`(0)
                                  })
                                }),
         shelf = if_else(expand_aisle,
                         map(aisle_html,
                             function(x) {
                               x %>%
                                 html_nodes(".shelf a") %>%
                                 html_attr("href")
                             }),
                         map(aisle_html, ~ NA_character_))) %>%
  select(category, department, aisle, shelf) %>%
  unnest

# Put aside aisles that don't expand into shelves
aisles <- filter(products5, is.na(shelf))
others <- filter(products5, !is.na(shelf))

# Combine shelves at all levels
shelves <-
  bind_rows(mutate(departments, aisle = department, shelf = department),
            mutate(aisles, shelf = aisle),
            others) %>%
  mutate(shelf = paste0(shelf, "?pageSize=100")) # 100 is the max, by experiment

# Get the goods on the shelves
# Takes 20 minutes (and the rest) (1165 * 1 / 60)
goods <-
  shelves %>%
  mutate(shelf_html = map(shelf, ~ try(render(.x, wait = 1))))

# saveRDS(goods, "goods.Rds")

# Look for the number of results.  If it's missing, request the page again
goods <-
  goods %>%
  mutate(n_text = map_chr(shelf_html,
                 function(x) {
                   x %>%
                     html_node("#resultsHeading") %>%
                     html_text %>%
                     str_replace("-", "")}), # Confuses parse_number with minus
         n = as.integer(parse_number(n_text))) %>%
  filter(!is.na(n_text)) # no products available

# Shelves with more then 100 items won't have more than 100 links on them, so
# the items will be missed, but hopefully they'll mostly appear on other shelves
# instead.
goods %>%
  filter(n > 100) %>%
  select(shelf, n) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

items <-
  goods %>%
  mutate(item = map(shelf_html,
                    function(x) {
                      x %>%
                        html_nodes(".productNameAndPromotions a") %>%
                        html_attr("href")
                    })) %>%
  select(category, department, aisle, shelf, item) %>%
  unnest

# saveRDS(items, "items.Rds")
# items <- readRDS("items.Rds")

# length(unique(items$item))
# [1] 45328

# Modify render to not keep the html in memory
render_save <- function(splash_obj, url, wait) {
  filename <-
    url %>%
    str_extract("groceries[^\"]*") %>%
    str_replace_all("/", "|") %>%
    paste0(".html")
  if (filename == "NA.html") {
    stop("url: ", url, "\n")
  }
  if (file.exists(filename)) {
    cat("Exists: ", filename, "\n")
  } else {
    cat("Requesting: ", filename, "\n")
    x <- render_html(splash_obj = splash_obj, url = url, wait = wait)
    write_xml(x, filename)
  }
}
render <- partial(render_save, splash_obj = host)

# 6 hours?  Maybe 12?
items <-
  items %>%
  mutate(short = paste0("http://www.sainsburys.co.uk/shop/gb/groceries",
                        stri_extract_last_regex(item, "/[^/]+$")))

length(unique(items$short))

walk(items$short, ~ try(render(.x, wait = 1)))

stop_splash(splash_vm)
