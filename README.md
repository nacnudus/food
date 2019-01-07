# Scrape food nutrition and price from a supermarket

The Office of National Statistics does this too
https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/articles/researchindicesusingwebscrapedpricedata/may2016update/researchindicesusingwebscrapeddatamay2016.pdf

This is the least reproducible work I have ever done.  The scraping was done
overnight near Easter 2016.  If I were to do it again I would use the `polite`
and `furrr` packages.

The nutritional information tables aren't exactly clean, but a surprisingly high
proportion can be cleaned with the `htmltab` and `unpivotr` R packages, and the
`quantulum3` Python library via R's `reticulate`.

## Scripts

1. `R/scrape.R` isn't reproducible but is left here as a starting point for
   anyone wanting to try.
1. `R/nutrition.R` extracts the nutritional information from the scraped pages.
1. `R/price.R` extracts the prices from the scraped pages.

## Data

1. `items` directory of product pages
1. `data/nutrition.tsv`, `data/nurition.Rds` nutritional information for each
   product, by its filename in `items`.
1. `data/price.tsv`, `data/nurition.Rds` price and unit size of each product, by
   its filename in `items`.
