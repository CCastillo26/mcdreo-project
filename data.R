library(tidyverse)

mcdreo_df <- read.csv("/Users/charlycastillo/Downloads/mcdreo.csv")

#` Filter and augment MCDREO data
#' @description Filter and augument MCDREO data for interest variables
#' @param mcdreo_df data frame
#' @return data frame with four columns
filter_data <- function(mcdreo_df) {
  oil_regions <- c("Caucasus and Central Asia Oil Exporters",
                   "Caucasus and Central Asia Oil Importers",
                   "Middle East and North Africa, Afghanistan, Pakistan Oil Exporters",
                   "Middle East and North Africa, Afghanistan, Pakistan Oil Importers",
                   "Middle East and North Africa, Afghanistan, Pakistan Non-GCC Oil Exporters")
  
  gdp_indicator <- "Gross domestic product (GDP), Constant prices, Percent change"
  
  year <- names(mcdreo_df)[names(mcdreo_df) %in% as.character(2000:2030)]
  
  mcdreo_df %>%
    filter(COUNTRY %in% oil_regions, 
           INDICATOR == gdp_indicator, 
           FREQUENCY == "Annual") %>% # Source: Stack Overflow
    pivot_longer(cols = year, names = "year", values_to = "gdp_growth") %>%
    mutate(year = year, gdp_growth = gdp_growth, region = COUNTRY,
           period = if_else(year < 2020, "pre2020", "post2020")) %>%
    select(region, year, period, gdp_growth) %>%
    arrange(region, year)
  
}

