library(tidyverse)

mcdreo_df <- read.csv("/Users/charlycastillo/Downloads/mcdreo.csv", 
                      check.names = FALSE) # Fix year column names

#` Filter and augment MCDREO data
#' @description Filters and augments MCDREO data for interest variables
#' @param mcdreo_df data frame
#' @return data frame with four columns
filter_data <- function(mcdreo_df) {
  oil_regions <- c("Caucasus and Central Asia Oil Exporters",
                   "Caucasus and Central Asia Oil Importers",
                   "Middle East and North Africa, Afghanistan, Pakistan Oil Exporters",
                   "Middle East and North Africa, Afghanistan, Pakistan Oil Importers",
                   "Middle East and North Africa, Afghanistan, Pakistan Non-GCC Oil Exporters")
   
  gdp_indicator <- "Gross domestic product (GDP), Constant prices, Percent change"
  
  year_names <- as.character(2000:2030)
  year_cols <- which(names(mcdreo_df) %in% year_names)
  
  mcdreo_df %>%
    filter(COUNTRY %in% oil_regions, 
           INDICATOR == gdp_indicator, 
           FREQUENCY == "Annual") %>% # Source: Stack Overflow
    pivot_longer(cols = year_cols, names_to = "year", 
                 values_to = "gdp_growth") %>%
    mutate(year = as.numeric(year), gdp_growth = as.numeric(gdp_growth), 
           region = COUNTRY, period = if_else(year < 2020, "pre2020", "post2020")) %>%
    select(region, year, period, gdp_growth) %>%
    arrange(region, year)
}
