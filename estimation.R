source("data.R")
library(tidyverse)

mcdreo_df <- filter_data(mcdreo_df)

#` Summarize GDP growth
#' @description Summarizes GDP growth with statistics by region, period
#' @param mcdreo_df data frame
#' @return data frame with five columns
calculate_stats <- function(mcdreo_df) {
  mcdreo_df %>%
    group_by(region, period) %>%
    summarize(mean = mean(gdp_growth), sd = sd(gdp_growth), n = n())
}

#` Run t-tests
#' @description Runs t-tests comparing pre, post-2020 growth
#' @param mcdreo_df data frame
#' @return data frame with three columns

# calculate_stats
# run_growth
# fit_growth


# simulate_growth
# plot_growth