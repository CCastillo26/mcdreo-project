source("data.R")
library(tidyverse)

mcdreo_df <- filter_data(mcdreo_df)

#` Plot growth
#' @description Plot simulated, IMF projected growths
#' @param mcdreo_df data frame
#' @param sim_df data frame
#' @return ggplot object
plot_growth <- function(mcdreo_df, sim_df) {
  start_year <- 2020
  
  # Split IMF data
  pre_data <- mcdreo_df[mcdreo_df$year < start_year, ]
  post_data <- mcdreo_df[mcdreo_df$year >= start_year, ]
  
  
}