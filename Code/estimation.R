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
#' @return data frame with five columns
run_growth <- function(mcdreo_df) {
  region_list <- unique(mcdreo_df$region)
  
  # Create empty data frame
  test_results <- data.frame(region = character(), mean_pre = numeric(),
                             mean_post = numeric(), t_statistic = numeric(),
                             p_value = numeric())
  
  for (i in region_list) {
    region_data <- mcdreo_df[mcdreo_df$region == i, ]
    
    pre_growth <- region_data$gdp_growth[region_data$period == "pre2020"]
    post_growth <- region_data$gdp_growth[region_data$period == "post2020"]
    
    t <- t.test(pre_growth, post_growth)
    
    new_row <- data.frame(region = i, mean_pre = mean(pre_growth), 
                          mean_post = mean(post_growth), 
                          t_statistic = as.numeric(t$statistic), 
                          p_value = t$p.value)
    
    test_results <- rbind(test_results, new_row) # Append to data frame
  }
  
  test_results
}


#` Fit growth model
#' @description Fits growth model on region, period, interaction
#' @param mcdreo_df data frame
#' @return linear model
fit_growth <- function(mcdreo_df) {
  lm(gdp_growth ~ region * period, data = mcdreo_df)
}
