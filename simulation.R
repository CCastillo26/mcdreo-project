source("data.R")
library(tidyverse)

mcdreo_df <- filter_data(mcdreo_df)

#` Simulate GDP growth
#' @description Simulates GDP growth using linear, ARIMA models
#' @param mcdreo_df data frame
#' @return data frame with seven columns
simulate_growth <- function(mcdreo_df) {
  start_year <- 2020
  end_year <- 2030
  sims <- 1000
  
  pre_data <- mcdreo_df[mcdreo_df$year < start_year, ]
  
  region_list <- unique(mcdreo_df$region)
  span <- start_year:end_year
  n_years <- length(span)
  
  set.seed(123)
  
  # Use linear model
  linear_sim <- data.frame(region = character(), year = integer(), 
                           sim = integer(), growth = numeric())
  
  for (i in region_list) {
    region_data <- mcdreo_df[mcdreo_df$region == i, ]
    
    mean <- mean(region_data$gdp_growth)
    sd <- sd(region_data$gdp_growth)
    
    for (s in 1: sims) {
      sim_vals <- rnorm(n_years, mean = mean, sd = sd)
      
      new_row <- data.frame(region = i, year = span, sim = s, 
                            growth = sim_vals)
      
      linear_sim <- rbind(linear_sim, new_row)
    }
  }
  
  linear_summary <- linear_sim %>%
    group_by(region, year) %>%
    summarize(mean = mean(growth), lower = quantile(growth, 0.025),
              upper = quantile(growth, 0.975)) %>% # 95% interval
    mutate(model = "linear")
  
  # Use ARIMA model
  arima_summary <- data.frame(region = character(), year = integer(),
                              mean = numeric(), lower = numeric(), 
                              upper = numeric(), model = character())
  
  for (i in region_list) {
    region_data <- pre_data[pre_data$region == i, ]
  }
}

# simulate_growth
# plot_growth