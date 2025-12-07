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
  sims <- 10000
  
  pre_data <- mcdreo_df[mcdreo_df$year < start_year, ]
  
  region_list <- unique(pre_data$region)
  span <- start_year:end_year
  n_years <- length(span)
  
  set.seed(123)
  
  # Use linear model
  linear_sim <- data.frame(region = character(), year = integer(), 
                           sim = integer(), growth = numeric())
  
  for (i in region_list) {
    region_data <- pre_data[pre_data$region == i, ]
    
    mean2 <- mean(region_data$gdp_growth)
    sd2 <- sd(region_data$gdp_growth)
    
    for (s in 1: sims) {
      sim_vals <- rnorm(n_years, mean = mean2, sd = sd2)
      
      new_row <- data.frame(region = i, year = span, sim = s, 
                            growth = sim_vals)
      
      linear_sim <- rbind(linear_sim, new_row)
    }
  }
  
  linear_summary <- linear_sim %>%
    group_by(region, year) %>%
    summarize(mean = mean(growth), lower = quantile(growth, 0.025),
              upper = quantile(growth, 0.975)) %>% # Use 95% interval
    mutate(model = "linear")
  
  # Use ARIMA(1, 0, 0) model - Source: Duke
  arima_summary <- data.frame(region = character(), year = integer(),
                              mean = numeric(), lower = numeric(), 
                              upper = numeric(), model = character())
  
  for (i in region_list) {
    region_data <- pre_data[pre_data$region == i, ]
    
    min_year <- min(region_data$year)
    growth_ts <- ts(region_data$gdp_growth, start = min_year, frequency = 1)
    
    fit <- arima(growth_ts, order = c(1, 0, 0))
    
    forecast <- predict(fit, n.ahead = n_years) # Source: RDocumentation
    
    mean3 <- as.numeric(forecast$pred)
    se <- as.numeric(forecast$se)
    
    # Calculate 95% forecast interval
    lower <- mean3 - 1.96 * se # Use z-score
    upper <- mean3 + 1.96 * se
    
    new_row <- data.frame(region = i, year = span, mean = mean3, lower = lower,
                          upper = upper, model = "arima")
    
    arima_summary <- rbind(arima_summary, new_row)
  }
  
  sim_summary <- rbind(linear_summary, arima_summary)

  sim_summary
}
