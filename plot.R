source("data.R")
library(tidyverse)
library(patchwork)

#` Plot growth
#' @description Plot simulated, IMF projected growths
#' @param mcdreo_df data frame
#' @param sim_df data frame
#' @return ggplot object
plot_growth <- function(mcdreo_df, sim_df) {
  start_year <- 2020
  
  # Split IMF data (2000-2020, 2020-2030)
  pre_data <- mcdreo_df[mcdreo_df$year < start_year, ]
  post_data <- mcdreo_df[mcdreo_df$year >= start_year, ]
  
  # Split simulation by model
  sim_linear <- sim_df[sim_df$model == "linear", ]
  sim_arima <- sim_df[sim_df$model == "arima", ]
  
  plot <- ggplot() + geom_line(data = pre_data, 
                               aes(x = year, y = gdp_growth), color = "black") +  
    geom_vline(xintercept = start_year, linetype = "dashed") +
    geom_ribbon(data = sim_arima, 
                aes(x = year, ymin = lower, ymax = upper), alpha = 0.2,
                fill = "pink") + # Source: Posit
    geom_line(data = sim_linear, 
              aes(x = year, y = mean, color = "linear"), linetype = "dashed") +
    geom_line(data = sim_arima, 
              aes(x = year, y = mean, color = "arima")) +
    geom_point(data = post_data, aes(x = year, y = gdp_growth), color = "black") +
    facet_wrap(~ region, scales = "free_y", 
               labeller = label_wrap_gen(width = 20)) +  # Wrap region names
    scale_color_manual(name = "Model", values = c(linear = "blue", arima = "red")) +
    labs(x = "Year", y = "Real GDP Growth (%)")
    
    plot
}
