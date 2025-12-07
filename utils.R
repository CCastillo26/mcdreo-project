source("data.R")
source("estimation.R")
source("simulation.R")
source("plot.R")

#` Run full analysis
#' @description Runs full analysis (estimation, simulation, plotting)
#' @return list with data, summary tables, model, simulations, plot
run_analysis <- function() {
  mcdreo_df <- filter_data(mcdreo_df)
  
  # Run estimation.R
  stats_table <- calculate_stats(mcdreo_df)
  test_table <- run_growth(mcdreo_df)
  growth_lm <- fit_growth(mcdreo_df)
  
  # Run simulation.R
  sim_df <- simulate_growth(mcdreo_df)
  
  # Run plot.R
  fig <- plot_growth(mcdreo_df, sim_df)
  
  list(data = mcdreo_df, stats = stats_table, tests = test_table, 
       model = growth_lm, sim = sim_df, plot = fig)
}