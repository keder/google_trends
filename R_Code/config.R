# Runs prediction model for each trend and calculates MSE for it
run_predictions <- TRUE
# Checks if selected rate could be predicted by trend
granger_test <- TRUE
# Runs cross-correlation function to compare trend and selected rate
cross_correlation <- TRUE
# Date cutoffs
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2022-12-31")
# Where input data is stored
data_dir <- "../data"
incidence_trend_dir <- paste(data_dir, "incidence", sep = "/")
mortality_trend_dir <- paste(data_dir, "mortality", sep = "/")
hopkins_path <- paste(data_dir, "hopkins", "timeseries", sep = "/")
hopkins_incidence_path <- paste(hopkins_path, "time_series_covid19_confirmed_US.csv", sep = "/")
hopkins_mortality_path <- paste(hopkins_path, "time_series_covid19_deaths_US.csv", sep = "/")
overall_causes_mortality <- paste(mortality_trend_dir, "total_mortality.csv", sep = "/")
population_path <- paste(data_dir, "population.csv", sep = "/")
# Where output plots to
plot_dir <- "../plots"
# Other output files' location
output_dir <- "../R_Output"
incidence_trend_table_file <- paste0(output_dir, "/", "incidence_trend.RData")
mortality_trend_table_file <- paste0(output_dir, "/", "mortality_trend.RData")
incidence_table_file <- paste0(output_dir, "/", "incidence.RData")
mortality_table_file <- paste0(output_dir, "/", "mortality.RData")
incidence_analysis_result_file <- paste0(output_dir, "/", "incidence_analysis_result.RData")
mortality_analysis_result_file <- paste0(output_dir, "/", "incidence_analysis_result.RData")
# Prediction method
# method <- "arima_pred"
# method <- "prophet_pred"
method <- "subwave_pred"
# How many month to predict, counting from the end of time series
prediction_length <- 33
# Verbose printing
print_results <- TRUE
# How much best predicting trends to select
trend_top_count <- 4
state_top_count <- 3
# heatmap_palette <- c("#0571b0", "white", "#ca0020")
heatmap_palette <- c("#e66101", "white", "#5e3c99")
# fit_lengths <- c(8, 10, 12)
fit_lengths <- c(12)
prediction_horizons <- c(2, 2)
cores <- 1
# Sets size of the bootstrap sample
# Boostrap size of 0 turns off bootstrapping
# bootstrap_size <- 0
bootstrap_size <- 10
# model_params <- list(
#     arima_pred = list(max.p=12)
# )
# Use cumulative or differential data? Note that input data ALWAYS should be differential (incidence)!
# For ARIMA it is advised to use differential, for Prophet it is cumulative.
cumm_data <- TRUE
prediction_plots <- TRUE
# You can select transformation of data before fit and after fit/prediction. Values: "boxcox" and "disabled". 
# For prophet it is strongly recommended to disable transformation.
# transformation <- "boxcox"
transformation <- "disabled"

make_side_by_side_plot_style <- list(
    incidence = list(legend_position = rep("topleft", trend_top_count * state_top_count)),
    mortality = list(legend_position = c(rep("topright", trend_top_count * 2), rep("topleft", trend_top_count)))
)
