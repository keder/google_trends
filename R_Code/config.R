# Runs prediction model for each trend and calculates MSE for it
run_trend_predictions = FALSE
# Checks if selected rate could be predicted by trend
granger_test = TRUE
# Runs cross-correlation function to compare trend and selected rate
cross_correlation = TRUE
# Where input data is stored
data_dir = "../data"
incidence_trend_dir = paste(data_dir, "incidence", sep = "/")
mortality_trend_dir = paste(data_dir, "mortality", sep = "/")
hopkins_path = paste(data_dir, "hopkins", "timeseries", sep = "/")
hopkins_incidence_path = paste(hopkins_path, "time_series_covid19_confirmed_US.csv", sep = "/")
hopkins_mortality_path = paste(hopkins_path, "time_series_covid19_deaths_US.csv", sep = "/")
overall_causes_mortality = paste(mortality_trend_dir, "total_mortality.csv", sep = "/")
population_path = paste(data_dir, "population.csv", sep = "/")
# Where output plots to
plot_dir = "../plots"
# Other output files' location
output_dir = "../R_Output"
# Prediction method
method = "prophet"
# How many month to predict, counting from the end of time series
prediction_length = 33
# Verbose printing
print_results = TRUE
