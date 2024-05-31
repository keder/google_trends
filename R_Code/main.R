source(paste(getwd(), "config.R", sep = "/"))
source(paste(getwd(), "data_load.R", sep = "/"))
source(paste(getwd(), "forecasting.R", sep = "/"))
# source(paste(getwd(), "calc_error.R", sep = "/"))
source(paste(getwd(), "make_plot.R", sep = "/"))
source(paste(getwd(), "analysis.R", sep = "/"))
library(xtable)
library(lmtest)
library(dplyr)
library(pheatmap)
library(grid)
# library(fuzzySim)
library(tidyr)
library(parallel)

set.seed(1)

aggregate_from_files <- function(aggregated_file, files) {
    result <- NULL
    if (file.exists(aggregated_file)) {
        df_name <- load(file = aggregated_file)
        result <- get(df_name[1])
        return(result)
    }
    for (file in files) {
        tmp_namespace <- new.env()
        df_name <- load(file = file, envir = tmp_namespace)
        tmp_result <- tmp_namespace$result
        if (is.null(result)) {
            result <- tmp_result
        } else {
            result <- rbind(result, tmp_result)
        }
    }
    save(result, file = aggregated_file)
    return(result)
}

predict_and_analyze_trends <- function(rate_name, rate, trend) {
    synchronize <- function(result_parallel) {
        while (!all(sapply(result_parallel, function(x) inherits(x, "try-error")))) {
            Sys.sleep(1)
        }
    }

    line_handle <- function(kw, state, file) {
        selected_trend <- trend[trend$kw == kw & trend$state == state, ]
        prediction_result <- NULL
        if (file.exists(file)) {
            df_name <- load(file = file)
            prediction_result <- get(df_name[1])
        }

        prediction_result <- collect_mses(
            prediction_result,
            rate,
            selected_trend,
            prediction_horizons,
            method,
            fit_lengths,
            # c(18),
            callback = save, # save results after every single interation
            file = file
        )

        return(1)
    }

    kws <- unique(trend$kw)
    # states <- unique(trend$state)
    states <- head(unique(trend$state), 4)
    params <- expand.grid(kw = kws, state = states, stringsAsFactors = FALSE)
    params$file <- paste0(output_dir, "/", rate_name, "_", method, "_result_", 1:nrow(params), ".RData")

    if (cores == 1) {
        result_parallel <- mapply(line_handle, params$kw, params$state, params$file)
    } else {
        result_parallel <- mcmapply(line_handle, params$kw, params$state, params$file, mc.cores = cores)
        synchronize(result_parallel)
    }

    result <- aggregate_from_files(
        paste0(output_dir, "/", rate_name, "_", method, "_result.RData"),
        params$file
    )
    return(result)
}


population <- load_population(population_path)
population <- population[population$year == 2020, c("population", "state")]

if (!file.exists(incidence_table_file)) {
    incidence <- load_hopkins_timeseries(hopkins_incidence_path)
    incidence <- divide_by_population(incidence, population)
    incidence <- trim_by_dates(incidence)
    save(incidence, file = incidence_table_file)
} else {
    load(file = incidence_table_file)
}
# desc_stat_incidence <- get_rate_descriptive_statistics(incidence, "incidence")

if (!file.exists(mortality_table_file)) {
    specific_mortality <- load_hopkins_timeseries(hopkins_mortality_path)
    specific_mortality <- divide_by_population(specific_mortality, population)
    specific_mortality <- trim_by_dates(specific_mortality)
    save(incidence, file = mortality_table_file)
} else {
    load(file = mortality_table_file)
}
# desc_stat_specific_mortality <- get_rate_descriptive_statistics(specific_mortality, "mortality")

if (!file.exists(incidence_trend_table_file)) {
    incidence_trend <- load_trends(incidence_trend_dir)
    incidence_trend <- trim_by_dates(incidence_trend)
    save(incidence_trend, file = incidence_trend_table_file)
} else {
    load(file = incidence_trend_table_file)
}
# incidence_trend_statistics <- get_trend_descriptive_statistics(incidence_trend, "incidence_trend")

if (!file.exists(incidence_analysis_result_file)) {
    incidence_analysis_result <- ts_analyze("incidence", incidence, incidence_trend, population)
    save(incidence_analysis_result, file = incidence_analysis_result_file)
} else {
    load(file = incidence_analysis_result_file)
}

if (!file.exists(mortality_trend_table_file)) {
    mortality_trend <- load_trends(mortality_trend_dir)
    mortality_trend <- trim_by_dates(mortality_trend)
    save(mortality_trend, file = mortality_trend_table_file)
} else {
    load(file = mortality_trend_table_file)
}

if (!file.exists(mortality_analysis_result_file)) {
    mortality_analysis_result <- ts_analyze("mortality", specific_mortality, mortality_trend, population)
    save(incidence_analysis_result, file = mortality_analysis_result_file)
} else {
    load(file = mortality_analysis_result_file)
}
# mortality_trend_statistics <- get_trend_descriptive_statistics(mortality_trend, "mortality_trend")

# Select several top keywords
selected_incidence_kws <- select_top_trends(incidence_analysis_result)
# Select several bottom keywords for comparison
# selected_incidence_kws <- c(selected_incidence_kws, select_top_trends(incidence_analysis_result, order = 1))
# print(selected_incidence_kws)
# readline()
selected_incidence_kws <- c("coronavirus", "covid_test")

prediction_result_file_name <- paste0(output_dir, "/incidence_", method, "_result.RData")
incidence_trend <- incidence_trend[incidence_trend$kw %in% selected_incidence_kws, ]
prediction_result <- NULL
if (file.exists(prediction_result_file_name)) {
    df_name <- load(file = prediction_result_file_name)
    prediction_result <- get(df_name[1])
}

if (run_predictions) {
    prediction_result <- predict_and_analyze_trends("incidence", incidence, incidence_trend)
}

output_prediction_result(prediction_result, paste0(output_dir, "/incidence/", method, "_result"))
