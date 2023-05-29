source(paste(getwd(), "data_load.R", sep = "/"))
source(paste(getwd(), "forecasting.R", sep = "/"))
source(paste(getwd(), "calc_error.R", sep = "/"))
source(paste(getwd(), "make_plot.R", sep = "/"))
source(paste(getwd(), "config.R", sep = "/"))
library(xtable)
library(lmtest)

.trend_predict <- function(result, trend) {
    if (!(trend$kw %in% trend_eval$kw)) {
        trend_eval[nrow(trend_eval) + 1, "kw"] <- trend$kw
    }
    if (is.null(trend$table)) {
        trend_eval[which(trend_eval$kw == trend$kw), trend$state] <- NA
        result <- list(trend_eval = trend_eval, trend = trend)
        return(result)
    }
    table <- trend$table
    table_length <- dim(table)[1]
    fit_length <- table_length - prediction_length
    prediction <- predict_trend(table, prediction_length, paste0(method, "_pred"))
    trend$table$predicted <- prediction
    trend$table$predicted[is.na(trend$table$predicted)] <- 0
    file_name <- paste0(plot_dir, "/", method, "_trend_prediction_", trend$kw, "_", trend$state, ".pdf")
    data_to_plot <- list(prediction, table$data)
    names(data_to_plot) <- c("Predicted Trend", "Actual Trend")
    save_compare_plots(list(table$ds, table$ds), data_to_plot, file_name)
    mse <- calc_mse(prediction[fit_length:table_length], table$data[fit_length:table_length])
    if (print_results) {
        print(paste0("Trend prediction MSE: ", mse))
    }
    trend_eval[which(trend_eval$kw == trend$kw), trend$state] <- mse
    result <- list(trend_eval = trend_eval, trend = trend)
    return(result)
}

.reconcile_rate_and_trend <- function(rate, trend) {
    result <- list(trend = trend)
    if (is.null(rate) | is.null(trend$table)) {
        return(result)
    }
    table <- trend$table
    selected_rate <- rate[rate$state == trend$state, ]
    first_date <- max(c(min(selected_rate$ds), min(result$trend$table$ds)))
    last_date <- min(c(max(selected_rate$ds), max(result$trend$table$ds)))
    selected_rate <- selected_rate[selected_rate$ds >= first_date & selected_rate$ds <= last_date, ]
    trend$table <- table[table$ds >= first_date & table$ds <= last_date, ]
    result$rate <- selected_rate
    result$trend <- trend
    if (nrow(trend$table) != nrow(selected_rate)) {
        stop()
    }
    return(result)
}

.run_granger_test <- function(result, rate, trend) {
    if (!("granger_pvalue" %in% names(result))) {
        result$granger_pvalue <- data.frame(kw = c())
    }
    if (!("granger_fstat" %in% names(result))) {
        result$granger_fstat <- data.frame(kw = c())
    }
    if (!(trend$kw %in% result$granger_pvalue$kw)) {
        result$granger_pvalue[nrow(result$granger_pvalue) + 1, "kw"] <- trend$kw
    }
    if (!(trend$kw %in% result$granger_fstat$kw)) {
        result$granger_fstat[nrow(result$granger_fstat) + 1, "kw"] <- trend$kw
    }
    if (is.null(rate) | is.null(trend$table)) {
        result$granger_pvalue[which(result$granger_pvalue$kw == trend$kw), trend$state] <- NA
        result$granger_fstat[which(result$granger_fstat$kw == trend$kw), trend$state] <- NA
        return(result)
    }
    table <- trend$table
    if (var(rate$data) == 0 | var(table$data) == 0) {
        result$granger_pvalue[which(result$granger_pvalue$kw == trend$kw), trend$state] <- NA
        result$granger_fstat[which(result$granger_fstat$kw == trend$kw), trend$state] <- NA
        return(result)
    }
    gr_test <- grangertest(y = rate$data, x = table$data, order = 1)
    result$granger_pvalue[which(result$granger_pvalue$kw == trend$kw), trend$state] <- gr_test[2, 4]
    result$granger_fstat[which(result$granger_fstat$kw == trend$kw), trend$state] <- gr_test[2, 3]
    if (print_results) {
        print(paste0("Granger test p-value: ", gr_test[2, 4]))
    }
    return(result)
}

.calc_cross_correlation <- function(result, rate, trend) {
    fields <- data.frame(name = c("cross_correlation", "cross_correlation_minus_one", "cross_correlation_plus_one"), lag = c(0, -1, 1))
    for (i in 1:nrow(fields))
    {
        field <- fields[i, ]
        field_name <- field$name
        stat_sign_field_name <- paste0(field_name, "_stat_signif")
        diff_field_name <- paste0(field_name, "_diff")
        if (!(field_name %in% names(result))) {
            result[[length(result) + 1]] <- data.frame(kw = c())
            names(result)[length(names(result))] <- field_name
            result[[length(result) + 1]] <- data.frame(kw = c())
            names(result)[length(names(result))] <- stat_sign_field_name
            result[[length(result) + 1]] <- data.frame(kw = c())
            names(result)[length(names(result))] <- diff_field_name
        }
        if (!(trend$kw %in% result[[field_name]]$kw)) {
            result[[field_name]][nrow(result[[field_name]]) + 1, "kw"] <- trend$kw
            result[[stat_sign_field_name]][nrow(result[[stat_sign_field_name]]) + 1, "kw"] <- trend$kw
            result[[diff_field_name]][nrow(result[[diff_field_name]]) + 1, "kw"] <- trend$kw
        }
        if (is.null(rate) | is.null(trend$table)) {
            result[[field_name]][which(result[[field_name]]$kw == trend$kw), trend$state] <- NA
            result[[stat_sign_field_name]][which(result[[stat_sign_field_name]]$kw == trend$kw), trend$state] <- NA
            result[[diff_field_name]][which(result[[diff_field_name]]$kw == trend$kw), trend$state] <- NA
            next()
        }
        if (var(rate$data) == 0 | var(trend$table$data) == 0) {
            result[[field_name]][which(result[[field_name]]$kw == trend$kw), trend$state] <- NA
            result[[stat_sign_field_name]][which(result[[stat_sign_field_name]]$kw == trend$kw), trend$state] <- NA
            result[[diff_field_name]][which(result[[diff_field_name]]$kw == trend$kw), trend$state] <- NA
            next()
        }
        ccf_values <- ccf(rate$data, trend$table$data, pl = FALSE)
        value <- ccf_values$acf[ccf_values$lag == field$lag]
        if (print_results) {
            print(paste0(field_name, " is ", value))
        }
        result[[field_name]][which(result[[field_name]]$kw == trend$kw), trend$state] <- value
        significance_threshold <- 2 / (length(rate$data) - abs(field$lag))**0.5
        is_significant <- (abs(value) > significance_threshold)
        result[[stat_sign_field_name]][which(result[[stat_sign_field_name]]$kw == trend$kw), trend$state] <- is_significant
        result[[diff_field_name]][which(result[[diff_field_name]]$kw == trend$kw), trend$state] <- abs(value) - significance_threshold
    }
    return(result)
}

.save_result <- function(result_list, rate_type) {
    digits_after_point <- 4
    for (i in 1:length(result_list))
    {
        df <- result_list[[i]]
        df_type <- names(result_list)[[i]]
        if (print_results) {
            print(df)
        }
        numeric_cols <- sapply(df, is.numeric)
        df[numeric_cols] <- lapply(df[numeric_cols], function(x) ifelse(abs(x) < 10^(digits_after_point - 1), 
            signif(x, digits = digits_after_point), round(x)))
        tablex <- xtable(x = df)
        table_file_name <- paste0(output_dir, "/", rate_type, "_", df_type, ".tex")
        print.xtable(x = tablex, type = "latex", file = table_file_name, include.rownames = FALSE, NA.string = "--")
        table_file_name <- paste0(output_dir, "/", rate_type, "_", df_type, ".csv")
        write.csv(df, table_file_name, row.names = FALSE)
    }
}

.get_conventional_stat_significance <- function(df) {
    stat_significance <- data.frame(cbind(df[, 1], df[, -1] < 0.05))
    stat_significance[is.na(stat_significance)] <- FALSE
    stat_significance$true_count <- rowSums(stat_significance == TRUE)
    if (print_results) {
        print(stat_significance)
    }
    return(stat_significance)
}

ts_analyze <- function(rate_type, rate, trend_dir) {
    result <- list()
    trend <- get_next_trend(trend_dir)
    while (!is.null(trend)) {
        print(paste0("Trend: ", trend$kw, " for state: ", trend$state))
        if (run_trend_predictions) {
            result <- .trend_predict(result, trend)
            trend <- result$trend
        }
        reconciled <- .reconcile_rate_and_trend(rate, trend)
        if (granger_test) {
            result <- .run_granger_test(result, reconciled$rate, reconciled$trend)
        }
        if (cross_correlation) {
            result <- .calc_cross_correlation(result, reconciled$rate, reconciled$trend)
        }
        trend <- get_next_trend(trend_dir)
    }

    if (granger_test) {
        result$granger_stat_sign <- .get_conventional_stat_significance(result$granger_pvalue)
    }

    if (cross_correlation) {
        result$cross_correlation$average <- apply(result$cross_correlation[-1], 1, mean, na.rm = TRUE)
    }
    .save_result(result, rate_type)
    return(result)
}

get_trend_descriptive_statistics <- function(trend_dir, rate_type) {
    result <- list()
    result$popularity <- data.frame(kw = c())
    result$variance <- data.frame(kw = c())
    trend <- get_next_trend(trend_dir)
    while (!is.null(trend)) {
        print(paste0("Descriptive statistics for trend: ", trend$kw, " for state: ", trend$state))
        if (!(trend$kw %in% result$popularity$kw)) {
            result$popularity[nrow(result$popularity) + 1, "kw"] <- trend$kw
            result$variance[nrow(result$variance) + 1, "kw"] <- trend$kw
        }
        if (is.null(trend$table)) {
            result$popularity[which(result$popularity$kw == trend$kw), trend$state] <- NA
            result$variance[which(result$variance$kw == trend$kw), trend$state] <- NA
            trend <- get_next_trend(trend_dir)
            next()
        }
        result$popularity[which(result$popularity$kw == trend$kw), trend$state] <- sum(trend$table$data)
        result$variance[which(result$variance$kw == trend$kw), trend$state] <- var(trend$table$data)
        trend <- get_next_trend(trend_dir)
    }
    quantiles <- apply(result$popularity[, -1], 1, quantile, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.0), na.rm = TRUE)
    means <- apply(result$popularity[, -1], 1, mean, na.rm = TRUE)
    sds <- apply(result$popularity[, -1], 1, sd, na.rm = TRUE)
    result$quantile <- data.frame(t(quantiles))
    result$quantile <- cbind(result$popularity$kw, result$quantile, means, sds)
    names(result$quantile) <- c("kw", "Min", "2.5%", "25%", "Median", "75%", 
        "97.5%", "Max", "Mean", "SD")
    .save_result(result, rate_type)
    return(result)
}

get_rate_descriptive_statistics <- function(rate, rate_type)
{
    result <- list(quantiles=data.frame(state = c()))
    for (state in unique(rate$state))
    {
        new_row <- list()
        new_row = data.frame(state, t(quantile(rate[rate$state == state, "data"], probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.0), na.rm = TRUE)),
            mean(rate[rate$state == state, "data"], na.rm = TRUE), sd(rate[rate$state == state, "data"], na.rm = TRUE))
        result$quantiles = rbind(result$quantiles, new_row)
    }
    names(result$quantiles) <- c("state", "Min", "2.5%", "25%", "Median", "75%",
        "97.5%", "Max", "Mean", "SD")
    .save_result(result, rate_type)
    return(result)
}

divide_by_population <- function(rate, population) {
    rate <- merge(rate, population, by = "state")
    rate$data <- rate$data * 100000 / rate$population
    return(rate)
}

incidence_trend_statistics <- get_trend_descriptive_statistics(incidence_trend_dir, "incidence_trend")
mortality_trend_statistics <- get_trend_descriptive_statistics(mortality_trend_dir, "mortality_trend")

population <- load_population(population_path)
population <- population[population$year == 2020, c("population", "state")]

mortality <- load_mortality(overall_causes_mortality)
mortality <- divide_by_population(mortality, population)

incidence <- load_hopkins_timeseries(hopkins_incidence_path)
incidence <- divide_by_population(incidence, population)

specific_mortality <- load_hopkins_timeseries(hopkins_mortality_path)
specific_mortality <- divide_by_population(specific_mortality, population)

mortality_desc_statistics <- get_rate_descriptive_statistics(mortality, "mortality")
incidence_desc_statistics <- get_rate_descriptive_statistics(incidence, "incidence")
specific_mortality_desc_statistics <- get_rate_descriptive_statistics(specific_mortality, "specific_mortality")

incidence_result <- ts_analyze("incidence", incidence, incidence_trend_dir)
specific_mortality_result <- ts_analyze("specific_mortality", specific_mortality, mortality_trend_dir)
mortality_result <- ts_analyze("overall_mortality", mortality, mortality_trend_dir)
