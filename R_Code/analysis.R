plot_counter <- 1

trim_by_dates <- function(df) {
    return(df[df$ds >= start_date & df$ds <= end_date, ])
}

.scale <- function(data) {
    if (all(data == 0)) {
        warning("Zero sequence scaling")
        return(as.numeric(data))
    }
    # return(100 * data / (median(data) + 0.0001))
    return(100 * data / mean(data))
}

.add_mean_and_median <- function(df, file_name) {
    numeric_cols <- sapply(df, is.numeric)
    df$mean <- rowMeans(df[numeric_cols], na.rm = TRUE)
    df$median <- apply(df[numeric_cols], 1, median, na.rm = TRUE)
    return(df)
}

.add_true_count <- function(df, file_name) {
    df$true_count <- rowSums(df[, 2:ncol(df)], na.rm = TRUE)
    return(df)
}

.save_heatmap <- function(df, file_name) {
    save_heatmap_tables(df, paste0(plot_dir, "/", file_name, "_heatmap"))
    return(df)
}

limit_signif_digits <- function(x) {
    digits_after_point <- 2
    ifelse(abs(x) < 10^(digits_after_point - 1),
        signif(x, digits = digits_after_point), round(x)
    )
}

limit_digits <- function(x) {
    result <- round(x, digits = 3)
    if (result == 0 && x != 0) {
        return("<.001")
    }
    return(result)
}

.calc_quantiles <- function(df, names) {
    quantiles <- apply(df, 1, quantile, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.0), na.rm = TRUE)
    means <- apply(df, 1, mean, na.rm = TRUE)
    sds <- apply(df, 1, sd, na.rm = TRUE)
    result <- data.frame(t(quantiles))
    result <- cbind(names[-1], result, means, sds)
    names(result) <- c(
        names[1], "Min", "2.5%", "25%", "Median", "75%",
        "97.5%", "Max", "Mean", "SD"
    )
    return(result)
}

.apply_fdr_adjustment <- function(df) {
    result <- data.frame(kw = df$kw, fdr = t(apply(df[-1], 1, p.adjust, method = "fdr")))
    names(result) <- names(df)
    return(result)
}

.reconcile_rate_and_trend <- function(rate, trends) {
    result <- list()
    if (is.null(rate)) {
        return(result)
    }
    first_date <- max(c(min(rate$ds), min(trends$ds)))
    last_date <- min(c(max(rate$ds), max(trends$ds)))
    selected_rate <- rate[rate$ds >= first_date & rate$ds <= last_date, ]
    trends <- trends[trends$ds >= first_date & trends$ds <= last_date, ]
    result$rate <- selected_rate
    result$trends <- trends
    return(result)
}


.run_granger_test <- function(rate, trends) {
    result <- list()
    states <- unique(trends$state)
    kws <- unique(trends$kw)
    for (kw in kws) {
        for (state in states) {
            selected_rows <- trends$state == state & trends$kw == kw
            trend <- trends[selected_rows, ]
            selected_rate <- rate[rate$state == state, ]
            if (!("granger_pvalue" %in% names(result))) {
                result$granger_pvalue <- data.frame(kw = c())
            }
            if (!("granger_fstat" %in% names(result))) {
                result$granger_fstat <- data.frame(kw = c())
                attr(result$granger_fstat, "summary_fun") <- c(.save_heatmap, .add_mean_and_median)
            }
            if (!(kw %in% result$granger_pvalue$kw)) {
                result$granger_pvalue[nrow(result$granger_pvalue) + 1, "kw"] <- kw
            }
            if (!(kw %in% result$granger_fstat$kw)) {
                result$granger_fstat[nrow(result$granger_fstat) + 1, "kw"] <- kw
            }
            if (!nrow(selected_rate) | !nrow(trend)) {
                result$granger_pvalue[result$granger_pvalue$kw == kw, state] <- NA
                result$granger_fstat[result$granger_fstat$kw == kw, state] <- NA
                next()
            }
            if (var(selected_rate$data) == 0 | var(trend$data) == 0) {
                result$granger_pvalue[which(result$granger_pvalue$kw == kw), state] <- NA
                result$granger_fstat[which(result$granger_fstat$kw == kw), state] <- NA
                next()
            }
            gr_test <- grangertest(y = selected_rate$data, x = trend$data, order = 1)
            result$granger_pvalue[which(result$granger_pvalue$kw == kw), state] <- gr_test[2, 4]
            result$granger_fstat[which(result$granger_fstat$kw == kw), state] <- gr_test[2, 3]
            if (print_results) {
                print(paste0("Granger test p-value: ", gr_test[2, 4]))
            }
        }
    }
    result$granger_pvalue <- .apply_fdr_adjustment(result$granger_pvalue)
    result$granger_pvalue_formatted <- result$granger_pvalue
    numeric_cols <- sapply(result$granger_pvalue_formatted, is.numeric)
    result$granger_pvalue_formatted[numeric_cols] <- lapply(result$granger_pvalue_formatted[numeric_cols], limit_signif_digits)
    result$granger_pvalue_formatted <- result$granger_pvalue_formatted %>%
        mutate(across(-1, ~ ifelse(. < 0.001, "\\cellcolor{green!25}{<.001}", ifelse(. < 0.05, paste0("\\cellcolor{green!25}{", ., "}"), .))))
    return(result)
}

.calc_cross_correlation <- function(rate, trends) {
    result <- list()
    states <- unique(trends$state)
    kws <- unique(trends$kw)
    fields <- data.frame(name = c("cross_correlation", "cross_correlation_minus_one", "cross_correlation_plus_one"), lag = c(0, -1, 1))
    for (i in 1:nrow(fields))
    {
        field <- fields[i, ]
        field_name <- field$name
        stat_sign_field_name <- paste0(field_name, "_stat_signif")
        diff_field_name <- paste0(field_name, "_diff")
        quantile_field_name <- paste0(field_name, "_quantile")
        heatmap_name <- paste0(field_name, "_heatmap")
        if (!(field_name %in% names(result))) {
            result[[length(result) + 1]] <- data.frame(kw = c())
            attr(result[[length(result)]], "summary_fun") <- c(.save_heatmap, .add_mean_and_median)
            names(result)[length(result)] <- field_name
            result[[length(result) + 1]] <- data.frame(kw = c())
            attr(result[[length(result)]], "summary_fun") <- .add_true_count
            names(result)[length(result)] <- stat_sign_field_name
            result[[length(result) + 1]] <- data.frame(kw = c())
            attr(result[[length(result)]], "summary_fun") <- .add_mean_and_median
            names(result)[length(result)] <- diff_field_name
        }
        for (kw in kws) {
            for (state in states) {
                selected_rows <- trends$state == state & trends$kw == kw
                trend <- trends[selected_rows, ]
                selected_rate <- rate[rate$state == state, ]
                if (!(kw %in% result[[field_name]]$kw)) {
                    result[[field_name]][nrow(result[[field_name]]) + 1, "kw"] <- kw
                    result[[stat_sign_field_name]][nrow(result[[stat_sign_field_name]]) + 1, "kw"] <- kw
                    result[[diff_field_name]][nrow(result[[diff_field_name]]) + 1, "kw"] <- kw
                }
                if (!nrow(selected_rate) | !nrow(trend)) {
                    result[[field_name]][which(result[[field_name]]$kw == kw), state] <- NA
                    result[[stat_sign_field_name]][which(result[[stat_sign_field_name]]$kw == kw), state] <- NA
                    result[[diff_field_name]][which(result[[diff_field_name]]$kw == kw), state] <- NA
                    next()
                }
                if (var(selected_rate$data) == 0 | var(trend$data) == 0) {
                    result[[field_name]][which(result[[field_name]]$kw == kw), state] <- NA
                    result[[stat_sign_field_name]][which(result[[stat_sign_field_name]]$kw == kw), state] <- NA
                    result[[diff_field_name]][which(result[[diff_field_name]]$kw == kw), state] <- NA
                    next()
                }
                ccf_values <- ccf(selected_rate$data, trend$data, pl = FALSE)
                value <- ccf_values$acf[ccf_values$lag == field$lag]
                if (print_results) {
                    print(paste0(field_name, " is ", value))
                }
                result[[field_name]][result[[field_name]]$kw == kw, state] <- value
                significance_threshold <- 2 / (length(rate$data) - abs(field$lag))**0.5
                is_significant <- (abs(value) > significance_threshold)
                result[[stat_sign_field_name]][result[[stat_sign_field_name]]$kw == kw, state] <- is_significant
                result[[diff_field_name]][result[[diff_field_name]]$kw == kw, state] <- abs(value) - significance_threshold
            }
        }
        result[[quantile_field_name]] <- .calc_quantiles(result[[field_name]][, -1], c("Key Word", result[[field_name]]$kw))
        # save_heatmap_tables(result[[field_name]], paste0(plot_dir, "/", heatmap_name))
    }
    return(result)
}

.save_result <- function(result_list, rate_type) {
    for (i in 1:length(result_list))
    {
        df <- result_list[[i]]
        if (!is.null(attr(df, "summary_fun"))) {
            funs <- attr(df, "summary_fun")
            if (length(funs) == 1) {
                df <- funs(df, paste0(rate_type, "_", names(result_list)[i]))
            } else {
                for (fun in funs) {
                    df <- fun(df, paste0(rate_type, "_", names(result_list)[i]))
                }
            }
        }
        if (print_results) {
            print(df)
        }
        numeric_cols <- sapply(df, is.numeric)
        df[numeric_cols] <- lapply(df[numeric_cols], limit_signif_digits)
        result_list[[i]] <- df
        df <- df %>%
            mutate_if(is.character, ~ gsub("_", " ", .))
        tablex <- xtable(x = df)
        df_name <- names(result_list)[[i]]
        curr_dir <- paste0(output_dir, "/", rate_type)
        if (!file.exists(curr_dir)) {
            dir.create(curr_dir, recursive = TRUE)
        }
        table_file_name <- paste0(curr_dir, "/", df_name, ".tex")
        print.xtable(x = tablex, file = table_file_name, type = "latex", include.rownames = FALSE, NA.string = "--")
        table_file_name <- paste0(curr_dir, "/", df_name, ".csv")
        write.csv(df, table_file_name, row.names = FALSE)
    }
    return(result_list)
}

.get_conventional_stat_significance <- function(df) {
    stat_significance <- data.frame(df[, 1], df[, -1] < 0.05)
    names(stat_significance)[1] <- "kw"
    if (print_results) {
        print(stat_significance)
    }
    return(stat_significance)
}

analyze_top_results <- function(rate_type, rate, trends, kws, states) {
    file_name <- paste0(plot_dir, "/", rate_type, "_side_by_side")
    pdf(paste0(file_name, ".pdf"), height = 13, width = 16.25)
    par(par(mfrow = c(state_top_count, trend_top_count)), mar = c(7.1, 5.1, 3, 2.1))
    config_style <- make_side_by_side_plot_style[[rate_type]]
    iteration <- 1
    for (state in states) {
        for (kw in kws) {
            trend_scaled <- trends[trends$kw == kw & trends$state == state, ]
            trend_scaled$data <- .scale(trend_scaled$data)
            trend_scaled$date_int <- as.integer(trend_scaled$ds)
            l_obj <- loess(data ~ date_int, data = trend_scaled, span = 0.25)
            trend_scaled$data_smoothed <- predict(l_obj)
            rate_scaled <- rate[rate$state == state, ]
            rate_scaled$data <- .scale(rate_scaled$data)
            rate_scaled$date_int <- as.integer(rate_scaled$ds)
            l_obj <- loess(data ~ date_int, data = rate_scaled, span = 0.25)
            rate_scaled$data_smoothed <- predict(l_obj)
            # file_name <- paste0(plot_dir, "/", rate_type, "_", kw, "_", state, "_side_by_side")
            rate_name <- str_to_title(gsub("_", " ", rate_type))
            search_name <- gsub("_", " ", kw)
            style <- list(
                main = paste0(rate_name, ' vs "', search_name, '" for ', state),
                legend_position = config_style$legend_position[iteration]
            )
            make_side_by_side_plots(rate_type, trend_scaled, rate_scaled, style)
            add_letter_label(LETTERS[iteration])
            iteration <- iteration + 1
        }
    }
    dev.off()
}

ts_analyze <- function(rate_type, rate, trends, population) {
    result <- list(population = population)
    reconciled <- .reconcile_rate_and_trend(rate, trends)
    if (granger_test) {
        result <- c(result, .run_granger_test(reconciled$rate, reconciled$trends))
        result$granger_stat_sign <- .get_conventional_stat_significance(result$granger_pvalue)
        attr(result$granger_stat_sign, "summary_fun") <- .add_true_count
    }
    if (cross_correlation) {
        result <- c(result, .calc_cross_correlation(reconciled$rate, reconciled$trends))
        top_kw <- select_top_trends(result)
        top_states <- select_top_states(result)
        analyze_top_results(rate_type, reconciled$rate, reconciled$trends, top_kw, top_states)
    }

    result <- .save_result(result, rate_type)
    return(result)
}

get_trend_descriptive_statistics <- function(trends, rate_type) {
    result <- list()
    result$aggregated_popularity <- data.frame(kw = c())
    result$variance <- data.frame(kw = c())
    states <- unique(trends$state)
    kws <- unique(trends$kw)
    for (kw in kws) {
        for (state in states)
        {
            print(paste0("Descriptive statistics for trend: ", kw, " for state: ", state))
            trend <- trends[trends$state == state & trends$kw == kw, ]
            if (!(kw %in% result$aggregated_popularity$kw)) {
                result$aggregated_popularity[nrow(result$aggregated_popularity) + 1, "kw"] <- kw
                result$variance[nrow(result$variance) + 1, "kw"] <- kw
            }
            if (!any(trends$state == state & trends$kw == kw)) {
                result$aggregated_popularity[which(result$aggregated_popularity$kw == kw), state] <- NA
                result$variance[which(result$variance$kw == kw), state] <- NA
            }
            result$aggregated_popularity[which(result$aggregated_popularity$kw == kw), state] <- sum(trend$data)
            result$variance[which(result$variance$kw == kw), state] <- var(trend$data)
        }
    }
    result$normalized_popularity <- cbind(result$aggregated_popularity[, 1], 100 * result$aggregated_popularity[, -1] / max(result$aggregated_popularity[, -1]))
    names(result$normalized_popularity) <- names(result$aggregated_popularity)
    save_heatmap_tables(result$normalized_popularity, paste0(plot_dir, "/", rate_type, "_normalized_popularity_heatmap"))
    result$aggregated_popularity_quantile <- .calc_quantiles(result$aggregated_popularity[, -1], c("Key Word", result$aggregated_popularity$kw))
    result$normalized_popularity_quantile <- .calc_quantiles(result$normalized_popularity[, -1], c("Key Word", result$normalized_popularity$kw))
    .save_result(result, rate_type)
    return(result)
}

get_rate_descriptive_statistics <- function(rate, rate_type) {
    result <- list(quantiles = data.frame(state = c()))
    for (state in unique(rate$state))
    {
        new_row <- list()
        new_row <- data.frame(
            state, t(quantile(rate[rate$state == state, "data"], probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.0), na.rm = TRUE)),
            mean(rate[rate$state == state, "data"], na.rm = TRUE), sd(rate[rate$state == state, "data"], na.rm = TRUE)
        )
        result$quantiles <- rbind(result$quantiles, new_row)
    }
    names(result$quantiles) <- c(
        "State", "Min", "2.5%", "25%", "Median", "75%",
        "97.5%", "Max", "Mean", "SD"
    )
    .save_result(result, rate_type)
    return(result)
}

divide_by_population <- function(rate, population) {
    if (!nrow(population)) {
        stop("Population is empty")
    }
    rate <- merge(rate, population, by = "state")
    rate$data <- rate$data * 100000 / rate$population
    return(rate)
}

select_top_trends <- function(result, order = -1) {
    df <- result$cross_correlation
    means <- rowMeans(df[, -1])
    df <- df[order(order * means), ]
    return(head(df$kw, trend_top_count))
}

select_top_states <- function(result, order = -1) {
    df <- result$population
    df <- df[order(order * df$population), ]
    return(head(df$state, state_top_count))
}

trend_analyze <- function(result, trends, rate_type) {
    pred_result <- list()
    while (!is.null(trend)) {
        if (trend$kw %in% selected_trends) {
            print(paste0("Prediction for trend: ", trend$kw, " for state: ", trend$state))
            temp <- .trend_predict(pred_result, trend)
            pred_result <- temp[[1]]
            trend <- temp[[2]]
        } else {
            print(paste0("Trend ", trend$kw, " is not in selected trends. "))
        }
        trend <- get_next_trend(trend_dir)
    }
    .save_result(pred_result, rate_type)
    return(pred_result)
}

roll_fun <- function(data, fit_window, method, state, kw) {
    result <- NULL
    fit_data <- data[1:fit_window, ]
    col_names <- names(data[, 2:ncol(data)])
    eval_data <- data[(fit_window + 1):nrow(data), ]
    for (column in col_names)
    {
        tmp_data <- fit_data[, c("ds", column)]
        # print(tmp_data)
        if (cumm_data) {
            tmp_data[column] <- cumsum(tmp_data[column])
        }
        # print(tmp_data)
        names(tmp_data) <- c("ds", "y")
        if (bootstrap_size > 0) {
            bootstrap_result <- bootstrap_predict(
                tmp_data,
                nrow(eval_data),
                method,
                bootstrap_size = bootstrap_size
            )
            prediction <- bootstrap_result$prediction$median
            fit <- bootstrap_result$fit$median
        } else {
            model <- fit_data(tmp_data$y, tmp_data$ds, method)
            prediction_dates <- seq(tail(tmp_data$ds, 1), length.out = nrow(eval_data) + 1, by = "month")[-1]
            prediction_raw <- predict_trend(model, prediction_dates, method)
            prediction <- prediction_raw$yhat
            fit <- model$fit
            prediction[is.na(prediction)] <- 1e9
            fit[is.na(fit)] <- 1e9
        }
        # readline()
        if (cumm_data) {
            print(c(fit, prediction))
            data[[paste0("pred_", column)]] <- c(fit[1], diff(c(fit, prediction)))
            eval_data[[paste0("pred_", column)]] <- diff(c(fit[length(fit)], prediction))
            print(data)
        } else {
            data[[paste0("pred_", column)]] <- c(fit, prediction)
            eval_data[[paste0("pred_", column)]] <- prediction
        }
        readline()
        # diff_prediction <- list(
        #     lower = c(bootstrap_result$fit$lower, bootstrap_result$prediction$lower),
        #     upper = c(bootstrap_result$fit$upper, bootstrap_result$prediction$upper),
        #     median = c(bootstrap_result$fit$median, bootstrap_result$prediction$median)
        # )
        # make_prediction_plot(1:nrow(data), nrow(data), cumsum(unlist(data[, column])), diff_prediction)
        # readline()
    }
    # print(eval_data)
    print(data)
    readline()
    col_names <- names(eval_data[, 2:ncol(eval_data)])
    metrics <- c("MSE", "MAE", "DTW", "CCF")
    # metrics <- c("MAE", "MSE")
    for (metric in metrics) {
        metric_result <- list(metric = metric)
        for (i in seq(1, length(col_names) - 1)) {
            for (j in seq(i + 1, length(col_names))) {
                if (prediction_plots) {
                    # y_vect_list <- list(cumsum(data[, col_names[i]]), cumsum(data[, col_names[j]]))
                    y_vect_list <- list(data[, col_names[i]], data[, col_names[j]])
                    names(y_vect_list) <- c(col_names[i], col_names[j])
                    file_name <- paste0("../plots/incidence_", kw, "_", state, "_", col_names[i], "_vs_", col_names[j], "_", plot_counter, ".pdf")
                    plot_counter <<- plot_counter + 1
                    save_compare_plots(list(data$ds, data$ds), y_vect_list, file_name)
                    # readline()
                }

                col_name <- paste0(col_names[i], "_vs_", col_names[j])
                metric_result[[col_name]] <- do.call(metric, list(eval_data[, col_names[i]], eval_data[, col_names[j]]))
                if (any(is.na(metric_result[[col_name]]))) {
                    stop(paste0(metric, " metric returned NA for the ", col_name))
                }
            }
        }
        # print(result)
        # print(metric_result)
        # readline()
        if (is.null(result)) {
            result <- data.frame(metric_result)
        } else {
            result <- rbind(result, metric_result)
        }
    }
    # print(result)
    # readline()
    return(result)
}

my_better_rollapply <- function(df, FUN, window, ...) {
    result <- NULL
    if (nrow(df) < window) {
        stop(paste0("Window is wider than the data frame ", window, " vs ", nrow(df)))
    }
    for (start_idx in 1:(nrow(df) - window)) {
        iteration_result <- FUN(df[start_idx:(start_idx + window - 1), ], ...)
        if (is.null(result)) {
            result <- data.frame(iteration_result)
        } else {
            result <- rbind(result, iteration_result)
        }
    }
    return(result)
}

collect_mses <- function(result, rate_data, trend_data, horizon_bounds, method, fit_lengths, callback = NULL, ...) {
    max_fit_len <- max(fit_lengths)
    kws <- unique(trend_data$kw)
    states <- unique(trend_data$state)
    for (kw in kws) {
        for (state in states) {
            print(paste0("State ", state, " keyword ", kw))
            curr_rate_data <- rate_data[rate_data$state == state, c("ds", "data")]
            curr_trend_data <- trend_data[trend_data$kw == kw & trend_data$state == state, c("ds", "data")]
            data <- data.frame(
                ds = curr_rate_data$ds,
                rate = .scale(curr_rate_data$data),
                trend = .scale(curr_trend_data$data)
            )
            # print(data)
            # readline()
            for (horizon in seq(horizon_bounds[1], horizon_bounds[2])) {
                for (fit_length in fit_lengths) {
                    if (!is.null(result) && any(result$kw == kw & result$state == state & result$fit_length == fit_length & result$horizon == horizon)) {
                        next
                    }
                    tmp_data <- data[(max_fit_len - fit_length + 1):nrow(data), ]
                    raw_result <- my_better_rollapply(
                        tmp_data,
                        roll_fun,
                        window = fit_length + horizon,
                        fit_window = fit_length,
                        method = method,
                        state = state,
                        kw = kw
                    )
                    # raw_result <- roll_fun(tmp_data[1:(fit_length + horizon), ], fit_length, method, state, kw)
                    raw_result <- aggregate(raw_result[, 2:ncol(raw_result)], list(raw_result$metric), mean)
                    names(raw_result)[1] <- "metric"
                    iter_result <- cbind(list(kw = kw, state = state, horizon = horizon, fit_length = fit_length), raw_result)
                    if (is.null(result)) {
                        result <- as.data.frame(iter_result)
                    } else {
                        result <- rbind(result, iter_result)
                    }
                    if (!is.null(callback)) {
                        callback(result, ...)
                    }
                }
            }
        }
    }
    return(result)
}

output_prediction_result <- function(df, file_name) {
    # Reorder and drop some columns
    # print(df)
    # print(df[apply(is.na(df), 1, any), ])
    df <- df[, c(
        "kw",
        "state",
        "horizon",
        "fit_length",
        "metric",
        "rate_vs_trend",
        # "rate_vs_pred_rate",
        "trend_vs_pred_trend",
        "rate_vs_pred_trend"
    )]
    # tmp <- df[df$kw == "hand_sanitizer" & df$metric == "MAE" & df$fit_length == 18,]
    # print(df)
    # print(quantile(tmp$rate_vs_trend, 0.5))
    result <- df %>%
        group_by(kw, horizon, fit_length, metric) %>%
        select(-"state") %>%
        summarise(
            across(
                1:(ncol(.) - 4), # This is not how it should be, but there is no elegant ways in R
                ~ sprintf("%.2f (%.2f, %.2f)", quantile(.x, 0.5), quantile(.x, 0.025), quantile(.x, 0.975))
            )
        ) %>%
        ungroup() %>%
        pivot_longer(
            cols = 5:ncol(.),
            names_to = "comparison",
            values_to = "range"
        ) %>%
        pivot_wider(
            names_from = c("comparison", "fit_length"),
            values_from = "range",
            names_sep = ", fit="
        )
    # print(result[result$kw == "hand_sanitizer",])
    # readline()
    for (metric in unique(result$metric)) {
        tmp_result <- result[result$metric == metric, ] %>%
            select(-"metric", -"horizon")
        write.csv(tmp_result, file = paste0(file_name, "_", metric, ".csv"), row.names = FALSE)
    }
}
