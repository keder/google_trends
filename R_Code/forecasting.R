library("prophet")
library("forecast")
library("boot")
library("dtw")
source(paste(getwd(), "subwave.R", sep = "/"))

.linear_coeff <- 1
.linear_intercept <- 0

MSE <- function(y, y_hat) {
  return(mean((y - y_hat)^2))
}

MAE <- function(y, y_hat) {
  return(mean(abs(y - y_hat)))
}

DTW <- function(y, y_hat) {
  return(dtw(x = y, y = y_hat, keep = TRUE)$distance)
}

CCF <- function(y, y_hat) {
  tweak_variable <- 10^-6
  shift <- rnorm(n = length(y), mean = 0, sd = tweak_variable)
  result <- ccf(x = y + shift, y = y_hat + shift, plot = FALSE)
  return(result$acf[result$lag == 0])
}

.box_cox_transform <- function(input_data) {
  lambda <- BoxCox.lambda(input_data + 1, method = "loglik")
  result <- BoxCox(input_data + 1, lambda)
  return(result)
}

.box_cox_inverse <- function(input_data) {
  result <- InvBoxCox(x = input_data, lambda = attr(input_data, "lambda")) - 1
  return(result)
}

.atan_transform <- function(input_data) {
  result <- 10 * pi * tan((input_data - 50) * pi / 101)
  return(result)
}

.atan_inverse <- function(input_data) {
  result <- 101 * atan(input_data / (pi * 10)) / pi + 50
  return(result)
}

.linear_transform <- function(input_data) {
  result <- input_data
  which_zeros <- which(input_data == 0)
  zero_count <- length(which_zeros)
  result[which_zeros] <- .linear_coeff * (input_data[which_zeros] + .linear_intercept) + rnorm(n = zero_count, mean = 0, sd = 1)
  return(result)
}

.linear_inverse <- function(input_data) {
  result <- (input_data - .linear_intercept) / .linear_coeff
  return(result)
}

.no_transform <- function(input_data) {
  return(input_data)
}

if (transformation == "boxcox") {
  .transform <- .box_cox_transform
  .inverse <- .box_cox_inverse
} else {
  .transform <- .no_transform
  .inverse <- .no_transform
}

.fit_arima <- function(fit_data, dates, ...) {
  # print(fit_data)
  transformed_data <- .transform(fit_data)
  # transformed_data <- fit_data
  # print(transformed_data)
  # my_autoarima(transformed_data)
  # Arima(transformed_data, order=c(3,3,3))
  obj <- auto.arima(transformed_data,
    d = 2,
    # D = 2,
    ic = "aic",
    max.p = length(fit_data),
    max.q = length(fit_data),
    start.p = length(fit_data) - 1,
    start.q = length(fit_data) - 1,
    max.P = length(fit_data),
    max.Q = length(fit_data),
    start.P = length(fit_data) - 1,
    start.Q = length(fit_data) - 1,
    # approximation = FALSE,
    # stepwise = FALSE,
    stationary = TRUE,
    # seasonal = FALSE,
    # allowdrift = FALSE,
    # lambda = "auto",
    ...
  )
  # obj <- Arima(fit_data, order = c(2, 0, 0))

  obj$bc_lambda <- attr(transformed_data, "lambda")
  attr(obj$fitted, "lambda") <- obj$bc_lambda
  obj$fit <- .inverse(obj$fitted)
  # obj$fit <- obj$fitted
  # print(obj$fitted)
  # print(obj$fit)
  # print(obj)
  # readline()
  # readline()
  return(obj)
}

.predict_arima <- function(obj, dates) {
  forecast_length <- length(dates)
  prediction <- forecast(obj, h = forecast_length)
  attr(prediction$mean, "lambda") <- obj$bc_lambda
  attr(prediction$lower, "lambda") <- obj$bc_lambda
  attr(prediction$upper, "lambda") <- obj$bc_lambda
  yhat <- .inverse(prediction$mean)
  yhat_lower <- .inverse(prediction$lower)
  yhat_upper <- .inverse(prediction$upper)
  names(yhat) <- NULL
  names(yhat_lower) <- NULL
  names(yhat_upper) <- NULL
  # print(result$mean)
  # print(obj$fitted)
  result <- data.frame(
    ds = dates,
    yhat = yhat,
    yhat_lower = yhat_lower,
    yhat_upper = yhat_upper
  )
  # print(obj$fit)
  # print(result)
  # result <- result[2:length(result)]
  # readline()
  return(result)
}

.fit_prophet <- function(fit_data, dates, ...) {
  # print(fit_data)
  transformed_data <- .transform(fit_data)
  data_to_feed <- data.frame(
    ds = dates,
    y = transformed_data
  )
  obj <- prophet(
    data_to_feed,
    # yearly.seasonality = TRUE,
    yearly.seasonality = 4,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.prior.scale = 3,
    interval.width = 0.1,
    changepoint.range = 0.45
  )
  obj$bc_lambda <- attr(transformed_data, "lambda")
  fit <- predict(obj, data_to_feed)
  obj$fit <- fit$yhat
  obj$fit_lower <- fit$yhat_lower
  obj$fit_upper <- fit$yhat_upper
  attr(obj$fit, "lambda") <- obj$bc_lambda
  attr(obj$fit_lower, "lambda") <- obj$bc_lambda
  attr(obj$fit_upper, "lambda") <- obj$bc_lambda
  obj$fit <- .inverse(obj$fit)
  obj$fit_lower <- .inverse(obj$fit_lower)
  obj$fit_upper <- .inverse(obj$fit_upper)
  # print(obj$fit)
  # print(obj)
  # readline()
  return(obj)
}

.predict_prophet <- function(obj, dates) {
  data_to_feed <- data.frame(
    ds = dates
  )
  prediction <- predict(obj, data_to_feed)
  attr(prediction$yhat, "lambda") <- obj$bc_lambda
  attr(prediction$yhat_lower, "lambda") <- obj$bc_lambda
  attr(prediction$yhat_upper, "lambda") <- obj$bc_lambda
  
  result <- data.frame(
    ds = dates,
    yhat = .inverse(prediction$yhat),
    yhat_lower = .inverse(prediction$yhat_lower),
    yhat_upper = .inverse(prediction$yhat_upper)
  )
  return(result)
}

.fit_subwave <- function(fit_data, dates, number_of_subwaves = NULL, ...) {
  print(fit_data)
  transformed_data <- fit_data
  best_model <- NULL
  best_MSE <- Inf
  if (is.null(number_of_subwaves)) {
    numbers <- seq(1, 4)
  } else {
    numbers <- c(number_of_subwaves)
  }
  for (number_of_subwaves in numbers) {
    obj <- subwave(number_of_subwaves, ...)
    # obj$bc_lambda <- attr(transformed_data, "lambda")
    obj <- fit.subwave(obj, transformed_data)
    mean_sq <- MSE(transformed_data, obj$fit)
    if (mean_sq < best_MSE) {
      best_MSE <- mean_sq
      best_model <- obj
    }
  }
  print(best_model$fit)
  # readline()
  return(best_model)
}

.predict_subwave <- function(obj, dates) {
  prediction <- predict(obj, dates)
  # attr(result, "lambda") <- obj$bc_lambda
  # result <- .inverse(result)
  result <- data.frame(
    ds = dates,
    yhat = prediction
  )
  return(result)
}

.fit_nonparam <- function(fit_data, dates, ...) {
}

.predict_nonparam <- function(obj, dates, forecast_length) {
}

fit_data <- function(data, dates, prediction_method, ...) {
  if (length(data) == 0) {
    stop("Empty input data frame")
  }
  obj <- switch(prediction_method,
    arima_pred = {
      .fit_arima(data, dates, ...)
    },
    prophet_pred = {
      .fit_prophet(data, dates, ...)
    },
    nonparam_pred = {
      .fit_nonparam(data, dates, ...)
    },
    subwave_pred = {
      .fit_subwave(data, dates, ...)
    }
  )
  return(obj)
}

predict_trend <- function(obj, dates, prediction_method) {
  result <- switch(prediction_method,
    arima_pred = {
      .predict_arima(obj, dates)
    },
    prophet_pred = {
      .predict_prophet(obj, dates)
    },
    nonparam_pred = {
      .predict_nonparam(obj, dates)
    },
    subwave_pred = {
      .predict_subwave(obj, dates)
    }
  )
  if (is.null(result)) {
    stop("Wrong prediction method")
  }
  return(result)
}

.poisson_resampling <- function(data) {
  data[data < 0] <- 0
  result <- rpois(length(data), data)
  return(result)
}

.trivial_prediction <- function(data, prediction_horizon) {
  result <- list(
    prediction = list(
      lower = rep(data$y[1], prediction_horizon),
      median = rep(data$y[1], prediction_horizon),
      upper = rep(data$y[1], prediction_horizon),
      raw = matrix(rep(data$y[1], prediction_horizon), nrow = 1)
    ),
    fit = list(
      lower = data$y,
      median = data$y,
      upper = data$y,
      raw = matrix(data$y, nrow = 1)
    )
  )
  return(result)
}

bootstrap_predict <- function(
    data,
    prediction_horizon,
    method,
    bootstrap_size = 500, ...) {
  # First of all deal with trivial case
  if (var(data$y) == 0) {
    return(.trivial_prediction(data, prediction_horizon))
  }
  # print(data)

  original_model <- fit_data(data$y, data$ds, prediction_method = method, ...)
  original_fit <- original_model$fit
  # print(original_fit)
  # print(data$y)
  if (cumm_data) {
    fit_diff <- c(original_fit[1], diff(original_fit))
  } else {
    fit_diff <- original_fit
  }
  # print(fit_diff)
  # readline()
  bootstrap_samples_diff <- boot(
    fit_diff,
    statistic = .poisson_resampling,
    R = bootstrap_size,
    sim = "parametric"
  )$t
  # bootstrap_samples_diff_with_first_value <- cbind(original_fit[1], bootstrap_samples_diff)
  # print(bootstrap_samples_diff)
  if (cumm_data) {
    bootstrap_samples <- t(apply(bootstrap_samples_diff, 1, cumsum))
  } else {
    bootstrap_samples <- bootstrap_samples_diff
  }
  # print(head(bootstrap_samples_diff_with_first_value, n = 20))
  # print(head(bootstrap_samples, n = 20))
  # plot_data <- list(
  #   lower = apply(bootstrap_samples, 2, quantile, probs = 0.0),
  #   median = apply(bootstrap_samples, 2, quantile, probs = 0.5),
  #   upper = apply(bootstrap_samples, 2, quantile, probs = 1.0)
  # )
  # make_bootstrap_plot(data$ds, data$y, plot_data, original_fit)
  # readline()

  models <- apply(bootstrap_samples, 1, fit_data, prediction_method = method, dates = data$ds, ...)
  bootstrap_fits <- sapply(models, function(x) x[["fit"]])
  prediction_dates <- seq(tail(data$ds, 1), length.out = prediction_horizon + 1, by = "month")[-1]
  bootstrap_predictions <- sapply(
    models,
    function(model, dates, prediction_method) predict_trend(model, dates, prediction_method)$yhat,
    dates = prediction_dates,
    prediction_method = method
  )
  # print(bootstrap_predictions)
  # readline()
  if (is.vector(bootstrap_predictions)) {
    bootstrap_predictions <- matrix(bootstrap_predictions, nrow = 1)
  }
  bootstrap_predictions[is.na(bootstrap_predictions)] <- 1e9
  bootstrap_fits[is.na(bootstrap_fits)] <- 1e9
  # print(bootstrap_predictions)
  # bootstrap_fitpred <- rbind(bootstrap_fits, bootstrap_predictions)
  # print(t(bootstrap_fitpred))
  # bootstrap_fitpred_diff <- apply(bootstrap_fitpred, 2, diff)
  # print(t(rbind(bootstrap_fitpred[1,], bootstrap_fitpred_diff)))
  # plot_data <- list(
  #   lower = apply(t(bootstrap_fits), 2, quantile, probs = 0.0),
  #   median = apply(t(bootstrap_fits), 2, quantile, probs = 0.5),
  #   upper = apply(t(bootstrap_fits), 2, quantile, probs = 1.0)
  # )
  # make_prediction_plot(data$ds, prediction_horizon, data$y, plot_data)

  result <- list(
    prediction = list(
      lower = apply(bootstrap_predictions, 1, quantile, probs = 0.025),
      median = apply(bootstrap_predictions, 1, quantile, probs = 0.500),
      upper = apply(bootstrap_predictions, 1, quantile, probs = 0.975),
      raw = bootstrap_predictions
    ),
    fit = list(
      lower = apply(bootstrap_fits, 1, quantile, probs = 0.025),
      median = apply(bootstrap_fits, 1, quantile, probs = 0.500),
      upper = apply(bootstrap_fits, 1, quantile, probs = 0.975),
      raw = bootstrap_fits
    )
  )
  # print(result)
  # readline()
  return(result)
}
