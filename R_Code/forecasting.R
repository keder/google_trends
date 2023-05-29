library(forecast)
library("prophet")
library("forecast")

.box_cox_transform <- function(input_data) {
  lambda <- BoxCox.lambda(input_data + 1, method="loglik")
  result <- BoxCox(input_data + 1, lambda)
  return(result)
}

.box_cox_inverse <- function(input_data) {
  result <- InvBoxCox(x=input_data, attr(input_data, "lambda")) - 1
  return(result)
}

.atan_transform <- function(input_data) {
  result <- 10 * pi * tan((input_data - 50) * pi / 101)
  return(result)
}

.atan_inverse <- function(input_data) {
  result <- 101 * atan(input_data/(pi*10)) / pi + 50
  return(result)
}

.linear_transform <- function(input_data) {
  result <- input_data
  which_zeros <- which(input_data == 0)
  zero_count <- length(which_zeros)
  result[which_zeros] <- 1 * (input_data[which_zeros] + 0) + rnorm(n=zero_count, mean = 0, sd = 1)
  return(result)
}

.linear_inverse <- function(input_data) {
  result <- (input_data - 0) / 1
  return(result)
}

.transform <- .box_cox_transform
.inverse <- .box_cox_inverse

.predict_arima <- function(input_data, forecast_length) {
  fit_length <- dim(input_data)[1] - forecast_length
  transformed_data <- input_data$data[1:fit_length]
  # transformed_data <- .transform(transformed_data)
  transformed_data <- .linear_transform(transformed_data)
  # transformed_data <- .atan_transform(input_data$data[1:fit_length])
  arima_object <- Arima(as.vector(transformed_data),
    order = c(12, 1, 12), include.drift = FALSE, method = "CSS")
  # arima_object <- auto.arima(as.vector(transformed_data))
  # print(transformed_data$data)
  prediction <- forecast(arima_object, h = forecast_length)
  attributes(prediction$mean) <- attributes(transformed_data)
  attributes(arima_object$fitted) <- attributes(transformed_data)
  reversed_fitted <- arima_object$fitted
  reversed_prediction <- prediction$mean
  # reversed_fitted <- .inverse(.linear_inverse(reversed_fitted))
  # reversed_prediction <- .inverse(.linear_inverse(reversed_fitted))
  # reversed_fitted <- .atan_inverse(arima_object$fitted)
  # reversed_prediction <- .atan_inverse(prediction$mean)
  result <- c(reversed_fitted, reversed_prediction)
  return(result)
}

.predict_prophet <- function(input_data, forecast_length) {
  fit_length = dim(input_data)[1] - forecast_length
  dates = data.frame(
    ds = input_data$ds
  )
  transformed_data = .transform(input_data$data[1:fit_length])
  data_to_feed <- data.frame(
    ds = input_data$ds[1:fit_length],
    y = transformed_data
  )
  prophet_object <- prophet(data_to_feed)
  result <- predict(prophet_object, dates)
  attributes(result$yhat) <- attributes(transformed_data)
  result <- .inverse(result$yhat)
  return(result)
}

.predict_nonparam <- function(input_data, forecast_length) {
}

predict_trend <- function(input_data, forecast_length, prediction_type) {
  switch(prediction_type,
  arima_pred = { .predict_arima(input_data, forecast_length) },
  prophet_pred = { .predict_prophet(input_data, forecast_length) },
  nonparam_pred = { .predict_nonparam(input_data, forecast_length) })
}

# source(paste(getwd(), "data_load.R", sep = "/"))
# directory = "../data/incidence"
# # table = get_next_trend(directory)$table
# # table = get_next_trend(directory)$table
# trend = get_next_trend(directory)
# table = trend$table
# table[,2] = 100 + table[,2]
# print(trend)

# result = predict_trend(table, 38, "prophet_pred")
# # result = predict_trend(table, 0, "prophet_pred")
# xlim = c(min(table$ds), max(table$ds))
# ylim = c(min(c(table[,2], result)), max(c(table[,2], result)))
# plot(table$ds, table[,2], xlim=xlim, ylim=ylim, type="l", col="green")
# lines(table$ds, result, col="red")

# result = predict_trend(table, 38, "arima_pred")
# print(result)
# xlim = c(min(table$ds), max(table$ds))
# ylim = c(min(c(table[,2], result)), max(c(table[,2], result)))
# plot(table$ds, table[,2], xlim=xlim, ylim=ylim, type="l", col="green")
# lines(table$ds, result, col="red")

