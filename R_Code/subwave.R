library(deSolve)
library(boot)
library(minpack.lm)
library(purrr)
library(stats)
library(nloptr)

# Object Constructor
subwave <- function(
    number_of_subwaves,
    optimization_function = "optim",
    initial_guess = "fixed",
    population_estimate = 100000,
    lower_bounds = NULL,
    upper_bounds = NULL) {
    obj <- list(
        number_of_subwaves = number_of_subwaves,
        fit_length = 0,
        optimization_function = optimization_function,
        initial_guess = initial_guess,
        population_estimate = population_estimate,
        parameters = NULL,
        fit = NULL
    )
    class(obj) <- append("subwave", class(obj))
    if (!is.null(lower_bounds)) {
        obj$lower_bounds <- lower_bounds
    } else {
        obj$lower_bounds <- c(r = 0.0001, p = 0.0, threshold = 1, q = 0, K_0 = 20)
    }
    if (!is.null(upper_bounds)) {
        obj$upper_bounds <- upper_bounds
    } else {
        obj$upper_bounds <- c(r = 1000, p = 1.0, threshold = 100, q = 5, K_0 = obj$population_estimate / obj$number_of_subwaves)
    }
    return(obj)
}

# Solves differential equation for given parameters and returns numeric sequence
.subwave_function <- function(time_points, states, parameters) {
    # Differential equation from which we find subwave function
    subwave_equation <- function(t, states, parameters) {
        # readline()
        r <- parameters[1]
        p <- parameters[2]
        threshold <- parameters[3]
        q <- parameters[4]
        K_0 <- parameters[5]
        C <- states
        n <- length(C)
        if (n > 1) {
            A <- c(1, C[-n] > threshold)
            K <- c(K_0, K_0 * exp(-(q * (1:(n - 1)))))
        } else {
            A <- c(1)
            K <- K_0
        }
        dC_dt <- r * A * C^p * (1 - C / K)
        return(list(dC_dt))
    }

    # Numerical solution, using Ordinary Differential Equations
    ode_solution <- ode(y = states, times = time_points, func = subwave_equation, parms = parameters, method = "ode45")

    solution <- as.data.frame(ode_solution)
    # Why rowSums can not return just one column if there is single column???
    if (ncol(solution) > 2) {
        # We need superposition of waves, as we optimize total number, not each wave separatly
        solution$total <- rowSums(solution[, -1])
    } else {
        solution$total <- solution[, -1]
    }
    return(solution)
}

# This is for optimizers which use problem
.subwave_optimization_problem <- function(time_points, r, p, threshold, q, K_0, ...) {
    parameters <- c(r, p, threshold, q, K_0)
    initial_state <- c(...)
    names(initial_state) <- as.character(seq_along(initial_state))
    result <- .subwave_function(time_points, initial_state, parameters)
    return(result$total)
}

# For optimizers that use objective function
.subwave_objective_function <- function(parameters, time_points, data) {
    initial_state <- parameters[6:length(parameters)]
    result <- .subwave_function(time_points, initial_state, parameters[1:5])
    if (any(is.na(result$total))) {
        squares <- 10e10
    } else {
        squares <- sum((data - result$total)^2)
    }

    return(squares)
}

fit.subwave <- function(obj, data, ...) {
    y <- data
    number_of_subwaves <- obj$number_of_subwaves
    time_points <- 0:(length(y) - 1)
    obj$fit_length <- length(time_points)
    param_lower_bounds <- obj$lower_bounds
    param_lower_bounds[paste0("C_", 1:number_of_subwaves)] <- rep(0, number_of_subwaves)
    param_upper_bounds <- obj$upper_bounds
    param_upper_bounds[paste0("C_", 1:number_of_subwaves)] <- rep(param_upper_bounds["K_0"], number_of_subwaves)

    if (obj$initial_guess == "fixed") {
        initial_params <- c(
            r = 0.2,
            p = 1.0,
            threshold = 10,
            q = 0.3,
            K_0 = 100000 / number_of_subwaves
        )
    } else if (obj$initial_guess == "random") {
        initial_params <- c(
            r = runif(1, min = param_lower_bounds["r"], max = param_upper_bounds["r"]),
            p = runif(1, min = param_lower_bounds["p"], max = param_upper_bounds["p"]),
            threshold = runif(1, min = param_lower_bounds["threshold"], max = param_upper_bounds["threshold"]),
            q = runif(1, min = param_lower_bounds["q"], max = param_upper_bounds["q"]),
            K_0 = runif(1, min = param_lower_bounds["K_0"], max = param_upper_bounds["K_0"])
        )
    } else {
        initial_params <- obj$initial_guess
    }

    initial_states <- rep(0.1, number_of_subwaves)
    if (y[1] > 0) {
        initial_states[1] <- y[1]
    }
    names(initial_states) <- paste0("C_", 1:number_of_subwaves)
    initial_params <- c(initial_params, initial_states)
    # formula_str <- paste0("y ~ subwave_optimization_problem(t, r, p, threshold, q, K_0)") # , paste0(names(subwaves), collapse = ", "), ")")
    # arg_formula <- as.formula(formula_str)

    # model_fit <- nlsLM(
    #     arg_formula,
    #     lower = param_lower_bounds,
    #     upper = param_upper_bounds,
    #     start = initial_params,
    #     trace = TRUE
    # )

    if (obj$optimization_function == "optim") {
        model_fit <- optim(
            par = initial_params,
            fn = .subwave_objective_function,
            lower = param_lower_bounds,
            upper = param_upper_bounds,
            method = "L-BFGS-B",
            control = list(
                factr = 1e16
            ),
            time_points = time_points,
            data = y,
            ...
        )
    } else if (obj$optimization_function == "slsqp") {
        model_fit <- slsqp(
            x0 = initial_params,
            fn = .subwave_objective_function,
            lower = param_lower_bounds,
            upper = param_upper_bounds,
            time_points = time_points,
            data = y,
            ...
        )
    } else {
        model_fit <- obj$optimization_function(
            initial_params,
            fn = .subwave_objective_function,
            lower = param_lower_bounds,
            upper = param_upper_bounds,
            time_points = time_points,
            data = y,
            ...
        )
    }
    obj$parameters <- model_fit$par
    states <- obj$parameters[6:length(obj$parameters)]
    obj$fit <- .subwave_function(time_points, states, obj$parameters[1:5])$total

    return(obj)
}

predict.subwave <- function(obj, time_points) {
    if (is.null(obj$parameters))
    {
        stop("Object is not fitted. Call fit() on it first.")
    }
    time_points <- seq(1, obj$fit_length + length(time_points))
    states <- obj$parameters[6:length(obj$parameters)]
    result <- .subwave_function(time_points, states, obj$parameters[1:5])
    # Cut away the fit part
    result <- result[(obj$fit_length + 1):nrow(result), ]
    return(result$total)
}
