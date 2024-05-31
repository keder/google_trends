library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(dplyr)

state.abb[length(state.abb) + 1] <- "DC"
state.name[length(state.name) + 1] <- "District of Columbia"

abbreviate_state <- function(state) {
  return(state.abb[match(tolower(state), tolower(state.name))])
}

.interpolate_negatives <- function(series) {
  negative_indices <- which(series < 0)
  if (length(negative_indices) == 0)
  {
    return(series)
  }
  clusters <- list()
  current_cluster <- c(negative_indices[1], negative_indices[1])

  # Iterate over the indices
  for (i in 2:length(negative_indices)) {
    if (negative_indices[i] == negative_indices[i - 1] + 1) {
      # Extend the current cluster
      current_cluster[2] <- negative_indices[i]
    } else {
      # Add the current cluster to the list
      clusters <- c(clusters, list(current_cluster))

      # Start a new cluster
      current_cluster <- c(negative_indices[i], negative_indices[i])
    }
  }

  # Add the final cluster to the list
  clusters <- c(clusters, list(current_cluster))

  for (cluster in clusters)
  {
    series[cluster[1]:cluster[2]] <- round(approx(x = c(cluster[1] - 1, cluster[2] + 1), y = series[c(cluster[1] - 1, cluster[2] + 1)], xout = cluster[1]:cluster[2])$y)
  }

  return(series)
}

.make.get_next_trend <- function() {
  dirs <- NULL
  dir_position <- NULL
  files <- NULL
  file_pos <- NULL
  parent_dir <- NULL
  get_next_dir <- function() {
    if (is.null(dirs)) {
      dirs <<- list.dirs(path = parent_dir)[-1]
      dir_position <<- 1
    } else {
      dir_position <<- dir_position + 1
    }
    if (dir_position > length(dirs)) {
      dirs <<- NULL
      dir_position <<- NULL
      return(NULL)
    }
    return(dirs[dir_position])
  }
  .get_next_trend <- function(dir) {
    if (is.null(parent_dir) || parent_dir != dir) {
      dirs <<- NULL
      dir_position <<- NULL
      files <<- NULL
      file_pos <<- NULL
      parent_dir <<- dir
    }
    curr_dir <- NULL
    kw <- NULL
    state <- NULL
    if (is.null(dirs) || file_pos + 1 > length(files)) {
      files <<- NULL
      while (is.null(files) || (length(files) < 1)) {
        curr_dir <- get_next_dir()
        if (is.null(curr_dir)) {
          parent_dir <- NULL
          return(NULL)
        }
        files <<- list.files(path = curr_dir, pattern = "\\.csv$")
        file_pos <<- 1
      }
    } else {
      file_pos <<- file_pos + 1
    }
    curr_dir <- dirs[dir_position]
    kw <- tail(strsplit(curr_dir, "/")[[1]], 1)
    file <- paste(curr_dir, files[file_pos], sep = "/")
    state <- strsplit(files[file_pos], ".", fixed = TRUE)[[1]][1]
    table <- NULL
    tryCatch(
      {
        table <- read.csv(file = file)
      },
      error = function(e) { }
    )
    if (!is.null(table)) {
      table <- table[which(table$isPartial == "False"), ]
      table$ds <- as.Date(table$date)
      table$data <- table[, 2]
    }
    result <- list("table" = table, "kw" = kw, "state" = state)
    return(result)
  }
  return(.get_next_trend)
}

load_trends <- function(trend_dir)
{
  print("Loading trends...")
  result <- data.frame()
  kw_dirs <- list.dirs(path = trend_dir)[-1]
  for (kw_dir in kw_dirs)
  {
    kw <- tail(strsplit(kw_dir, "/")[[1]], 1)
    files <- list.files(path = kw_dir, pattern = "\\.csv$")
    for (file in files)
    {
      file_path <- paste(kw_dir, file, sep = "/")
      state <- strsplit(file, ".", fixed = TRUE)[[1]][1]
      print(paste0("Loading trend for \"", kw, "\" in ", state))
      table <- NULL
      tryCatch(
        {
          table <- read.csv(file = file_path)
        },
        error = function(e) { }
      )
      if (!is.null(table)) {
        table <- table[table$isPartial == "False", ]
        table$kw <- kw
        table$state <- state
        table$ds <- as.Date(table$date)
        table$data <- table[, 2]
        table <- table[, c(-2, -3)]
        result <- rbind(result, table)
      }
    }
  }
  print("Trends successfully loaded.")
  return(result)
}

load_incidence <- function(path) {
  table <- read.csv(file = path)
  table$ds <- as.Date(table$submission_date, format = "%m/%d/%Y")
  table$month <- strftime(table$ds, format = "%Y-%m-01")
  states <- unique(table$state)
  months <- unique(table$month)
  incidence_table <- NULL
  specific_mortality_table <- NULL
  for (state in states)
  {
    for (month in months)
    {
      selected <- table[table$month == month & table$state == state, ]
      incidence <- sum(selected$new_case)
      mortality <- sum(selected$new_death)
      if (is.null(incidence_table)) {
        incidence_table <- data.frame(c(month), c(as.Date(month)), c(state), c(incidence))
        specific_mortality_table <- data.frame(c(month), c(as.Date(month)), c(state), c(mortality))
        names(incidence_table) <- c("date", "ds", "state", "data")
        names(specific_mortality_table) <- c("date", "ds", "state", "data")
      } else {
        incidence_table[nrow(incidence_table) + 1, ] <- list(month, as.Date(month, format = "%Y-%m-%d"), state, incidence)
        specific_mortality_table[nrow(specific_mortality_table) + 1, ] <- list(month, as.Date(month, format = "%Y-%m-%d"), state, mortality)
      }
    }
  }
  incidence_table <- incidence_table[order(incidence_table$state, incidence_table$date), ]
  specific_mortality_table <- specific_mortality_table[order(specific_mortality_table$state, specific_mortality_table$date), ]
  result <- list(incidence = incidence_table, specific_mortality = specific_mortality_table)
  return(result)
}

load_mortality <- function(path) {
  table <- read.csv(file = path)
  table <- table[table$Indicator == "Number of Deaths" & table$Period == "Monthly" & tolower(table$State) %in% tolower(state.name), ]
  table$ds <- lubridate::mdy(paste0(table$Month, "/01/", table$Year))
  table$data <- table[, 6]
  table$state <- abbreviate_state(table$State)
  return(table)
}

.diff_foreach_state <- function(df) {
  states <- unique(df$state)
  df$data <- NA
  for (state in states)
  {
    df[df$state == state, ]$data <- diff(c(0, df[df$state == state, ]$cumm_data))
  }
  return(df)
}

load_hopkins_timeseries <- function(path) {
  print(paste0("Loading timeseries from ", path, "..."))
  table <- read.csv(file = path, check.names = FALSE)
  table <- table[table$FIPS < 80000, ]
  col_names <- colnames(table)
  date_start_index <- grep("\\d{1,2}/\\d{2}/\\d{2}", col_names)[1]
  date_columns <- date_start_index:ncol(table)
  table <- table %>%
    group_by(table$Province_State) %>%
    summarise(across(-date_columns, first), across(date_columns, sum))
  table <- table[!is.na(table$Province_State), ]
  table <- table[, 2:ncol(table)]
  table <- table %>% pivot_longer(cols = date_columns, names_to = "date", values_to = "data")
  table$ds <- as.Date(table$date, format = "%m/%d/%y")
  selected_dates <- unique(table$ds[day(table$ds) == days_in_month(table$ds)])
  table <- table[table$ds %in% selected_dates, ]
  result <- data.frame(ds = table$ds, state = abbreviate_state(table$Province_State), cumm_data = table$data)
  day(result$ds) <- 1
  result <- result[!is.na(result$state), ]
  result$date_num <- as.numeric(result$ds)
  result <- result[order(result$state, result$ds), ]
  result <- .diff_foreach_state(result)
  result$data <- .interpolate_negatives(result$data)
  print("Loading was successful")
  return(result)
}

load_population <- function(path) {
  table <- read.csv(file = path, check.names = FALSE)
  table$state <- abbreviate_state(table$NAME)
  table <- table[!is.na(table$state), ]
  table <- table %>% pivot_longer(cols = c("POPESTIMATE2020", "POPESTIMATE2021", "POPESTIMATE2022"), names_to = "year", values_to = "population")
  table$year <- as.integer(substr(table$year, 12, nchar(table$year)))
  result <- data.frame(state = table$state, year = table$year, population = table$population)
  return(result)
}

get_next_trend <- .make.get_next_trend()
