library(lubridate) ## required for %--%
library(tibble) ##  required for %>%

shift_dates <- function(dataset,
                        by_minutes,
                        shift_data_column_name = c("PUMPE_TOT"),
                        date_column_name = "index") {
    if (by_minutes == 0 || nrow(dataset) == 0) {
        return(dataset)
    }
    if ((by_minutes %% 15) != 0) {
        stop("by_minutes must be a multiple of 15")
    }
    if (by_minutes < 0) {
        stop("by_minutes must be positive")
    }
    if (!lubridate::is.POSIXt(dataset[[date_column_name]])) {
        stop(paste(date_column_name, " must be of type POSIXt"))
    }
    number_of_missing_data_points <- 0
    determine_index <- function(index, next_index) {
        if (nrow(dataset) < next_index) {
            return(NULL)
        } else if (index == next_index) {
            return(determine_index(index, next_index + 1))
        } else if (nrow(dataset) < index) {
            stop("Invalid index")
        }
        current_date <- dataset[index, ][[date_column_name]]
        next_date <- dataset[next_index, ][[date_column_name]]
        ## the elpased minutes between the two dates
        time_interval <- lubridate::as.duration(current_date %--% next_date) / 60
        if ((time_interval %% 15) != 0) {
            stop("time_interval must be a multiple of 15")
        }
        if (time_interval == by_minutes) {
            return(next_index)
        } else if (time_interval > by_minutes) {
            number_of_missing_data_points <- number_of_missing_data_points + 1
            return(next_index)
        } else {
            return(determine_index(index, next_index + 1))
        }
    }

    # construct a new empty dataset with the same column names as the dataset
    shifted_data <- dataset[1, ]
    shifted_data <- shifted_data[-1, ]
    time_index <- 1
    for (data_index in seq_len(nrow(dataset))) {
        if (data_index %% 1000 == 0) {
            print(paste("Currently at index", data_index))
        }
        time_index <- determine_index(data_index, time_index)
        if (is.null(time_index)) {
            break
        } else {
            new_row <- dataset[data_index, ]
            if (is.vector(shift_data_column_name)) {
                for (column_name in shift_data_column_name) {
                    new_row[[column_name]] <- dataset[time_index, ][[column_name]]
                }
            } else {
                new_row[[shift_data_column_name]] <- dataset[time_index, ][[shift_data_column_name]]
            }
            shifted_data <- shifted_data %>% tibble::add_row(tibble::tibble_row(new_row))
        }
    }
    if (number_of_missing_data_points > 0) {
        warning(paste(number_of_missing_data_points, " data points were missing"))
    }
    return(shifted_data)
}

insert_missing_data <- function(dataset, by_minutes = 30) {
    if ((by_minutes %% 15) != 0) {
        stop("by_minutes must be a multiple of 15")
    }
    if (by_minutes < 0) {
        stop("by_minutes must be positive")
    }
    first_date <- dataset[1, ]$index
    last_date <- dataset[nrow(dataset), ]$index

    completed <- tibble(index = seq(first_date, last_date, by = paste(by_minutes, "min")))
    completed$household <- rep(dataset$household[1], nrow(completed))
    return(merge(dataset, completed, all.y = TRUE))
}

interpolate_missing_data <- function(dataset, column_name = "TEMPERATURE.TOTAL", interpolate = base::mean) {
    index <- 1

    while (any(is.na(dataset[[column_name]]))) {
        missing_data_indices <- which(is.na(dataset[[column_name]]))
        before_after_values <- matrix(c(dataset[[column_name]][missing_data_indices - index], dataset[[column_name]][missing_data_indices + index]), ncol = 2)
        interpolated_data <- apply(before_after_values, 1, interpolate, na.rm = TRUE)
        dataset[[column_name]][missing_data_indices] <- interpolated_data
        index <- index + 1
        if (index > 4) {
            warning("Stopped interpolation after 4 iterations")
            break
        }
    }
    return(dataset)
}

remove_missing_data <- function(dataset, column_name = "TEMPERATURE.TOTAL", threshold = 0.01) {
    na_indices <- which(is.na(dataset[[column_name]]))
    if (length(na_indices) / nrow(dataset) > threshold) {
        warning(paste("More than ", threshold * 100, "% of the data is missing"))
    }
    return(dataset[-which(is.na(dataset[[column_name]])), ])
}
