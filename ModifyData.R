insert_missing_data <- function(household_dataset, by_minutes = 15) {
    if ((by_minutes %% 15) != 0) {
        stop("by_minutes must be a multiple of 15")
    }
    if (by_minutes < 0) {
        stop("by_minutes must be positive")
    }
    first_date <- household_dataset[1, ]$index
    last_date <- household_dataset[nrow(household_dataset), ]$index

    completed <- tibble::tibble(
        index = seq(first_date, last_date, by = paste(by_minutes, "min"))
    )
    completed$household <- as.factor(
        rep(household_dataset$household[1], nrow(completed))
        )
    return(merge(household_dataset, completed, all.y = TRUE))
}

interpolate_missing_data <- function(household_dataset,
                                     column_name = "TEMPERATURE.TOTAL",
                                     interpolate = base::mean) {
    index <- 1

    while (any(is.na(household_dataset[[column_name]]))) {
        missing_data_indices <- which(is.na(household_dataset[[column_name]]))
        before_after_values <- matrix(c(
            household_dataset[[column_name]][missing_data_indices - index],
            household_dataset[[column_name]][missing_data_indices + index]
        ), ncol = 2)
        interpolated_data <- apply(
            before_after_values, 1, interpolate,
            na.rm = TRUE
        )
        household_dataset[[column_name]][missing_data_indices] <- interpolated_data
        index <- index + 1
        if (index > 4) {
            warning("Stopped interpolation after 4 iterations")
            break
        }
    }
    return(household_dataset)
}

remove_missing_data <- function(dataset,
                                column_name = "TEMPERATURE.TOTAL",
                                threshold = 0.01) {
    na_indices <- which(is.na(dataset[[column_name]]))
    if (length(na_indices) / nrow(dataset) > threshold) {
        warning(paste(
            "More than ", threshold * 100, "% of the data is missing"
        ))
    }
    return(dataset[-which(is.na(dataset[[column_name]])), ])
}

group_by_day_and_night <- function(dataset) {
    all_households <- unique(dataset$household)

    grouped_dataset <- tibble::tibble(
        PUMPE_TOT = numeric(),
        TEMPERATURE.TOTAL = numeric(),
        household = factor(),
        isDay = factor()
    )
    for (household in all_households) {
        household_data <- dataset[which(dataset$household == household), ]
        household_data <- insert_missing_data(household_data)
        if (nrow(household_data) == 0) {
            next
        }
        hours <- as.numeric(format(household_data$index, "%H"))
        household_data$isDay <- as.factor(
            ifelse(hours >= 8 & hours < 20, "day", "night")
        )

        first_day_night_shift_index <- 1
        first_is_day <- household_data$isDay[1]
        while (first_is_day == household_data$isDay[first_day_night_shift_index]) {
            first_day_night_shift_index <- first_day_night_shift_index + 1
        }

        last_day_night_shift_index <- nrow(household_data)
        last_is_day <- household_data$isDay[nrow(household_data)]
        while (last_is_day == household_data$isDay[last_day_night_shift_index]) {
            last_day_night_shift_index <- last_day_night_shift_index - 1
        }

        number_of_day_night_changes <- (
            last_day_night_shift_index - first_day_night_shift_index + 1
        ) / (12 * 4)
        if (number_of_day_night_changes %% 1 != 0) {
            stop(paste(
                "number_of_day_night_changes must be an integer",
                number_of_day_night_changes,
                "current household:",
                household
            ))
        }

        first_group <- rep(1, (first_day_night_shift_index - 1))
        middle_groups <- rep(2:(number_of_day_night_changes + 1), each = 12 * 4)
        last_group <- rep(
            number_of_day_night_changes + 2,
            (nrow(household_data) - last_day_night_shift_index)
        )
        household_data$group <- c(first_group, middle_groups, last_group)
        grouped_household_data <- dplyr::summarise(
            dplyr::group_by(household_data, group),
            PUMPE_TOT = mean(PUMPE_TOT, trim = 0.05, na.rm = TRUE),
            isDay = isDay[1],
            TEMPERATURE.TOTAL = mean(TEMPERATURE.TOTAL, na.rm = TRUE)
        )
        grouped_household_data <- dplyr::select(grouped_household_data, -group)
        grouped_household_data$household <- as.factor(rep(
            household, nrow(grouped_household_data)
        ))
        grouped_dataset <- tibble::add_row(
            grouped_dataset, grouped_household_data
        )
    }
    return(grouped_dataset)
}
