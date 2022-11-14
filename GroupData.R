group_by_dataset <- function(dataset, group_by_timeframe) {
    timeframe_of_hour <- function(hour) {
        if (hour >= 0 && hour < 6) {
            return("night")
        } else if (hour >= 6 && hour < 12) {
            return("morning")
        } else if (hour >= 12 && hour < 18) {
            return("afternoon")
        } else if (hour >= 18 && hour < 24) {
            return("evening")
        }
    }

    inject_missing_times <- function(household_dataset) {
        first_date <- household_dataset[1, ]$index
        last_date <- household_dataset[nrow(household_dataset), ]$index

        expected_first_date <- readr::parse_datetime(
            "2019-01-01 00:00:00",
            "%Y-%m-%d %H:%M:%S"
        )

        expected_last_date <- readr::parse_datetime(
            "2019-12-31 23:00:00",
            "%Y-%m-%d %H:%M:%S"
        )
        if (first_date != expected_first_date ||
            last_date != expected_last_date) {

        }
        completed <- tibble::tibble(
            index = seq(first_date, last_date, by = "15 min")
        )
        completed$household <- as.factor(
            rep(household_dataset$household[1], nrow(completed))
        )
        return(merge(household_dataset, completed, all.y = TRUE))
    }

    groups_by_indices <- function(household_data) {
        number_of_changes_in_middle <- switch(group_by_timeframe,
            "6h" = {
                6 * 4
            },
            "season" = {
                1
            },
            "day_night" = {
                (12 * 4)
            }
        )
        time_points <- as.numeric(format(
            household_data$index,
            switch(group_by_timeframe,
                "6h" = {
                    "%H"
                },
                "season" = {
                    "%m"
                },
                "day_night" = {
                    "%H"
                }
            )
        ))
        household_data$timeframe <- as.factor(switch(group_by_timeframe,
            "6h" = {
                sapply(time_points, timeframe_of_hour)
            },
            "season" = {
                ifelse(time_points >= 4 & time_points < 10, "summer", "winter")
            },
            "day_night" = {
                ifelse(time_points >= 8 & time_points < 20, "day", "night")
            }
        ))

        first_shift_index <- 1
        first_data_point <- household_data$timeframe[1]
        while (
            first_data_point == household_data$timeframe[first_shift_index]
        ) {
            first_shift_index <- first_shift_index + 1
        }

        last_shift_index <- nrow(household_data)
        last_data_point <- household_data$timeframe[nrow(household_data)]
        while (
            last_data_point == household_data$timeframe[last_shift_index]
        ) {
            last_shift_index <- last_shift_index - 1
        }

        number_of_changes <- (
            last_shift_index - first_shift_index + 1
        ) / number_of_changes_in_middle

        if (number_of_changes %% 1 != 0) {
            stop(paste(
                "number_of_day_night_changes must be an integer",
                number_of_changes,
                "current household:",
                household_data$household
            ))
        }

        first_group <- rep(1, (first_shift_index - 1))
        middle_groups <- rep(2:(number_of_changes + 1),
            each = number_of_changes_in_middle
        )
        last_group <- rep(
            number_of_changes + 2,
            (nrow(household_data) - last_shift_index)
        )
        return(list(
            groups = c(first_group, middle_groups, last_group),
            timeframe = household_data$timeframe
        ))
    }

    group_by <- function(...) {
        all_households <- unique(dataset$household)
        grouped_dataset <- NULL

        for (household in all_households) {
            household_data <- dataset[which(dataset$household == household), ]
            household_data <- inject_missing_times(household_data)
            if (nrow(household_data) == 0) {
                next
            }
            groups <- groups_by_indices(household_data)
            household_data$group <- groups$groups
            household_data$timeframe <- groups$timeframe
            grouped_household_data <- NULL
            if (group_by_timeframe == "season") {
                grouped_household_data <- dplyr::summarise(
                    dplyr::group_by(household_data, group),
                    HAUSHALT_TOT = mean(HAUSHALT_TOT, trim = 0.05, na.rm = TRUE),
                    timeframe = timeframe[1],
                    ...
                )
            } else {
                grouped_household_data <- dplyr::summarise(
                    dplyr::group_by(household_data, group),
                    HAUSHALT_TOT = mean(HAUSHALT_TOT, trim = 0.05, na.rm = TRUE),
                    index = index[1],
                    timeframe = timeframe[1],
                    ...
                )
            }

            grouped_household_data <- dplyr::select(
                grouped_household_data,
                -group
            )
            grouped_household_data$household <- as.factor(rep(
                household, nrow(grouped_household_data)
            ))
            if (is.null(grouped_dataset)) {
                grouped_dataset <- grouped_household_data
            } else {
                grouped_dataset <- tibble::add_row(
                    grouped_dataset, grouped_household_data
                )
            }
        }
        return(grouped_dataset)
    }

    if ("PUMPE_TOT" %in% colnames(dataset)) {
        ## heatpump data
        return(group_by(
            PUMPE_TOT = mean(PUMPE_TOT, trim = 0.05, na.rm = TRUE),
            TEMPERATURE.TOTAL = mean(TEMPERATURE.TOTAL, na.rm = TRUE)
        ))
    } else {
        return(group_by())
    }
}
