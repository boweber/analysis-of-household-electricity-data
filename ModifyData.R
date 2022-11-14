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