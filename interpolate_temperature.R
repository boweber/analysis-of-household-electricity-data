interpolate_temperature <- function(household_dataset) {
    index <- 1

    while (any(is.na(household_dataset$TEMPERATURE.TOTAL))) {
        missing_data_indices <- which(is.na(household_dataset$TEMPERATURE.TOTAL))
        before_after_values <- matrix(c(
            household_dataset$TEMPERATURE.TOTAL[missing_data_indices - index],
            household_dataset$TEMPERATURE.TOTAL[missing_data_indices + index]
        ), ncol = 2)
        interpolated_data <- apply(
            before_after_values, 1, mean,
            na.rm = TRUE
        )
        household_dataset$TEMPERATURE.TOTAL[missing_data_indices] <- interpolated_data
        index <- index + 1
        if (index > 4) {
            warning("Stopped interpolation after 4 iterations")
            break
        }
    }
    return(household_dataset)
}