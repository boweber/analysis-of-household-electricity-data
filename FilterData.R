filter_in_timeinterval <- function(dataset,
                                   first_date = readr::parse_datetime(
                                       "2019-01-01 00:00:00",
                                       "%Y-%m-%d %H:%M:%S"
                                   ),
                                   last_date = readr::parse_datetime(
                                       "2019-12-31 23:00:00",
                                       "%Y-%m-%d %H:%M:%S"
                                   )) {
    all_households <- unique(dataset$household)
    filtered_dataset <- NULL

    for (household in all_households) {
        household_data <- dataset[
            which(dataset$household == household),
        ]
        if (nrow(household_data) == 0 ||
            household_data[1, ]$index > first_date) {
            next
        }
        households_first_date_index <- 1
        while (
            household_data[households_first_date_index, ]$index < first_date
        ) {
            households_first_date_index <- households_first_date_index + 1
        }

        households_last_date_index <- nrow(household_data)
        while (
            household_data[households_last_date_index, ]$index > last_date
        ) {
            households_last_date_index <- households_last_date_index - 1
        }

        completed <- tibble::tibble(
            index = seq(first_date, last_date, by = "15 min")
        )
        completed$household <- as.factor(
            rep(household_data$household[1], nrow(completed))
        )
        merged_data <- merge(
            household_data[
                households_first_date_index:households_last_date_index,
            ],
            completed,
            all.y = TRUE
        )
        na_indices <- which(is.na(merged_data$HAUSHALT_TOT))
        if (length(na_indices) > (nrow(merged_data) * 0.8)) {
            next
        }

        if (is.null(filtered_dataset)) {
            filtered_dataset <- household_data[
                households_first_date_index:households_last_date_index,
            ]
        } else {
            filtered_dataset <- tibble::add_row(
                filtered_dataset,
                household_data[
                    households_first_date_index:households_last_date_index,
                ]
            )
        }
    }
    return(filtered_dataset)
}
