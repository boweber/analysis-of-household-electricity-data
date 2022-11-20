load_heatpump_datasets <- function() {
    csv_file_paths <- list.files(
        path = "data/original/OHNE PV",
        full.names = TRUE,
        pattern = ".+\\.csv"
    )
    heatpump_data <- NULL
    directory_name_length <- stringr::str_length("data/original/OHNE PV/")
    file_type_length <- stringr::str_length(".csv")

    for (path in csv_file_paths) {
        file_data <- readr::read_csv(
            path,
            col_types = list(
                readr::col_datetime(),
                readr::col_double(),
                readr::col_double(),
                readr::col_double()
            )
        )
        file_data <- dplyr::rename(
            file_data,
            "TEMPERATURE.TOTAL" = "TEMPERATURE:TOTAL"
        )
        file_name <- substring(
            path,
            directory_name_length + 1,
            stringr::str_length(path) - file_type_length
        )
        file_data$household_name <- as.factor(rep(file_name, nrow(file_data)))

        if (is.null(heatpump_data)) {
            heatpump_data <- file_data
        } else {
            heatpump_data <- dplyr::bind_rows(heatpump_data, file_data)
        }
    }
    return(heatpump_data)
}
