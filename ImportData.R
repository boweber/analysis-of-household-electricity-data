## a function to import the data
##
## on_import can be a function that is called on each dataset.
## -> this can be used to modify the dataset before
## adding the dataset to the list
##
## file_numbers can be a vector of file numbers or a
## number and determines which files are imported
## if file_numbers is null, all files are imported
##
## file_path specifies the path to the files, by
## default the files are expected to be in
## a folder called "OHNE PV" in the current working directory
##
## returns a list of datasets
import_data <- function(file_numbers = NULL, file_path = "OHNE\ PV/") {
    ## A helper function to get the file path of a specific number
    ## e.g.: get_file_path(3) returns "OHNE PV/SFH3.csv"
    file_directory <- function(number) {
        file_name <- paste("SFH", number, ".csv", sep = "")
        return(paste(file_path, file_name, sep = ""))
    }

    if (!is.null(file_numbers) && !is.numeric(file_numbers)) {
        stop("file_numbers must be numeric")
    }
    data <- NULL
    for (number in seq(3, 40)) {
        ## file_number defines all data files that should be imported
        ## if any number does not match any number in file_numbers
        ## skip the iteration (aka. the import of the file)
        if (!is.null(file_numbers) && !(number %in% file_numbers)) {
            next
        }
        if (file.exists(file_directory(number))) {
            file_data <- readr::read_csv(file_directory(number))
            file_data <- dplyr::rename(
                file_data,
                "TEMPERATURE.TOTAL" = "TEMPERATURE:TOTAL"
            )
            file_data$household <- as.factor(rep(number, nrow(file_data)))
            if (is.null(data)) {
                data <- file_data
            } else {
                data <- tibble::add_row(data, file_data)
            }
        }
    }
    return(data)
}

import_ruhrgebiet_data <- function(cities = NULL) {
    last_index <- stringr::str_length("Ruhrgebiet - Daten//")

    data <- NULL
    for (folder_name in list.dirs(path = "Ruhrgebiet - Daten")) {
        city <- substring(
            folder_name,
            last_index,
            stringr::str_length(folder_name)
        )
        if (!is.null(cities) && !(city %in% cities)) {
            next
        }
        if (stringr::str_length(city) > 0) {
            file_path <- paste(folder_name, "/", city, ".csv", sep = "")

            file_number_of_city <- 1
            for (file_name in list.files(path = folder_name)) {
                if (grepl(".csv", file_name)) {
                    file_path <- paste(folder_name, "/", file_name, sep = "")
                    if (file.exists(file_path)) {
                        file_data <- readr::read_csv(
                            file_path,
                            col_types = list(
                                readr::col_datetime(),
                                readr::col_double()
                            )
                        )
                        file_data <- dplyr::rename(
                            file_data,
                            "index" = "Zeitstempel",
                            "HAUSHALT_TOT" = "Messwert"
                        )
                        household <- paste(city, file_number_of_city, sep = "_")
                        file_data$household <- as.factor(
                            rep(household, nrow(file_data))
                        )
                        na_indices <- which(is.na(file_data$index))
                        if (length(na_indices) > 0) {
                            file_data <- file_data[-na_indices, ]
                        }
                        if (is.null(data)) {
                            data <- file_data
                        } else {
                            data <- tibble::add_row(data, file_data)
                        }
                        file_number_of_city <- file_number_of_city + 1
                    }
                }
            }
        }
    }
    return(data)
}
