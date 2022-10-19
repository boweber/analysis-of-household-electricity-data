library("tibble")
## a function to import the data
##
## on_import can be a function that is called on each dataset.
## -> this can be used to modify the dataset before adding the dataset to the list
##
## file_numbers can be a vector of file numbers or a number and determines which files are imported
## if file_numbers is null, all files are imported
##
## file_path specifies the path to the files, by default the files are expected to be in
## a folder called "OHNE PV" in the current working directory
##
## returns a list of datasets
import_data <- function(on_import = NULL, file_numbers = NULL, file_path = "OHNE\ PV/") {
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
        if (!(number %in% file_numbers)) {
            next
        }
        if (file.exists(file_directory(number))) {
            file_data <- readr::read_csv(file_directory(number))
            file_data <- file_data %>% dplyr::rename("TEMPERATURE.TOTAL" = "TEMPERATURE:TOTAL")
            file_data$household <- rep(number, nrow(file_data))
            if (!is.null(on_import)) {
                file_data <- on_import(file_data)
            }
            if (is.null(data)) {
                data <- file_data
            } else {
                data <- data %>% tibble::add_row(file_data)
            }
        }
    }
    return(data)
}
