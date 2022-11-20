longest_na_sequence <- function(household_dataset) {
    na_indices <- which(is.na(household_dataset$TEMPERATURE.TOTAL))
    run_length_encoded <- rle(diff(na_indices))
    sequence_lengths <- max(run_length_encoded$lengths) + 1

    end_index <- sum(
        run_length_encoded$lengths[
            seq_len(which.max(run_length_encoded$lengths))
        ]
    ) + 1

    return(list(
        start_date = household_dataset$index[end_index - sequence_lengths + 1],
        end_date =household_dataset$index[end_index]
    ))
}