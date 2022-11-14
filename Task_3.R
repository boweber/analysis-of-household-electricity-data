source("ImportData.R")
source("GroupData.R")
source("FilterData.R")

heat_pump_data <- import_data()
heat_pump_data <- group_by_dataset(heat_pump_data, "6h")
heat_pump_data$has_heat_pump <- as.factor(rep(TRUE, nrow(heat_pump_data)))
heat_pump_data <- subset(
    heat_pump_data,
    select = -c(PUMPE_TOT, TEMPERATURE.TOTAL)
)

no_heat_pump_data <- import_ruhrgebiet_data()
no_heat_pump_data <- filter_in_timeinterval(no_heat_pump_data)
no_heat_pump_data <- group_by_dataset(no_heat_pump_data, "6h")
no_heat_pump_data$has_heat_pump <- as.factor(
    rep(FALSE, nrow(no_heat_pump_data))
)

training_dataset_indices <- sample(
    seq_len(ifelse(
        nrow(heat_pump_data) > nrow(no_heat_pump_data),
        nrow(no_heat_pump_data),
        nrow(heat_pump_data)
    )),
    size = floor(0.8 * ifelse(
        nrow(heat_pump_data) > nrow(no_heat_pump_data),
        nrow(no_heat_pump_data),
        nrow(heat_pump_data)
    ))
)
training_dataset <- tibble::add_row(
    heat_pump_data[training_dataset_indices, ],
    no_heat_pump_data[training_dataset_indices, ]
)

training_dataset <- training_dataset[
    -which(is.na(training_dataset$HAUSHALT_TOT)),
]


test_dataset <- tibble::add_row(
    heat_pump_data[-training_dataset_indices, ],
    no_heat_pump_data[-training_dataset_indices, ]
)


heatpump_forest_model <- randomForest::randomForest(
    has_heat_pump ~ HAUSHALT_TOT + index,
    data = training_dataset,
    importance = TRUE
)

test_predictions <- predict(heatpump_forest_model, test_dataset, type = "class")

mean(test_predictions == test_dataset$has_heat_pump, na.rm = TRUE)
table(test_predictions, test_dataset$has_heat_pump)
