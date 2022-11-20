library(tidyverse)
library(lubridate)
source("import_data.R")
source("longest_na_sequence.R")
source("descriptive_analysis_plots.R")
set.seed(191122)

heatpump_datasets <- load_heatpump_datasets()

complete_time_frame <- tibble(
    index = seq(
        from = min(heatpump_datasets$index),
        to = max(heatpump_datasets$index),
        by = "15 min"
    )
)

complete_temperature <- heatpump_datasets %>%
    select(index, TEMPERATURE.TOTAL) %>%
    group_by(index) %>%
    summarise(TEMPERATURE.TOTAL = TEMPERATURE.TOTAL[1]) %>%
    arrange(index)

complete_dataset <- complete_time_frame %>%
    full_join(complete_temperature)

descriptive_results <- list()
household_names <- unique(heatpump_datasets$household_name)
for (household in household_names) {
    household_dataset <- heatpump_datasets %>%
        filter(household_name == household)

    ## adds missing time steps to the dataset
    household_dataset <- complete_dataset %>%
        full_join(household_dataset)

    ## investigate the missing values

    household_results <- list()

    household_results$longest_na_sequence <- longest_na_sequence(
        household_dataset
    )
    household_results$total_number_na <- length(
        which(is.na(household_dataset$TEMPERATURE.TOTAL))
    )

    ## household_results$relative_na_plot <- relative_na_plot(household_dataset)

    ## investigate the heatpump_data

    household_results$heatpump_data <- household_dataset %>%
        select(PUMPE_TOT, index) %>%
        summarise(
            max_value = max(PUMPE_TOT, na.rm = TRUE),
            max_value_time = index[which.max(PUMPE_TOT)],
            min_value = min(PUMPE_TOT = TRUE),
            min_value_time = index[which.min(PUMPE_TOT)],
            mean_value = mean(PUMPE_TOT, na.rm = TRUE),
            trimmed_mean_value = mean(PUMPE_TOT, trim = 0.1, na.rm = TRUE),
            median_value = median(PUMPE_TOT, na.rm = TRUE),
            quantile_25 = quantile(PUMPE_TOT, 0.25, na.rm = TRUE),
            quantile_75 = quantile(PUMPE_TOT, 0.75, na.rm = TRUE)
        )


    ## investigate the household_tot

    household_results$household_tot_data <- household_dataset %>%
        select(HAUSHALT_TOT, index) %>%
        summarise(
            max_value = max(HAUSHALT_TOT, na.rm = TRUE),
            max_value_time = index[which.max(HAUSHALT_TOT)],
            min_value = min(HAUSHALT_TOT = TRUE),
            min_value_time = index[which.min(HAUSHALT_TOT)],
            mean_value = mean(HAUSHALT_TOT, na.rm = TRUE),
            trimmed_mean_value = mean(HAUSHALT_TOT, trim = 0.1, na.rm = TRUE),
            median_value = median(HAUSHALT_TOT, na.rm = TRUE),
            quantile_25 = quantile(HAUSHALT_TOT, 0.25, na.rm = TRUE),
            quantile_75 = quantile(HAUSHALT_TOT, 0.75, na.rm = TRUE)
        )

    household_results$heatpump_temperature_plot <- heatpump_temperature_plot(household_dataset)

    household_results$heatpump_temperature_over_time_plot <- heatpump_temperature_over_time_plot(
        household_dataset
    )

    household_results$heatpump_household_consumption_over_time_plot <- heatpump_household_consumption_over_time(household_dataset)

    household_results$heatpump_household_consumption <- heatpump_household_consumption(household_dataset)
    household_results$household_name <- household
    descriptive_results[[household]] <- household_results
}

save(descriptive_results, file = "data/derived/descriptive_results.RData")

for (household_result in descriptive_results) {
    # ggsave(
    #     filename = paste0(
    #         "figures/",
    #         household_result$household_name,
    #         "_relative_na_plot.pdf"
    #     ),
    #     plot = household_result$relative_na_plot
    # )

    ggsave(
        filename = paste0(
            "figures/",
            household_result$household_name,
            "_heatpump_temperature_plot.pdf"
        ),
        plot = household_result$heatpump_temperature_plot
    )

    ggsave(
        filename = paste0(
            "figures/",
            household_result$household_name,
            "_heatpump_household_consumption_over_time_plot.pdf"
        ),
        plot = household_result$heatpump_household_consumption_over_time_plot
    )

    ggsave(
        filename = paste0(
            "figures/",
            household_result$household_name,
            "_heatpump_household_consumption.pdf"
        ),
        plot = household_result$heatpump_household_consumption
    )

    ggsave(
        filename = paste0(
            "figures/",
            household_result$household_name,
            "_heatpump_temperature_over_time_plot.pdf"
        ),
        plot = household_result$heatpump_temperature_over_time_plot
    )
}

random_households <- sample(household_names, 5)

household_comparison_plot <- household_comparison(
    filter(heatpump_datasets, household_name %in% random_households)
)

ggsave(
    "figures/household_comparison_plot.pdf",
    plot = household_comparison_plot
)
