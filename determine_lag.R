source("import_data.R")
source("interpolate_temperature.R")
library(tidyverse)
library(lubridate)

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

complete_dataset <- interpolate_temperature(complete_dataset)

lag_results <- list()
for (household in unique(heatpump_datasets$household_name)) {
    household_dataset <- heatpump_datasets %>%
        filter(household_name == household)

    ## adds missing time steps to the dataset
    household_dataset <- complete_dataset %>%
        full_join(household_dataset) %>%
        filter(TEMPERATURE.TOTAL <= 18 &
            PUMPE_TOT < quantile(PUMPE_TOT, 0.99, na.rm = TRUE))

    ## investigate the missing values

    household_results <- list()

    ccf_result <- ccf(
        household_dataset$PUMPE_TOT,
        household_dataset$TEMPERATURE.TOTAL,
        na.action = na.pass
    )
    start_index <- which(ccf_result$lag == 0)
    ccf_result <- tibble(
        acf = ccf_result$acf[start_index:length(ccf_result$acf)],
        lag = (seq(
            from = 0,
            to = length(ccf_result$acf) - start_index
        ) * 15) / 60
    )

    household_results$ccf <- ccf_result

    x_anotation_position <- ifelse(
        ccf_result$lag[which.min(ccf_result$acf)] > mean(ccf_result$lag),
        ccf_result$lag[which.min(ccf_result$acf)] - 3,
        ccf_result$lag[which.min(ccf_result$acf)]
    )
    
     

    household_results$plot <- ccf_result %>%
        ggplot(aes(x = lag, y = acf)) +
        geom_segment(
            aes(x = lag, xend = lag, y = 0, yend = acf),
            color = "grey"
        ) +
        geom_point(
            color = ifelse(
                ccf_result$acf == min(ccf_result$acf),
                "red",
                "orange"
            ),
            size = 4
        ) +
        theme_light() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank()
        ) +
        xlab("lag (h)") +
        ylab("cross-correlation") +
        ggtitle(paste(
            "cross-correlation of heatpump and temperature data shifted by time (",
            household,
            ")"
        )) +
        annotate(
            "text",
            x = x_anotation_position,
            y = min(ccf_result$acf) * 1.1,
            label = paste(
                "cross-correlation of",
                round(min(ccf_result$acf), 2),
                "\nobserved for a lag of",
                ccf_result$lag[which.min(ccf_result$acf)] * 60,
                "min"
            ),
            color = "red",
            size = 4,
            angle = 0,
            fontface = "bold",
            hjust = 0
        )
    household_results$household <- household
    lag_results[[household]] <- household_results
}
save(lag_results, file = "data/derived/lag_results.RData")

for (result in lag_results) {
    ggsave(
        filename = paste(
            "figures/lag/",
            result$household,
            "_lag.pdf",
            sep = ""
        ),
        plot = result$plot
    )
}
