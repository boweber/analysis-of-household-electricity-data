library("tidyverse") ## %+%
source("ImportData.R")
source("ModifyData.R")

dataset <- import_data()

## MARK: - Determine Lag

correlation_values <- list()
all_households <- unique(dataset$household)
for (household in all_households) {
    household_data <- dataset[which(dataset$household == household), ]
    household_data <- insert_missing_data(household_data)
    household_data <- interpolate_missing_data(household_data)
    threshold_indices <- which(
        !is.na(household_data$PUMPE_TOT) &
            household_data$TEMPERATURE.TOTAL <= 18
    )
    household_correlation <- c()
    for (time_index in 0:48) {
        threshold_indices <- threshold_indices[
            which(threshold_indices > time_index)
        ]
        household_correlation <- c(
            household_correlation,
            cor(
                household_data$PUMPE_TOT[threshold_indices],
                household_data$TEMPERATURE.TOTAL[threshold_indices - time_index],
                use = "complete.obs"
            )
        )
    }
    correlation_values[[household]] <- household_correlation
}

par(mfrow = c(2, 3))
for (household_index in all_households[1:6]) {
    plot(
        seq(0, 0.25 * 48, 0.25),
        correlation_values[[household_index]],
        xlab = "Zeit in Stunden",
        ylab = "Kreuzkorrelation",
        main = paste("Haus ", household_index),
        pch = 20
    )
}

household_min_correlation <- list()
for (household in all_households) {
    household_min_correlation[[household]] <- data.frame(
        index = which.min(correlation_values[[household]]),
        value = min(correlation_values[[household]])
    )
}

## MARK: - Mixed Model

grouped_dataset <- group_by_day_and_night(dataset)
grouped_dataset <- grouped_dataset[
    which(grouped_dataset$TEMPERATURE.TOTAL <= 18),
]


mixed_model <- lme4::lmer(
    PUMPE_TOT ~ TEMPERATURE.TOTAL + (1 + TEMPERATURE.TOTAL | household),
    data = grouped_dataset[which(grouped_dataset$isDay == "night"), ],
)

MuMIn::r.squaredGLMM(mixed_model)

coefficients_plot <- ggplot2::ggplot(
    data = grouped_dataset,
    mapping = ggplot2::aes(
        x = TEMPERATURE.TOTAL,
        y = PUMPE_TOT,
        colour = household
    )
) +
    ggplot2::geom_point(na.rm = TRUE, alpha = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(
        intercept = Intercept,
        slope = Slope,
        colour = household
    ),
    size = 1.5
    ) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::scale_y_continuous(limits = c(-100, 6500)) +
    ggplot2::scale_x_continuous(limits = c(-10, 20)) +
    ggplot2::theme_minimal()


coefficients <- dplyr::rename(
    coef(mixed_model)$household,
    Intercept = `(Intercept)`,
    Slope = TEMPERATURE.TOTAL
)
coefficients$household <- as.factor(row.names(coefficients))

joined_model <- dplyr::left_join(
    grouped_dataset,
    coefficients,
    by = "household"
)
plot1 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) <= 3), ]
plot2 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 3 &
    as.numeric(joined_model$household) <= 6), ]
plot3 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 6 &
    as.numeric(joined_model$household) <= 9), ]
plot4 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 9 &
    as.numeric(joined_model$household) <= 12), ]
plot5 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 12 &
    as.numeric(joined_model$household) <= 15), ]
plot6 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 15 &
    as.numeric(joined_model$household) <= 18), ]
plot7 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 18 &
    as.numeric(joined_model$household) <= 21), ]
plot8 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 21 &
    as.numeric(joined_model$household) <= 24), ]
plot9 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 24 &
    as.numeric(joined_model$household) <= 27), ]
plot10 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 27 &
    as.numeric(joined_model$household) <= 30), ]
plot11 <- coefficients_plot %+% joined_model[which(as.numeric(joined_model$household) > 30 &
    as.numeric(joined_model$household) <= 33), ]

ggpubr::ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3, nrow = 2) +
    ggplot2::labs(title = "Zusammenhang am Tag")
ggpubr::ggarrange(plot7, plot8, plot9, plot10, plot11, ncol = 3, nrow = 2) +
    ggplot2::labs(title = "Zusammenhang am Tag")


