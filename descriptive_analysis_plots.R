library(tidyverse)
library(lubridate)
library(broom)

relative_na_plot <- function(household_dataset) {
    household_dataset %>%
        group_by(
            year_ = year(index),
            month_ = month(index, label = TRUE)
        ) %>%
        summarise(
            mean_na = mean(is.na(TEMPERATURE.TOTAL))
        ) %>%
        ggplot(
            aes(
                x = month_,
                y = mean_na,
                yend = 1
            )
        ) +
        geom_segment(
            aes(
                x = month_,
                xend = month_,
                y = 0,
                yend = mean_na
            ),
            color = "grey"
        ) +
        geom_point(color = "orange", size = 4, na.rm = TRUE) +
        xlab("Month (2019)") +
        coord_flip() +
        ylab("Relative number of missing observations") +
        ggtitle(
            paste(
                "Relative number of missing values per month (",
                household_dataset$household_name[1],
                ")",
                sep = ""
            )
        )
}

heatpump_temperature_plot <- function(household_dataset) {
    fit_household_dataset <- household_dataset %>%
        mutate(
            in_model = TEMPERATURE.TOTAL <= 18 &
                PUMPE_TOT < quantile(PUMPE_TOT, 0.99, na.rm = TRUE) &
                !is.na(PUMPE_TOT)
        )

    linear_fit <- lm(
        PUMPE_TOT ~ TEMPERATURE.TOTAL,
        data = fit_household_dataset[which(fit_household_dataset$in_model), ]
    )

    fit_household_dataset %>%
        ggplot(
            aes(
                x = TEMPERATURE.TOTAL,
                y = PUMPE_TOT,
                color = in_model
            )
        ) +
        geom_point(na.rm = TRUE, alpha = 0.2) +
        geom_line(
            inherit.aes = FALSE,
            data = augment(linear_fit),
            aes(x = TEMPERATURE.TOTAL, y = .fitted),
            color = 9,
            size = 1.5,
            alpha = 0.8
        ) +
        labs(
            x = "Temperature (Celsius °)",
            y = "heat pump power consumption (W)",
            color = "In linear model"
        ) +
        theme(
            axis.title.y = element_text(size = 13),
            axis.title.x = element_text(size = 13)
        ) +
        ggtitle(paste(
            "Heat pump power consumption and temperature comparison (",
            household,
            ")",
            sep = ""
        ))
}

heatpump_temperature_over_time_plot <- function(household_dataset) {
    household_dataset %>%
        group_by(
            date = date(index)
        ) %>%
        summarise(
            TEMPERATURE.TOTAL = mean(TEMPERATURE.TOTAL, na.rm = TRUE),
            PUMPE_TOT = mean(PUMPE_TOT, na.rm = TRUE)
        ) %>%
        ggplot(aes(x = date)) +
        geom_line(aes(y = PUMPE_TOT), size = 1, color = "#009E73") +
        geom_line(
            aes(
                y = TEMPERATURE.TOTAL * 60
            ),
            size = 1,
            color = "#E69F00"
        ) +
        scale_y_continuous(
            name = "heat pump power consumption (W)",
            sec.axis = sec_axis(
                ~ . / 60,
                name = "Temperature (Celsius °)"
            )
        ) +
        theme(
            axis.title.y = element_text(color = "#009E73", size = 13),
            axis.title.y.right = element_text(color = "#E69F00", size = 13)
        ) +
        labs(x = NULL) +
        ggtitle(
            paste(
                "Heat pump power consumption and temperature over the year 2019 (",
                household,
                ")",
                sep = ""
            )
        )
}

heatpump_household_consumption <- function(household_dataset) {
    household_dataset %>%
        group_by(
            date = date(index)
        ) %>%
        summarise(
            PUMPE_TOT = mean(PUMPE_TOT, na.rm = TRUE),
            HAUSHALT_TOT = mean(HAUSHALT_TOT, na.rm = TRUE)
        ) %>%
        mutate(
            season = case_when(
                month(date) %in% c(12, 1, 2) ~ "Winter",
                month(date) %in% c(3, 4, 5) ~ "Spring",
                month(date) %in% c(6, 7, 8) ~ "Summer",
                month(date) %in% c(9, 10, 11) ~ "Autumn"
            )
        ) %>%
        ggplot(aes(x = HAUSHALT_TOT, y = PUMPE_TOT, color = season)) +
        geom_point(na.rm = TRUE) +
        geom_smooth(se = FALSE, method = lm) +
        labs(
            x = "household power consumption (W)",
            y = "heat pump power consumption (W)",
            color = "season"
        ) +
        ggtitle(
            "Comparison of Heat pump and household power consumption"
        )
}

heatpump_household_consumption_over_time <- function(household_dataset) {
    household_dataset %>%
        pivot_longer(
            cols = c("PUMPE_TOT", "HAUSHALT_TOT"),
            names_to = "type",
            values_to = "consumption"
        ) %>%
        group_by(
            date = date(index),
            type
        ) %>%
        summarise(
            consumption = mean(consumption, na.rm = TRUE)
        ) %>%
        ggplot(aes(x = date, y = consumption, color = type)) +
        geom_line() +
        scale_color_discrete(
            name = "power consumer",
            labels = c("household", "heat pump")
        ) +
        labs(
            x = "date",
            y = "power consumption (W)"
        ) +
        ggtitle(
            paste(
                "Heat pump and household power consumption over the year 2019 (",
                household,
                ")",
                sep = ""
            )
        )
}

household_comparison <- function(household_datasets) {
    household_datasets %>%
        group_by(
            household_name,
            week = isoweek(index)
        ) %>%
        summarise(
            PUMPE_TOT = mean(PUMPE_TOT, na.rm = TRUE),
        ) %>%
        ggplot(
            aes(
                x = week,
                y = PUMPE_TOT,
                color = household_name
            )
        ) +
        geom_line() +
        labs(
            x = "week number of the year 2019",
            y = "heat pump power consumption (W)",
            color = "household name"
        ) +
        ggtitle(
            "Comparison of Heat pumps power consumption in 2019"
        )
}
