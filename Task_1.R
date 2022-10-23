source("ImportData.R")

dataset <- import_data()

## MARK: - Mixed Model approach

mixed_model <- lme4::lmer(PUMPE_TOT ~ TEMPERATURE.TOTAL + (1 | household), data = dataset)
summary(mixed_model)

merTools::plotREsim(merTools::REsim(mixed_model))
