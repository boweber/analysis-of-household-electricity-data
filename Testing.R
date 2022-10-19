source("ImportData.R")
source("ModifyData.R")

dataset_3 <- import_data(file_numbers = c(3, 4))
dataset_3 <- insert_missing_data(dataset_3, by_minutes = 15)
dataset_3 <- interpolate_missing_data(dataset_3)

# time shift on
# 31. march 2019
# 27. october 2019

time_shift_indices <- which(dataset_3$index >= as.POSIXct("2019-03-31 00:00:00") & dataset_3$index <= as.POSIXct("2019-03-31 04:00:00"))
View(dataset_3[time_shift_indices, ])
View(dataset_3)
