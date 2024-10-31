

load("Data/TeenGrowth_SimData_List_Split.RData")
library(TeenGrowth)
library(dplyr)
# within the testing data, look up the 50th centile for each age and gender and make that a column
vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("data_point", "age", "sex"))
pop_median <- data.frame()

for (sim_index in seq_along(TeenGrowth_SimData_List_Split)) {
  sim_data <- TeenGrowth_SimData_List_Split[[sim_index]]
  test <- sim_data$testing_data
  training <- sim_data$training_data

test <- test %>%
  filter(id <= 1000) 

test <- test |>
  mutate(centile_50 = vectorized_bmi_lookup(data_point = 0, 
                                              sex = test$sex, 
                                              age = test$agemos, 
                                              age_unit = 'months')) |> 
  mutate(eBMIz_diff = abs(0 - bmiz)) %>%
  mutate(eBMI_diff = abs(centile_50 - bmi)) %>%
  mutate(cutoff_85_centile_50 = centile_50*0.85) |> 
  mutate(bmiz_above_lower = ifelse(bmi >= cutoff_85_centile_50, 1, 0))

data <- test |> summarise(
  bmiz_above_lower = mean(bmiz_above_lower, na.rm = TRUE) * 100,
  mean_eBMIz_diff = mean(eBMIz_diff, na.rm = TRUE),
  median_eBMIz_diff = median(eBMIz_diff, na.rm = TRUE),
  mean_eBMI_diff = mean(eBMI_diff, na.rm = TRUE),
  median_eBMI_diff = median(eBMI_diff, na.rm = TRUE))

data$simulation <- training$simulation_type[1]
data$forecast <- "pop_median"

  pop_median <- rbind(pop_median, data)
}

library(stringr)

# Split the simulation colum into avg datapoints, rho, and sd
pop_median <- pop_median %>%
  mutate(
    n_datapoints = str_extract(simulation, "^n_datapoints_[^_]+"),
    rho = str_extract(simulation, "rho_\\d+\\.\\d+"),
    sd = str_extract(simulation, "sd_\\d+\\.\\d+")
  ) %>%
  mutate(
    n_datapoints = str_remove(n_datapoints, "n_datapoints_"),
    rho = str_remove(rho, "rho_"),
    sd = str_remove(sd, "sd_")
  ) |> 
  # if sd is NA , make it == 1
  mutate(sd = ifelse(is.na(sd), 1, sd) )
  
pop_median$missing <- 0
pop_median$bmiz_within_ci <- NA
pop_median$ci <- '85% of 50th'
pop_median$forecast_point <- 'pop_median'


# save pop_median data
save(pop_median, file = "Data/pop_median.RData")

load("Data/TeenGrowth_Forecast_Results.RData")
load("Data/pop_median.RData")

# make sure the columns are in the same order
pop_median <- pop_median[, colnames(results)]

# bind the data
results <- rbind(results, pop_median)

# save new results
save(results, file = "Data/TeenGrowth_Forecast_Results.RData")
