# Load the processed data
load("Data/TeenGrowth_SimData_List_Split_Processed.RData")

library(dplyr)

# Initialize an empty dataframe to store results
results <- data.frame()

# Loop through each simulation
for (sim_index in seq_along(TeenGrowth_SimData_List_Split)) {
  sim_data <- TeenGrowth_SimData_List_Split[[sim_index]]
  forecasts <- sim_data$forecasts
  training <- sim_data$training_data
  test <- sim_data$testing_data
  
  # Loop through each forecast within the current simulation
  for (forecast_name in names(forecasts)) {
    forecast_data <- forecasts[[forecast_name]]
    
    # Join forecast_data and test by id
    forecast_merge <- right_join(forecast_data, test, by = c("id", "agemos", "sex")) |> 
      filter(id <= 1000)
    
    # Check the percentage of IDs with missing eBMIz forecasts
    missing <- forecast_merge %>%
      # Make missing a binary variable 
      mutate(missing = ifelse(is.na(eBMIz), 1, 0)) %>%
      # Calculate the percentage of missing values
      summarise(missing = mean(missing) * 100)
    
    # Filter nonmissing 
    forecast_merge_nomiss <- forecast_merge %>%
      filter(!is.na(eBMIz)) %>%
      # Calculate how far eBMIz is from BMIz
      mutate(eBMIz_diff = abs(eBMIz - bmiz)) %>%
      # Calculate BMI diff
      mutate(eBMI_diff = abs(eBMI - bmi)) %>%
      # Make a binary variable as to whether the bmiz is between lower_eBMIz and upper_eBMIz
      mutate(bmiz_in_ci = ifelse(bmiz >= lower_eBMIz & bmiz <= upper_eBMIz, 1, 0)) |> 
      mutate(bmiz_above_lower = ifelse(bmiz >= lower_eBMIz, 1, 0))
    
    # Calculate the percentage of forecasts that are within the CI along with average eBMIz_diff and eBMI_diff
    no_miss <- forecast_merge_nomiss %>%
      summarise(
        bmiz_above_lower = mean(bmiz_above_lower, na.rm = TRUE) * 100,
        bmiz_within_ci = mean(bmiz_in_ci, na.rm = TRUE) * 100,
        mean_eBMIz_diff = mean(eBMIz_diff, na.rm = TRUE),
        median_eBMIz_diff = median(eBMIz_diff, na.rm = TRUE),
        mean_eBMI_diff = mean(eBMI_diff, na.rm = TRUE),
        median_eBMI_diff = median(eBMI_diff, na.rm = TRUE)
      )
    
    # Combine forecast_merge_nomiss and missing
    data <- merge(missing, no_miss)
    # Add the name of the simulation
    data$simulation <- training$simulation_type[1]
    # Add the name of the forecast
    data$forecast <- forecast_name
    
    # Append the result to the results dataframe
    results <- rbind(results, data)
  }
}
library(tidyr)
library(stringr)


# Split the simulation colum into avg datapoints, rho, and sd
results <- results %>%
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
  separate(forecast, c("forecast_point", "ci"), sep = "_ci_", remove = FALSE) |> 
  # if sd is NA , make it == 1
  mutate(sd = ifelse(is.na(sd), 1, sd) )
  
  
# Save Results
save(results, file = "Data/TeenGrowth_Forecast_Results.RData")
