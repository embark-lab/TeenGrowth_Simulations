load("Data/TeenGrowth_SimData_List_Split_Processed.RData")
library(dplyr)
# Initialize an empty dataframe to store results
ED_results <- data.frame()

# Loop through each simulation
for (sim_index in seq_along(TeenGrowth_SimData_List_Split)) {
  sim_data <- TeenGrowth_SimData_List_Split[[sim_index]]
  forecasts <- sim_data$forecasts
  training <- sim_data$training_data
  test <- sim_data$testing_data
  
  # Loop through each forecast within the current simulation
  for (forecast_name in names(forecasts)) {
    forecast_data <- forecasts[[forecast_name]]
    forecast_data$forecast <- forecast_name
    
    # Join forecast_data and test by id
    forecast_merge <- right_join(forecast_data, test, by = c("id", "agemos", "sex"))
    ED_results <- rbind(ED_results, forecast_merge) 
    
  }
  }

ED_results <- ED_results %>%
  filter(!is.na(eBMIz)) |> 
  mutate(bmiz_above_lower = ifelse(bmiz >= lower_eBMIz, 1, 0)) |> 
  select(id, forecast, simulation_type, bmiz_above_lower)

library(tidyr)
library(stringr)
# Split the simulation colum into avg datapoints, rho, and sd
ED_results <- ED_results %>%
  mutate(
    n_datapoints = str_extract(simulation_type, "^n_datapoints_[^_]+"),
    rho = str_extract(simulation_type, "rho_\\d+\\.\\d+"),
    sd = str_extract(simulation_type, "sd_\\d+\\.\\d+")
  ) %>%
  mutate(
    n_datapoints = str_remove(n_datapoints, "n_datapoints_"),
    rho = str_remove(rho, "rho_"),
    sd = str_remove(sd, "sd_")
  ) |> 
  separate(forecast, c("forecast_point", "ci"), sep = "_ci_", remove = FALSE) |> 
  # make group column with 'Control' for ID numbers <=1000 and 'ED' for those > 1000
  mutate(group = ifelse(id <= 1000, "Control", "ED")) |> 
  # remove original forecast and simulation_type columns
  select(-forecast, -simulation_type)

# recode rho 
ED_results <- ED_results |> 
  mutate(rho = case_when (
    rho == '0.926484872479069' ~ '0.4',
    rho == '0.981576529873752' ~ '0.8'
  ))


library(caret)
library(dplyr)

# Extract unique values for combinations
rho_values <- unique(ED_results$rho)
sd_values <- unique(ED_results$sd)
n_datapoints_values <- unique(ED_results$n_datapoints)
ci_values <- unique(ED_results$ci)
forecast_point_values <- unique(ED_results$forecast_point)

# Create a data frame of all combinations
combinations <- expand.grid(
  rho = rho_values,
  sd = sd_values,
  n_datapoints = n_datapoints_values,
  ci = ci_values,
  forecast_point = forecast_point_values
)

# Initialize list to store confusion matrices and vectors for performance metrics
confusion_matrices_list <- list()
sensitivity_values <- numeric(nrow(combinations))
specificity_values <- numeric(nrow(combinations))
accuracy_values <- numeric(nrow(combinations))
ppv_values <- numeric(nrow(combinations))
npv_values <- numeric(nrow(combinations))

# Iterate over all combinations
for (i in 1:nrow(combinations)) {
  # Extract parameter values for the current combination
  params <- combinations[i, ]
  
  # Filter your test data based on the current combination of parameters
  test <- ED_results %>%
    filter(rho == params$rho,
           sd == params$sd,
           n_datapoints == params$n_datapoints,
           ci == params$ci,
           forecast_point == params$forecast_point) %>%
    mutate(group = recode(group, 'Control' = 1, 'ED' = 0))
  
  # Compute the confusion matrix
  confusion_matrix <- caret::confusionMatrix(
    as.factor(test$bmiz_above_lower),
    as.factor(test$group),
    positive = '0'
  )
  
  # Store the confusion matrix in the list
  confusion_matrices_list[[i]] <- confusion_matrix
  
  # Extract and store performance metrics
  sensitivity_values[i] <- confusion_matrix$byClass[1]
  specificity_values[i] <- confusion_matrix$byClass[2]
  ppv_values[i] <- confusion_matrix$byClass[3]
  npv_values[i] <- confusion_matrix$byClass[4]
  accuracy_values[i] <- confusion_matrix$overall[1]
}

# Combine results into a data frame
confusion <- data.frame(
  rho = combinations$rho,
  sd = combinations$sd,
  n_datapoints = combinations$n_datapoints,
  ci = combinations$ci,
  forecast_point = combinations$forecast_point,
  sensitivity = sensitivity_values,
  specificity = specificity_values,
  ppv = ppv_values,
  npv = npv_values,
  accuracy = accuracy_values
)

print(confusion)

# Ensure n_datapoints is numeric
confusion$n_datapoints <- as.numeric(as.character(confusion$n_datapoints))

# Ensure ci, sd, rho, and forecast_point are factors
confusion$ci <- as.factor(confusion$ci)
confusion$sd <- as.factor(confusion$sd)
confusion$rho <- as.factor(confusion$rho)
confusion$forecast_point <- as.factor(confusion$forecast_point)

# recode ci 
confusion <- confusion |> 
  mutate(prediction_interval = case_when (
    ci == '95' ~ '95', 
    ci == '99' ~ '99', 
    ci == 'User-Defined' ~ '+/- 0.5 BMIz'
  ))

# drop ci
confusion <- confusion %>%
  select(-ci)


# recode Forecast Point
confusion <- confusion |> 
  mutate(forecast_point = case_when (
    forecast_point == 'max' ~ 'Max',
    forecast_point == 'mean' ~ 'Mean',
    forecast_point == 'mean+most_recent' ~ 'Mean + Most Recent',
    forecast_point == 'most_recent' ~ 'Most Recent'
  ))

# Save confusion matrix dataframe
save(confusion, file = "Data/ED_confusion_matrix.RData")

