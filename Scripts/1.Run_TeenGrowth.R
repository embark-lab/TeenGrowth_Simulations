install.packages("TeenGrowth", repos = NULL, type = "source")

library(TeenGrowth)
library(dplyr)

# Load the split data
load("Data/TeenGrowth_SimData_List_Split.RData")

# pick only the first one for testing
TeenGrowth_SimData_List_Split <- TeenGrowth_SimData_List_Split[1]
# only pick the first 50 rows for testing in both the training and test lists

# Define central values and conditions
central_values <- c("mean", "mean+most_recent")
conditions <- list(
  list(ci = 'User-Defined', upper_margin = 0.5, lower_margin = 0.5),
  list(ci = 95, upper_margin = NULL, lower_margin = NULL)
)

# Function to run forecasts with retry logic
run_forecasts <- function(training_data, central_value, condition) {
  attempt <- 1
  max_attempts <- 3
  success <- FALSE
  forecast <- NULL
  
  while (!success && attempt <= max_attempts) {
    tryCatch({
      forecast <- if (!is.null(condition$ci)) {
        forecast_bmi(training_data, 
                     central_value = central_value, 
                     ci = condition$ci)
      } else {
        forecast_bmi(training_data, 
                     central_value = central_value, 
                     upper_margin = condition$upper_margin, 
                     lower_margin = condition$lower_margin)
      }
      success <- TRUE
    }, error = function(e) {
      message("Attempt ", attempt, ": ", e$message)
      attempt <- attempt + 1
    })
  }
  return(forecast)
}

# Iterate over each split in the list
for (i in seq_along(TeenGrowth_SimData_List_Split)) {
  message("Processing split ", i, " of ", length(TeenGrowth_SimData_List_Split))
  
  training_data <- TeenGrowth_SimData_List_Split[[i]]$training_data
  message("  Loaded training data for split ", i)
  
  # Initialize a list to store forecasts for different combinations
  forecasts <- list()
  
  # Iterate over each central value
  for (central_value in central_values) {
    message("  Processing central value: ", central_value)
    
    # Iterate over each condition
    for (condition in conditions) {
      condition_desc <- if (!is.null(condition$ci)) {
        paste0("ci_", condition$ci)
      } else {
        "margin_0.5"
      }
      message("    Processing condition: ", condition_desc)
      
      # Run forecasts with retry logic
      forecast <- run_forecasts(training_data, central_value, condition)
      
      if (!is.null(forecast)) {
        message("    Forecast successful for ", central_value, " with ", condition_desc)
        
        # Remove the forecast data with agemos <= 60
        forecast <- forecast %>% filter(agemos > 60)
        
        # Create a unique key for the forecast
        condition_key <- if (!is.null(condition$ci)) {
          paste0(central_value, "_ci_", condition$ci)
        } else {
          paste0(central_value, "_margin_0.5")
        }
        
        # Save the forecast in the list with the unique key
        forecasts[[condition_key]] <- forecast
        message("    Forecast saved with key: ", condition_key)
      } else {
        message("    Forecast failed for ", central_value, " with ", condition_desc)
      }
    }
  }
  
  # Save all forecasts for this split in the original list
  TeenGrowth_SimData_List_Split[[i]]$forecasts <- forecasts
  message("  Completed processing split ", i)
}

# Save the processed data
 save(TeenGrowth_SimData_List_Split, file = "Data/TeenGrowth_SimData_List_Split_Processed.RData")
 message("All splits processed and data saved.")
