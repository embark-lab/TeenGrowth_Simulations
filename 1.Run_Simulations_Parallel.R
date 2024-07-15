# Load necessary packages
install.packages("TeenGrowth", repos = NULL, type = "source")
library(TeenGrowth)
library(dplyr)
library(parallel)

# Load the split data
load("Data/TeenGrowth_SimData_List_Split.RData")

# Define central values and conditions
central_values <- c("mean", "max", "most_recent", "mean+most_recent")
conditions <- list(
  list(ci = 'User-Defined', upper_margin = 0.5, lower_margin = 0.5),
  list(ci = 95, upper_margin = NULL, lower_margin = NULL),
  list(ci = 99, upper_margin = NULL, lower_margin = NULL)
)

# Function to run forecasts with retry logic and enhanced error logging
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
      message("Central value: ", central_value)
      message("Condition: ", toString(condition))
      message("Error details: ", capture.output(str(e)))
      message("Training data summary: ", capture.output(summary(training_data)))
      attempt <- attempt + 1
    })
  }
  return(forecast)
}

# Set up the number of cores to use
total_memory_gb <- 100 # Adjusted total memory request
memory_per_core_gb <- 5 # Estimated memory usage per core
num_cores <- min(detectCores() - 1, total_memory_gb / memory_per_core_gb)

# Function to process a single split
process_split <- function(split_data, split_index) {
  training_data <- split_data$training_data
  forecasts <- list()
  
  for (central_value in central_values) {
    for (condition in conditions) {
      condition_desc <- if (!is.null(condition$ci)) {
        paste0("ci_", condition$ci)
      } else {
        "margin_0.5"
      }
      
      forecast <- run_forecasts(training_data, central_value, condition)
      
      if (!is.null(forecast)) {
        forecast <- forecast %>% filter(agemos > 60)
        condition_key <- if (!is.null(condition$ci)) {
          paste0(central_value, "_ci_", condition$ci)
        } else {
          paste0(central_value, "_margin_0.5")
        }
        forecasts[[condition_key]] <- forecast
      }
    }
  }
  
  split_data$forecasts <- forecasts
  return(split_data)
}

# Run the processing in parallel
processed_splits <- mclapply(seq_along(TeenGrowth_SimData_List_Split), function(i) {
  message("Processing split ", i, " of ", length(TeenGrowth_SimData_List_Split))
  process_split(TeenGrowth_SimData_List_Split[[i]], i)
}, mc.cores = num_cores)

# Replace the original list with the processed splits
TeenGrowth_SimData_List_Split <- processed_splits

# Save the processed data
save(TeenGrowth_SimData_List_Split, file = "Data/TeenGrowth_SimData_List_Split_Processed.RData")
message("All splits processed and data saved.")
