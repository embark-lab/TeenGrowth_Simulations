library(dplyr)
library(tidyr)

# Set seed for reproducibility
set.seed(674)

# Number of children
n_control <- 1000
n_ed <- 100

# Age range in months
age_range_pre14 <- 25:(12 * 14)  # Ages before 14 years
age_range_post14 <- (12 * 14 + 1):240  # Ages after 14 years

# Number of data points
n_datapoints_list <- c(3, 6, 9, 12, 15)

# Annual autocorrelation parameters
rho_annual_list <- c(0.4, 0.8)

# Convert annual autocorrelation to monthly autocorrelation
rho_monthly_list <- rho_annual_list^(1/12)

# Within-child standard deviations
within_child_sd_list <- c(0.2, 0.4, 0.6)


# Generate sex vector with half male for control group and 20% male for ED group
sex_control <- rep(c("M", "F"), times = c(n_control * 0.5, n_control * 0.5))
sex_ed <- rep(c("M", "F"), times = c(n_ed * 0.2, n_ed * 0.8))
sex_vector <- c(sex_control, sex_ed)

# Function to simulate data for one child with autocorrelation
simulate_child_data <- function(child_id, n_datapoints, rho, within_child_sd, sex, group, drop_after_14 = FALSE, drop_mean = 1, drop_sd = 0.5) {
  # Determine number of data points before and after age 14
  n_datapoints_pre14 <- ceiling(2/3 * n_datapoints)
  n_datapoints_post14 <- n_datapoints - n_datapoints_pre14
  
  # Ages for the data points (2/3 before age 14 and 1/3 after age 14)
  ages_pre14 <- sort(sample(age_range_pre14, n_datapoints_pre14, replace = TRUE))
  ages_post14 <- sort(sample(age_range_post14, n_datapoints_post14, replace = TRUE))
  ages <- c(ages_pre14, ages_post14)
  
  # Ensure we have the correct number of unique ages
  while(length(unique(ages)) < n_datapoints) {
    ages_pre14 <- sort(sample(age_range_pre14, n_datapoints_pre14, replace = TRUE))
    ages_post14 <- sort(sample(age_range_post14, n_datapoints_post14, replace = TRUE))
    ages <- c(ages_pre14, ages_post14)
  }
  
  # BMIz-scores (mean = 0, sd between children = 1)
  child_mean <- rnorm(1, mean = 0, sd = 1)
  
  # Initialize the BMIz-scores vector
  bmi_zscores <- numeric(n_datapoints)
  bmi_zscores[1] <- rnorm(1, mean = child_mean, sd = within_child_sd)
  
  # Generate BMIz-scores with autocorrelation considering time difference
  if(n_datapoints > 1) {
    for (i in 2:n_datapoints) {
      time_diff <- (ages[i] - ages[i - 1])  # Time difference in months
      rho_adjusted <- rho^time_diff
      bmi_zscores[i] <- rho_adjusted * bmi_zscores[i - 1] + rnorm(1, mean = 0, sd = sqrt(1 - rho_adjusted^2) * within_child_sd)
    }
  }
  
  # Apply drop in BMIz score after age 14 if specified
  if (drop_after_14) {
    for (i in seq_along(ages)) {
      if (ages[i] > 12 * 14) {
        bmi_zscores[i] <- bmi_zscores[i] - rnorm(1, mean = drop_mean, sd = drop_sd)
      }
    }
  }
  
  # Create a data frame for the child's data
  data.frame(
    child_id = child_id,
    agemos = ages,
    bmi_zscore = bmi_zscores,
    sex = sex,
    group = group
  )
}

# Function to run the simulation with different parameters
run_simulation <- function(n_datapoints, rho, within_child_sd, sex_vector, n_control, n_ed, drop_mean, drop_sd) {
  # Simulate data for control children
  data_control <- bind_rows(lapply(1:n_control, function(child_id) {
    simulate_child_data(child_id, n_datapoints, rho, within_child_sd, sex_vector[child_id], group = "Control")
  }))
  
  # Simulate data for children with eating disorders
  data_ed <- bind_rows(lapply((n_control + 1):(n_control + n_ed), function(child_id) {
    simulate_child_data(child_id, n_datapoints, rho, within_child_sd, sex_vector[child_id], group = "ED", drop_after_14 = TRUE, drop_mean = drop_mean, drop_sd = drop_sd)
  }))
  
  # Combine both datasets
  bind_rows(data_control, data_ed)
}

# Run simulations and store results in a list
simulation_results <- list()
for (n_datapoints in n_datapoints_list) {
  for (rho_monthly in rho_monthly_list) {
    for (within_child_sd in within_child_sd_list) {
      sim_data <- run_simulation(n_datapoints, rho_monthly, within_child_sd, sex_vector, n_control, n_ed, drop_mean = 1, drop_sd = 0.5)
      simulation_type <- paste0("n_datapoints_", n_datapoints, "_rho_", rho_monthly, "_sd_", within_child_sd)
      simulation_results[[simulation_type]] <- sim_data
    }
  }
}

TeenGrowth_SimData_List <- simulation_results

# Combine results into a single data frame
TeenGrowth_SimData <- bind_rows(
  lapply(names(simulation_results), function(sim_type) {
    sim_data <- simulation_results[[sim_type]]
    # Extract parameters from simulation type
    params <- unlist(strsplit(sim_type, "_"))
    n_datapoints <- as.numeric(params[3])
    rho <- as.numeric(params[5])
    within_child_sd <- as.numeric(params[7])
    sim_data <- sim_data %>%
      mutate(
        n_datapoints = n_datapoints,
        rho = rho,
        within_child_sd = within_child_sd,
        simulation_type = sim_type
      )
    sim_data
  })
)

# Save combined data to an RData file
save(TeenGrowth_SimData, file = "Data/TeenGrowth_SimData.RData")
save(TeenGrowth_SimData_List, file = "Data/TeenGrowth_SimData_List.RData")

# View the first few rows of the combined dataset
head(TeenGrowth_SimData)

# Clean and split data
library(TeenGrowth)

# Load the data
load("Data/TeenGrowth_SimData_List.RData")

# Clean data
TeenGrowth_SimData_List_Clean <- list()
for (sim_name in names(TeenGrowth_SimData_List)) {
  cleaned_data <- clean_data(TeenGrowth_SimData_List[[sim_name]], 
                             id_col = "child_id",
                             age_col_name = "agemos", 
                             bmiz_col_name = "bmi_zscore",
                             age_unit = "months", 
                             sex_col_name = "sex")
  # Ensure all column names are valid
  colnames(cleaned_data) <- make.names(colnames(cleaned_data), unique = TRUE)
  # Add simulation type
  cleaned_data$simulation_type <- sim_name
  TeenGrowth_SimData_List_Clean[[sim_name]] <- cleaned_data
}

# Function to split data into training (before age 14) and testing (after age 14) sets
split_data <- function(data, split_age) {
  training_data <- data %>% filter(agemos <= split_age * 12)
  testing_data <- data %>% filter(agemos > split_age * 12)
  list(training_data = training_data, testing_data = testing_data)
}

# Split data
TeenGrowth_SimData_List_Split <- list()
for (sim_name in names(TeenGrowth_SimData_List_Clean)) {
  split_result <- split_data(TeenGrowth_SimData_List_Clean[[sim_name]], split_age = 14)
  TeenGrowth_SimData_List_Split[[sim_name]] <- split_result
}

# Save combined data to an RData file
save(TeenGrowth_SimData_List_Clean, file = "Data/TeenGrowth_SimData_List_Clean.RData")
save(TeenGrowth_SimData_List_Split, file = "Data/TeenGrowth_SimData_List_Split.RData")
