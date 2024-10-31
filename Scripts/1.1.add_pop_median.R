install.packages("TeenGrowth", repos = NULL, type = "source")

library(TeenGrowth)
library(dplyr)

vectorized_bmiz_lookup <- Vectorize(bmiz_lookup, vectorize.args = c("bmi", "age", "sex"))
vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("data_point", "age", "sex"))

load("Data/TeenGrowth_SimData_List_Split_Processed.RData")

# make forecast with population median data and add it to the sim data list
for (sim_index in seq_along(TeenGrowth_SimData_List_Split)) {
  forecast_data <-TeenGrowth_SimData_List_Split[[sim_index]]$forecasts
  
  # copy the first forecast and then add the population median data to it
  forecast_data$pop_median <- forecast_data[[1]]
  # name it population_median
  forecast_data$pop_median$`.model` <- "pop_median"
  forecast_data$pop_median <- forecast_data$pop_median |> 
    mutate(eBMIz = 0, 
           eBMI = median_bmi, 
           lower_eBMI = median_bmi * 0.85, 
           lower_eBMIz = vectorized_bmiz_lookup(bmi = lower_eBMI,
                                                age = forecast_data$pop_median$agemos,
                                                sex = forecast_data$pop_median$sex,
                                                age_unit = 'months'),
           upper_eBMIz = 1, 
           upper_eBMI = vectorized_bmi_lookup(data_point = upper_eBMIz,
                                              age = forecast_data$pop_median$agemos,
                                              sex = forecast_data$pop_median$sex,
                                              age_unit = 'months'))
  # add the population median forecast to the list
  TeenGrowth_SimData_List_Split[[sim_index]]$forecasts$pop_median <- forecast_data$pop_median
}


save(TeenGrowth_SimData_List_Split, file = "Data/TeenGrowth_SimData_List_Split_Processed.RData")
