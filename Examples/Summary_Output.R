library(TeenGrowth)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)


demo <- TeenGrowth::demo
demo$weight[9:12] <- demo$weight[9:12] + 5
demo$weight[13] <- demo$weight[13] + 3


demo_2 <- demo |> 
  filter(participant == 2)

clean_data_2 = clean_data(demo_2,
                        id_col_name = 'participant', 
                        age_col_name = 'age',
                        sex_col_name = 'sex',
                        ht_col_name = 'height',
                        wt_col_name = 'weight',
                        adult_ht_col_name = 'adult_height_in', 
                        ed_aoo_col_name = 'ed_aoo', 
                        ht_unit = 'in', 
                        wt_unit = 'lb')

BMI_forecast_2 <- forecast_bmi(
  data = clean_data_2, 
  central_value = "mean",
  ci = 95)

forecast_data_2 <- clean_forecast_data(BMI_forecast_2, 
                                       px = 2, 
                                       model = 'mean')
age_adult_ht <- clean_data_2$agemos_adult_ht[1]/12
ed_aoo <- clean_data_2$agemos_ed_onset[1]
adult_ht_in <- clean_data_2$adult_height_in[1]
adult_ht_cm <- adult_ht_in * 2.54
current_age <- max(clean_data_2$agemos)/12

most_recent_data <- clean_data_2 |> 
  filter(agemos == current_age*12)

current_height_in <- most_recent_data$height_in[1]
current_height_cm <- most_recent_data$height_cm[1]
current_weight_lb <- most_recent_data$weight_lb[1]
current_weight_kg <- most_recent_data$weight_kg[1]
current_bmi <- most_recent_data$bmi[1]
current_bmi_pct <- pnorm(most_recent_data$bmiz[1])*100
eBMI_pct <- pnorm(mean(BMI_forecast_2$eBMIz, na.rm = TRUE))*100
upper_eBMI_pct <- pnorm(mean(BMI_forecast_2$upper_eBMIz, na.rm = TRUE))*100
lower_eBMI_pct <- pnorm(mean(BMI_forecast_2$lower_eBMIz, na.rm = TRUE))*100


one_year_future <- (most_recent_data$agemos[1] + 12)
eWeight_one_year <- BMI_forecast_2 %>%
  filter(agemos == one_year_future) %>%
  select(`eBMI`, `lower_eBMI`, `upper_eBMI`, `eWeight`, `lower_eWeight`, `upper_eWeight`) %>%
  mutate(eWeight_kgs = eWeight * 0.453592,
         lower_eWeight_kgs = lower_eWeight * 0.453592,
         upper_eWeight_kgs = upper_eWeight * 0.453592) %>%
  mutate_all(round, 1) %>%
  mutate(`eBMI` = paste0(eBMI, ' (', lower_eBMI, ', ', upper_eBMI, ')')) %>%
  mutate(`eWt_lbs` = paste0(eWeight, ' (', lower_eWeight, ', ', upper_eWeight, ')')) %>%
  mutate(`eWt_kgs` = paste0(eWeight_kgs, ' (', lower_eWeight_kgs, ', ', upper_eWeight_kgs, ')')) %>%
  slice(1)

expected_bmi_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eBMI[1], "Not Provided")
expected_weight_lbs_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eWt_lbs[1], "Not Provided")
expected_weight_kgs_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eWt_kgs[1], "Not Provided")

five_year_future <- (most_recent_data$agemos[1] + 60)
eWeight_five_year <- BMI_forecast_2 %>%
  filter(agemos == five_year_future) %>%
  select(`eBMI`, `lower_eBMI`, `upper_eBMI`, `eWeight`, `lower_eWeight`, `upper_eWeight`) %>%
  mutate(eWeight_kgs = eWeight * 0.453592,
         lower_eWeight_kgs = lower_eWeight * 0.453592,
         upper_eWeight_kgs = upper_eWeight * 0.453592) %>%
  mutate_all(round, 1) %>%
  mutate(`eBMI` = paste0(eBMI, ' (', lower_eBMI, ', ', upper_eBMI, ')')) %>%
  mutate(`eWt_lbs` = paste0(eWeight, ' (', lower_eWeight, ', ', upper_eWeight, ')')) %>%
  mutate(`eWt_kgs` = paste0(eWeight_kgs, ' (', lower_eWeight_kgs, ', ', upper_eWeight_kgs, ')')) %>%
  slice(1)

expected_bmi_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eBMI[1], "Not Provided")
expected_weight_lbs_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eWt_lbs[1], "Not Provided")
expected_weight_kgs_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eWt_kgs[1], "Not Provided")

library(flextable)

# Create a data frame for the summary
summary_df <- data.frame(
  Attribute = c("Growth Chart Reference Sex", "Central BMIz Point", "Prediction Interval",
                "Eating Disorder Age of Onset", "Adult Height", "Most Recent Age with Available Data",
                "Most Recent Height", "Most Recent Weight", "Most Recent BMI", 
                "Most Recent BMI Percentile", "Expected BMI Percentile", 
                "Expected BMI (+1 Year)", "Expected Weight (+1 Year)",
                "Expected BMI (+5 Years)", "Expected Weight (+5 Years)"),
  Value = c(
    ifelse(demo_2$sex[1] == "F", 'Female', "Male"),
    "Mean",
    "95%",
    ifelse(!is.na(ed_aoo), sprintf("%.1f years", ed_aoo / 12), "Not Provided"),
    ifelse(!is.na(adult_ht_in), paste0(round(adult_ht_in, 0), " in / ", round(adult_ht_cm, 0), " cm"), "Not Provided"),
    sprintf("%.1f years", current_age),
    ifelse(!is.na(current_height_in), paste0(round(current_height_in, 1), " in / ", round(current_height_cm, 1), " cm"), "Not Provided"),
    ifelse(!is.na(current_weight_lb), paste0(round(current_weight_lb, 1), " lbs / ", round(current_weight_kg, 1), " kg"), "Not Provided"),
    ifelse(!is.na(current_bmi), round(current_bmi, 1), "Not Provided"),
    ifelse(!is.na(current_bmi_pct), round(current_bmi_pct, 1), "Not Provided"),
    paste0(round(eBMI_pct, 1), "% (", round(lower_eBMI_pct, 1), "%, ", round(upper_eBMI_pct, 1), "%)"),
    expected_bmi_one_year,
    ifelse(expected_weight_lbs_one_year != "NA (NA, NA)", paste0(expected_weight_lbs_one_year, " lbs / ", expected_weight_kgs_one_year, " kg"), "Not Provided"),
    expected_bmi_five_year,
    ifelse(expected_weight_lbs_five_year != "NA (NA, NA)", paste0(expected_weight_lbs_five_year, " lbs / ", expected_weight_kgs_five_year, " kg"), "Not Provided")
  )
)

# Create the flextable
summary_table <- flextable(summary_df)

# Style the table (optional)
summary_table <- theme_vanilla(summary_table)
summary_table <- autofit(summary_table)

# Print the flextable in the document
summary_table

# Save the flextable
save_as_image(summary_table, path = "Figs/summary_table_Teen2.png")

demo_3 <- demo |> 
  filter(participant == 3)

clean_data_3 = clean_data(demo_3,
                          id_col_name = 'participant', 
                          age_col_name = 'age',
                          sex_col_name = 'sex',
                          ht_col_name = 'height',
                          wt_col_name = 'weight',
                          adult_ht_col_name = 'adult_height_in', 
                          ed_aoo_col_name = 'ed_aoo', 
                          ht_unit = 'in', 
                          wt_unit = 'lb')

BMI_forecast_3 <- forecast_bmi(
  data = clean_data_3, 
  central_value = "mean",
  ci = 95)

forecast_data_3 <- clean_forecast_data(BMI_forecast_3, 
                                       px = 2, 
                                       model = 'mean')
age_adult_ht <- clean_data_3$agemos_adult_ht[1]/12
ed_aoo <- clean_data_3$agemos_ed_onset[1]
adult_ht_in <- clean_data_3$adult_height_in[1]
adult_ht_cm <- adult_ht_in * 2.54
current_age <- max(clean_data_3$agemos)/12

most_recent_data <- clean_data_3 |> 
  filter(agemos == current_age*12)

current_height_in <- most_recent_data$height_in[1]
current_height_cm <- most_recent_data$height_cm[1]
current_weight_lb <- most_recent_data$weight_lb[1]
current_weight_kg <- most_recent_data$weight_kg[1]
current_bmi <- most_recent_data$bmi[1]
current_bmi_pct <- pnorm(most_recent_data$bmiz[1])*100
eBMI_pct <- pnorm(mean(BMI_forecast_3$eBMIz, na.rm = TRUE))*100
upper_eBMI_pct <- pnorm(mean(BMI_forecast_3$upper_eBMIz, na.rm = TRUE))*100
lower_eBMI_pct <- pnorm(mean(BMI_forecast_3$lower_eBMIz, na.rm = TRUE))*100


one_year_future <- (most_recent_data$agemos[1] + 12)
eWeight_one_year <- BMI_forecast_3 %>%
  filter(agemos == one_year_future) %>%
  select(`eBMI`, `lower_eBMI`, `upper_eBMI`, `eWeight`, `lower_eWeight`, `upper_eWeight`) %>%
  mutate(eWeight_kgs = eWeight * 0.453592,
         lower_eWeight_kgs = lower_eWeight * 0.453592,
         upper_eWeight_kgs = upper_eWeight * 0.453592) %>%
  mutate_all(round, 1) %>%
  mutate(`eBMI` = paste0(eBMI, ' (', lower_eBMI, ', ', upper_eBMI, ')')) %>%
  mutate(`eWt_lbs` = paste0(eWeight, ' (', lower_eWeight, ', ', upper_eWeight, ')')) %>%
  mutate(`eWt_kgs` = paste0(eWeight_kgs, ' (', lower_eWeight_kgs, ', ', upper_eWeight_kgs, ')')) %>%
  slice(1)

expected_bmi_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eBMI[1], "Not Provided")
expected_weight_lbs_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eWt_lbs[1], "Not Provided")
expected_weight_kgs_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eWt_kgs[1], "Not Provided")

five_year_future <- (most_recent_data$agemos[1] + 60)
eWeight_five_year <- BMI_forecast_3 %>%
  filter(agemos == five_year_future) %>%
  select(`eBMI`, `lower_eBMI`, `upper_eBMI`, `eWeight`, `lower_eWeight`, `upper_eWeight`) %>%
  mutate(eWeight_kgs = eWeight * 0.453592,
         lower_eWeight_kgs = lower_eWeight * 0.453592,
         upper_eWeight_kgs = upper_eWeight * 0.453592) %>%
  mutate_all(round, 1) %>%
  mutate(`eBMI` = paste0(eBMI, ' (', lower_eBMI, ', ', upper_eBMI, ')')) %>%
  mutate(`eWt_lbs` = paste0(eWeight, ' (', lower_eWeight, ', ', upper_eWeight, ')')) %>%
  mutate(`eWt_kgs` = paste0(eWeight_kgs, ' (', lower_eWeight_kgs, ', ', upper_eWeight_kgs, ')')) %>%
  slice(1)

expected_bmi_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eBMI[1], "Not Provided")
expected_weight_lbs_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eWt_lbs[1], "Not Provided")
expected_weight_kgs_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eWt_kgs[1], "Not Provided")

library(htmltools)

# Create a data frame for the summary
summary_df <- data.frame(
  Attribute = c("Growth Chart Reference Sex", "Central BMIz Point", "Prediction Interval",
                "Eating Disorder Age of Onset", "Adult Height", "Most Recent Age with Available Data",
                "Most Recent Height", "Most Recent Weight", "Most Recent BMI", 
                "Most Recent BMI Percentile", "Expected BMI Percentile", 
                "Expected BMI (+1 Year)", "Expected Weight (+1 Year)",
                "Expected BMI (+5 Years)", "Expected Weight (+5 Years)"),
  Value = c(
    ifelse(demo_3$sex[1] == "F", 'Female', "Male"),
    "Mean",
    "95%",
    ifelse(!is.na(ed_aoo), sprintf("%.1f years", ed_aoo / 12), "Not Provided"),
    ifelse(!is.na(adult_ht_in), paste0(round(adult_ht_in, 0), " in / ", round(adult_ht_cm, 0), " cm"), "Not Provided"),
    sprintf("%.1f years", current_age),
    ifelse(!is.na(current_height_in), paste0(round(current_height_in, 1), " in / ", round(current_height_cm, 1), " cm"), "Not Provided"),
    ifelse(!is.na(current_weight_lb), paste0(round(current_weight_lb, 1), " lbs / ", round(current_weight_kg, 1), " kg"), "Not Provided"),
    ifelse(!is.na(current_bmi), round(current_bmi, 1), "Not Provided"),
    ifelse(!is.na(current_bmi_pct), round(current_bmi_pct, 1), "Not Provided"),
    paste0(round(eBMI_pct, 1), "% (", round(lower_eBMI_pct, 1), "%, ", round(upper_eBMI_pct, 1), "%)"),
    expected_bmi_one_year,
    ifelse(expected_weight_lbs_one_year != "NA (NA, NA)", paste0(expected_weight_lbs_one_year, " lbs / ", expected_weight_kgs_one_year, " kg"), "Not Provided"),
    expected_bmi_five_year,
    ifelse(expected_weight_lbs_five_year != "NA (NA, NA)", paste0(expected_weight_lbs_five_year, " lbs / ", expected_weight_kgs_five_year, " kg"), "Not Provided")
  )
)


# Create the flextable
summary_table <- flextable(summary_df)

# Style the table (optional)
summary_table <- theme_vanilla(summary_table)
summary_table <- autofit(summary_table)

# Print the flextable in the document
summary_table

# Save the flextable
save_as_image(summary_table, path = "Figs/summary_table_Teen1.png")
