library(dplyr)

Fig1<-read.csv('Tabs/Fig_2_Results.csv')

fig1.2 <- Fig1 |> filter(sd == 0.2 & forecast_point != 'Max' & forecast_point != '50th Percentile BMI')

mean(fig1.2$Mean.eBMI.Error)
median(fig1.2$Median.eBMI.Error)

fig1.4 <- Fig1 |> filter(sd == 0.4 & forecast_point != 'Max' & forecast_point != '50th Percentile BMI')

mean(fig1.4$Mean.eBMI.Error)
median(fig1.4$Median.eBMI.Error)

fig1.Median.2 <- Fig1 |> filter(sd == 0.2 & forecast_point == '50th Percentile BMI')
mean(fig1.Median.2$Mean.eBMI.Error)
median(fig1.Median.2$Median.eBMI.Error)


fig1.Median.4 <- Fig1 |> filter(sd == 0.4 & forecast_point == '50th Percentile BMI')
mean(fig1.Median.4$Mean.eBMI.Error)
median(fig1.Median.4$Median.eBMI.Error)

Fig1.collapsed <- Fig1 |> filter(forecast_point != 'Max' & forecast_point != '50th Percentile BMI')
median(Fig1.collapsed$Median.eBMI.Error)

Fig1.collapsed50 <- Fig1 |> filter(forecast_point == '50th Percentile BMI')
median(Fig1.collapsed50$Median.eBMI.Error)




load('Tabs/ED_confusion_matrix.RData')

confusion_abstract <- confusion |> 
  filter (forecast_point != "Max") |> 
  mutate(forecast_point_1 = case_when (forecast_point == "Population Median" ~ "Population",
                                       forecast_point == "Mean" ~ "Individual",
                                       forecast_point == "Most Recent" ~ "Individual",
                                       forecast_point == "Mean + Most Recent" ~ "Individual")) 

confusion_abstract <- confusion_abstract |> 
  group_by(forecast_point_1) |>
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE))
  
# use group_by to compute the mean accuracy within combinations of n_datapoints, forecast_point, and rho
confusion_table <- confusion |> 
  group_by(sd, forecast_point) |>
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE))

# round accurary to 2 decimal places
confusion_table$mean_accuracy <- round(confusion_table$mean_accuracy, 2)

# rename variables to be more readable
confusion_table <- confusion_table |> 
  rename('Mean Accuracy' = mean_accuracy) |> 
  rename("Forecast Point" = forecast_point) |> 
  rename("Within-Child Standard Deviation" = sd)

confusion_table <- confusion_table %>%
  arrange(`Within-Child Standard Deviation`)

library(gt)

gt_table <- confusion_table %>%
  gt() %>%
  tab_header(
    title = "Table 1. Mean Accuracy by Forecast Point and Within-Child Standard Deviation"
  ) %>%
  fmt_number(
    columns = vars(`Mean Accuracy`),
    decimals = 2
  ) %>%
  cols_label(
    `Forecast Point` = "Forecast Point",
    `Mean Accuracy` = "Mean Accuracy"
  ) %>%
  opt_row_striping() %>%
  tab_row_group(
    label = "Within-Child SD = 0.4",
    rows = `Within-Child Standard Deviation` == 0.4
  ) %>%
  # Manually set row group labels
  tab_row_group(
    label = "Within-Child SD = 0.2",
    rows = `Within-Child Standard Deviation` == 0.2
  ) %>%

  # Reduce table width and padding
  tab_options(
    table.width = px(300),  # Smaller overall table width
    column_labels.padding.horizontal = px(5),  # Reduce header padding
    data_row.padding = px(2),  # Reduce data row padding
    table.font.size = 14,
    heading.title.font.size = px(12),  # Adjust title font size
    heading.subtitle.font.size = px(10), 
    heading.align = 'left'
  ) %>%
  # Set column widths
  cols_width(
    vars(`Forecast Point`, `Mean Accuracy`) ~ px(80)
  ) %>%
  # Align columns to center
  cols_align(
    align = "center",
    columns = vars(`Forecast Point`, `Mean Accuracy`)
  ) %>%
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  # Add bolding to headers
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Add bolding to group titles
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )
  

# Save as a PNG file
gtsave(gt_table, "Tabs/confusion_table.png")


# Save as a .png file
gtsave(gt_table, "Tabs/confusion_table.png")

