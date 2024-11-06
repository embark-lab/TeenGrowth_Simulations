# load the results
load("Data/TeenGrowth_Forecast_Results.RData")

library(dplyr)
library(patchwork)
library(ggplot2)
library(grid)
# Make ggplot of results

graph_1 <- results |> 
  # within avg datapoints and forecast point, calculate the mean eBMIz_diff and sd
  group_by(n_datapoints, forecast_point, sd, rho) |>
  summarise(median_eBMI_diff = mean(median_eBMI_diff, na.rm = TRUE),
            mean_eBMI_diff = mean(mean_eBMI_diff, na.rm = TRUE), 
            ) |> 
  filter(sd != '1') |> 
  # recode the forecast points to be more readable
  mutate(forecast_point = case_when(
    forecast_point == 'max' ~ 'Max',
    forecast_point == 'mean+most_recent' ~ 'Mean + Most Recent',
    forecast_point == 'mean' ~ 'Mean',
    forecast_point == 'most_recent' ~ 'Most Recent', 
    forecast_point == 'pop_median' ~ 'Population Median')) |> 
  mutate(rho = case_when (
    rho == '0.970714469623222' ~ '0.7',
    rho == '0.991258389045303' ~ '0.9'
  ), 
  # recode n_datapoints to be 2,4,6,8,10 from 3,6,9,12,15
  n_datapoints = case_when(
    n_datapoints == '3' ~ '2',
    n_datapoints == '6' ~ '4',
    n_datapoints == '9' ~ '6',
    n_datapoints == '12' ~ '8',
    n_datapoints == '15' ~ '10'
  )
  )
# Define a lighter strip background color
strip_fill <- "#1A4F66"
strip_fill_light <- adjustcolor(strip_fill, alpha.f = 0.7)  # Lighten the strip background using alpha

Fig_1_Results <- graph_1 |>
 # sort ascending by eBMI_diff
  arrange(median_eBMI_diff) |> 
  # round median_eBMI_diff, mean_eBMIz_diff, and mean_eBMI_diff to 2 decimal places using sprintf
  mutate(median_eBMI_diff = sprintf("%.2f", median_eBMI_diff),
         mean_eBMI_diff = sprintf("%.2f", mean_eBMI_diff)) |>
  # rename median_eBMI_diff to 'Median eBMI Error', mean_eBMIz_diff to 'Mean eBMIz Error', and mean_eBMI_diff to 'Mean eBMI Error'
  rename('Median eBMI Error' = median_eBMI_diff,
         'Mean eBMI Error' = mean_eBMI_diff) 

# save Fig_1_Results
write.csv(Fig_1_Results, "Tabs/Fig_2_Results.csv")
  
  # Add a flag for shading
  graph_1 <- graph_1 %>%
    mutate(shading = ifelse(sd == 0.4, "shaded", "none"))

  # Custom labels for the rho parameter
  rho_labels <- c('0.7' = "Autocorrelation = 0.7", '0.9' = "Autocorrelation = 0.9")
# make 0.9 before 0.7 in the rho factor
graph_1$rho <- factor(graph_1$rho, levels = c('0.9', '0.7'))
  # Assuming your main plot is already created and saved in variable 'p'
background_fill <-  "#EEECE1"
background_fill_light <- adjustcolor(background_fill, alpha.f = 0.2)

graph_1$sd <- factor(graph_1$sd, levels = c(0.2, 0.4), labels = c('sd = 0.2', 'sd = 0.4'))


  # Create the main plot
  p <- ggplot(graph_1, aes(y = median_eBMI_diff, x = as.numeric(n_datapoints), color = forecast_point)) +
    geom_rect(data = subset(graph_1, shading == "shaded"),
              aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
              fill = background_fill_light, alpha = 0.05, inherit.aes = FALSE) +
    geom_point(size = 3, alpha = 0.7) +
    geom_line(aes(group = interaction(rho, forecast_point), linetype = as.factor(rho)), size = 1.5) +
    facet_grid(~sd) +
    labs(
      title = "Median eBMI error by Training Datapoints, Within-Child SD, \n Prediction Point, and Autocorrelation in Control Sample",
      x = "N Datapoints (Training)",
      y = "BMI Prediction Error",
      color = "Forecast Point",
      linetype = "Autocorrelation"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey"),
      strip.text = element_text(size = 12, colour = "#F0F0F0"),  # make forecast point text smaller
      axis.text.y = element_text(size = 12)  # make y axis text a little smaller
    ) +
    scale_color_manual(values = c("Max" = "#1A4F66", 
                                  "Mean" = "#EF6C45", 
                                  "Mean + Most Recent" = "#C2B824", 
                                  "Most Recent" = "#A481C7", 
                                  "Population Median" = "#E1AD01")) +
    embarktools::embark_theme_a +
    theme(legend.position = 'right') +
    scale_x_continuous(breaks = seq(2, 10, 2)) +
    theme(strip.background = element_rect(fill = strip_fill_light, color = "#F0F0F0"), 
          strip.text = element_text(size = 14, color = "#F0F0F0", face = "bold")) + 
    theme(panel.grid.major = element_line(color = "grey")) 
  
  
    
p 

# save the graph
ggsave("Figs/TeenGrowth_Forecast_Results_BMI.png", width = 12, height = 10, dpi = 300)


library(ggplot2)
library(dplyr)

# Load results
load("Data/TeenGrowth_Forecast_Results.RData")

results <- results |> 
  mutate(rho = case_when (
    rho == '0.970714469623222' ~ '0.7',
    rho == '0.991258389045303' ~ '0.9'
  )) |> 
  mutate(n_datapoints = 
           case_when(
             n_datapoints == '3' ~ '2',
             n_datapoints == '6' ~ '4',
             n_datapoints == '9' ~ '6',
             n_datapoints == '12' ~ '8',
             n_datapoints == '15' ~ '10'))

# When ci = 95 or 99, collapse the results to the mean of the bmiz_within_ci
graph_2_9599 <- results |> 
  filter(ci %in% c(95, 99)) |>  # Filter for ci 95 and 99
  group_by(ci, rho, n_datapoints, forecast_point) |> 
  summarise(bmiz_within_ci = mean(bmiz_within_ci, na.rm = TRUE), .groups = 'drop') |> 
  # make sd column == 'combined'
  mutate(sd = 'Combined')

# Check the intermediate result
print("Graph 2 - 95 and 99")
print(graph_2_9599)

# Define a lighter strip background color
strip_fill <- "#1A4F66"
strip_fill_light <- adjustcolor(strip_fill, alpha.f = 0.7)  # Lighten the strip background using alpha

graph_2_user_defined <- results |> 
  filter(ci == 'User-Defined' & sd != '1') |> 
  group_by(sd, ci, rho, n_datapoints, forecast_point) |> 
  summarise(bmiz_within_ci = mean(bmiz_within_ci, na.rm = TRUE), .groups = 'drop') 

# Check the intermediate result
print("Graph 2 - User Defined")
print(graph_2_user_defined)

# Combine the two datasets
combined_graph_2 <- bind_rows(graph_2_9599, graph_2_user_defined) |> 
  mutate(forecast_point = case_when(
    forecast_point == 'max' ~ 'Max',
    forecast_point == 'mean+most_recent' ~ 'Mean + Most Recent',
    forecast_point == 'mean' ~ 'Mean',
    forecast_point == 'most_recent' ~ 'Most Recent', 
    forecast_point == 'pop_median' ~ 'Population Median')) |> 
  mutate (ci = case_when(
    ci == 95 ~ '95',
    ci == 99 ~ '99',
    ci == 'User-Defined' ~ '+/- 0.5 BMIz')) 


# Custom labels for the rho parameter
rho_labels <- c('0.7' = "Autocorrelation = 0.7", '0.9' = "Autocorrelation = 0.9")
# Add a flag for shading
combined_graph_2 <- combined_graph_2 |> 
  mutate(shading = ifelse(rho == 0.9, "shaded", "none"))

combined_graph_2$ci <- factor(combined_graph_2$ci, levels = c('95', '99', '+/- 0.5 BMIz'))

# Create the plot with linetype for ci
combined_graph <- ggplot(combined_graph_2, aes(y = bmiz_within_ci, x = as.numeric(n_datapoints), color = forecast_point, linetype = as.factor(sd))) +
  geom_rect(data = subset(combined_graph_2, shading == "shaded"),
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "#EEECE1", alpha = 0.05, inherit.aes = FALSE) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(aes(group = interaction(forecast_point, sd)), size = 1.5) +
  facet_grid(ci ~ rho, labeller = labeller(rho = rho_labels)) +
  labs(
    title = "% of BMIz within Prediction Interval in Control Children",
    x = "N Datapoints (Training)",
    y = "% in Forecast Window",
    color = "Forecast Point",
    linetype = "BMIz SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey"),
    strip.text = element_text(size = 12, colour = "#F0F0F0"),  # make forecast point text smaller
    axis.text.y = element_text(size = 12)  # make y axis text a little smaller
  ) +
  scale_color_manual(values = c("Max" = "#1A4F66", 
                                "Mean" = "#EF6C45", 
                                "Mean + Most Recent" = "#C2B824", 
                                "Most Recent" = "#A481C7", 
                                "Population Median" = "#E1AD01")) +  
 scale_linetype_manual(values = c("0.1" = "dashed", "0.3" = "dotted", "Combined" = "solid")) +
  embarktools::embark_theme_a +
  theme(legend.position = 'right') + 
  theme(strip.background = element_rect(fill = strip_fill_light, color = "#F0F0F0"),           
        strip.text = element_text(size = 12, color = "#F0F0F0", face = "bold")) + 
  theme(panel.grid.major = element_line(color = "grey")) 




combined_graph
# save as combined graph
ggsave("Figs/Combined_Graph.png", width = 12, height = 10, units = "in", dpi = 300)

# remove shading column, order descending by bmi_within_ci, make bmi_within_ci a % with two decimal places using sprint f, rename 'ci' to 'prediction interval' and save the dataframe
combined_graph_2 |> 
  select(-shading) |> 
  arrange(desc(bmiz_within_ci)) |> 
  mutate(bmiz_within_ci = sprintf("%.2f%%", bmiz_within_ci)) |> 
  rename("Prediction Interval" = ci) |> 
  write.csv("Tabs/Combined_Graph.csv", row.names = FALSE)

combined_graph
p
