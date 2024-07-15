
library(reshape2)
library(ggplot2)
library(dplyr)
# load confusion matrix
load('Data/ED_confusion_matrix.RData')

# Check for NA values in ci and sd
confusion <- confusion %>%
  filter(!is.na(prediction_interval) & !is.na(sd))

# Melt the confusion data frame for visualization
confusion_melted <- melt(confusion, id.vars = c("rho", "sd", "n_datapoints", "prediction_interval", "forecast_point"))

# Order data by n_datapoints
confusion_melted <- confusion_melted %>%
  arrange(n_datapoints)

# order prediction_interval as a factor to be 95, 99, +/- 0.5 SD
confusion_melted$prediction_interval <- factor(confusion_melted$prediction_interval, levels = c('95', '99', '+/- 0.5 BMIz'))
# Change autocorrelation factor levels
confusion_melted$rho <- factor(confusion_melted$rho, levels = c('0.8', '0.4'))

library(ggplot2)
library(gtable)
library(grid)

# Add a 'shaded' column to the dataset
confusion_melted <- confusion_melted %>%
  mutate(shaded = ifelse(sd %in% c(0.2, 0.6), "shaded", "not_shaded"))

# Define the background color for shading
shaded_fill <- "#F5F5DC"
library(dplyr)
library(ggplot2)

# Add a 'shaded' column to the dataset
confusion_melted <- confusion_melted %>%
  mutate(shaded = ifelse(sd %in% c(0.2, 0.6), "shaded", "not_shaded"))

# Define the background color for shading
shaded_fill <- "#F5F5DC"

# Define a lighter strip background color
strip_fill <- "#1A4F66"
strip_fill_light <- adjustcolor(strip_fill, alpha.f = 0.7)  # Lighten the strip background using alpha

# recode n_datapoints to be 2,4,6,8,10 instead of 3,6,9,12,15
confusion_melted <- confusion_melted |> 
 mutate(n_datapoints = case_when(
    n_datapoints == 3 ~ 2,
    n_datapoints == 6 ~ 4,
    n_datapoints == 9 ~ 6,
    n_datapoints == 12 ~ 8,
    n_datapoints == 15 ~ 10
  ) )
  
  
# recode sd to be 'sd = 0.2', 'sd = 0.4', and  'sd = 0.6'
confusion_melted$sd <- factor(confusion_melted$sd, levels = c(0.2, 0.4, 0.6), labels = c('sd = 0.2', 'sd = 0.4', 'sd = 0.6'))

# Create the plot with shading
p <- ggplot(confusion_melted %>% filter(variable == 'accuracy'), 
            aes(x = n_datapoints, y = value, color = as.factor(forecast_point), 
                linetype = as.factor(rho), group = interaction(forecast_point, rho, sd))) +
  geom_rect(data = subset(confusion_melted, shaded == "shaded"),
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = shaded_fill, alpha = 0.05, inherit.aes = FALSE) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_grid(prediction_interval ~ sd) +
  labs(title = "Overall Accuracy Across Simulations", x = "N Datapoints (Training)", 
       y = "Accuracy", color = "Forecast Point", linetype = "Autocorrelation") +
  embarktools::embark_theme_a + 
  theme(legend.position = "right") +
  scale_color_manual(values = c("Max" = "#1A4F66", 
                                "Mean" = "#EF6C45", 
                                "Mean + Most Recent" = "#C2B824", 
                                "Most Recent" = "#A481C7")) +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  theme(strip.text = element_text(size = 12, color = '#EEECE1', face = "bold")) + 
  theme(strip.background = element_rect(fill = strip_fill_light, color = "#F0F0F0")) + 
  theme(panel.grid.major = element_line(color = "grey")) 

# Draw the plot
print(p)

# Save 
ggsave("Figs/Confusion_Accuracy.png", p, width = 10, height = 6, dpi = 300)

# make the variable column start with a capital letter
confusion_melted$variable <- gsub("accuracy", "Accuracy", confusion_melted$variable)
confusion_melted$variable <- gsub("sensitivity", "Sensitivity", confusion_melted$variable)
confusion_melted$variable <- gsub("specificity", "Specificity", confusion_melted$variable)
confusion_melted$variable <- gsub("ppv", "Positive Predictive Value", confusion_melted$variable)
confusion_melted$variable <- gsub("npv", "Negative Predictive Value", confusion_melted$variable)


library(ggplot2)
library(dplyr)
library(cowplot)
library(embarktools)

plot_confusion_metrics <- function(data, dv) {
  
  p <- ggplot(data %>% filter(variable == dv), 
              aes(x = n_datapoints, 
                  y = value, 
                  color = as.factor(forecast_point), 
                  linetype = as.factor(rho), 
                  group = interaction(forecast_point, rho, sd))) +
    geom_rect(data = subset(data, shaded == "shaded"),
              aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
              fill = shaded_fill, alpha = 0.05, inherit.aes = FALSE) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    facet_grid(prediction_interval ~ sd) +
    labs(title = paste(dv), 
         color = "Forecast Point", 
         linetype = "Autocorrelation", 
         x = "N Datapoints (Training)") +
    embarktools::embark_theme_a + 
    scale_color_manual(values = c("Max" = "#1A4F66", 
                                  "Mean" = "#EF6C45", 
                                  "Mean + Most Recent" = "#C2B824", 
                                  "Most Recent" = "#A481C7")) +
    scale_x_continuous(breaks = seq(2, 10, 2)) +
    theme(strip.text = element_text(size = 12, color = '#EEECE1', face = "bold")) + 
    theme(strip.background = element_rect(fill = strip_fill_light, color = "#F0F0F0")) + 
    theme(axis.title.y = element_blank()) +
    ylim(0.1, 1) +
    theme(legend.position = 'top') + 
    theme(panel.grid.major = element_line(color = "grey")) 
    
  
  return(p)
}

# Example usage:
Plot_1 <- plot_confusion_metrics(confusion_melted, 'Sensitivity')
Plot_2 <- plot_confusion_metrics(confusion_melted, 'Specificity')
Plot_3 <- plot_confusion_metrics(confusion_melted, 'Positive Predictive Value')
Plot_4 <- plot_confusion_metrics(confusion_melted, 'Negative Predictive Value')

# Extract the legend components manually
legend_guide <- ggplotGrob(Plot_1)$grobs
legend <- legend_guide[[which(sapply(legend_guide, function(x) x$name) == "guide-box")]]


# Remove the legends from the graphs 
Plot_1 <- Plot_1 + theme(legend.position = "none", 
                         axis.text.x = element_blank(), 
                         axis.title.x = element_blank())
Plot_2 <- Plot_2 + theme(legend.position = "none", 
                         axis.text.y = element_blank(), 
                         axis.text.x = element_blank(), 
                         axis.title.x = element_blank())
Plot_3 <- Plot_3 + theme(legend.position = "none")
Plot_4 <- Plot_4 + theme(legend.position = "none", 
                         axis.text.y = element_blank())

# Combine plots into a grid
combined_plot <- cowplot::plot_grid(Plot_1, Plot_2, Plot_3, Plot_4, ncol = 2, align = 'hv')

# Add title and legend
final_plot <- cowplot::plot_grid(
  cowplot::ggdraw() + cowplot::draw_label("Screening Performance Metrics Across Simulations", fontface = 'bold', color = "#1A4F66", size = 24),
  legend,
  combined_plot,
  ncol = 1,
  rel_heights = c(0.1, 0.1, 2)
)

# Print the final plot
print(final_plot)

# Save the final plot
ggsave("Figs/Combined_Confusion_Metrics.png", final_plot, width = 12, height = 12, dpi = 300)



