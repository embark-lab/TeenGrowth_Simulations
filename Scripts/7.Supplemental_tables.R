load('Tabs/Example_Data.RData')
load('Tabs/ED_confusion_matrix.RData')
supp_1 <- forecast_data
supp_3 <- confusion

# round numieric columns of supp_3 to 2 decimal places
supp_3 <- supp_3 %>%
  mutate_if(is.numeric, ~round(., 2))
# sort decending by sensitivity
supp_3 <- supp_3 %>%
  arrange(desc(accuracy))

supp_2 <- read.csv('Tabs/Fig_1_Results.csv')
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "ST1-Example_Forecasts")
writeData(wb, "ST1-Example_Forecasts", supp_1, rowNames = FALSE)

addWorksheet(wb, "ST2-Figure_2_Data")
writeData(wb, "ST2-Figure_2_Data", supp_2, rowNames = FALSE)

addWorksheet(wb, "ST3-Figure_3_Data")
writeData(wb, "ST3-Figure_3_Data", supp_3, rowNames = FALSE)

saveWorkbook(wb, file = "Tabs/Supplmemental_Tables.xlsx", overwrite = TRUE)
