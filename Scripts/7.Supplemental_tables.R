load('Tabs/Example_Data.RData')
supp_3 <- read.csv('Tabs/Combined_Graph.csv')
supp_1 <- combined_forecast
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
