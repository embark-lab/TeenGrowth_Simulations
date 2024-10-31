library(dplyr)

Fig1<-read.csv('Tabs/Fig_1_Results.csv')

fig1.4 <- Fig1 |> filter(sd == 0.4 & forecast_point != 'Max')

mean(fig1.4$Mean.eBMI.Error)
median(fig1.4$Median.eBMI.Error)

fig1.4.Median <- Fig1 |> filter(sd == 0.4 & forecast_point != 'Population Median')
mean(fig1.4.Median$Mean.eBMI.Error)
median(fig1.4.Median$Median.eBMI.Error)


fig1.2.Mostrecent <- Fig1 |> filter(sd == 0.2 & forecast_point == 'Most Recent')
mean(fig1.2.Mostrecent$Mean.eBMI.Error)
median(fig1.2.Mostrecent$Median.eBMI.Error)
