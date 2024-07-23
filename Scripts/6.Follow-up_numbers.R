library(dplyr)

Fig1<-read.csv('Tabs/Fig_1_Results.csv')

fig1.6 <- Fig1 |> filter(sd == 0.6 & forecast_point != 'Max')

mean(fig1.6$Mean.eBMI.Error)
median(fig1.6$Median.eBMI.Error)

fig1.2.Mostrecent <- Fig1 |> filter(sd == 0.2 & forecast_point == 'Most Recent')
median(fig1.2.Mostrecent$Median.eBMI.Error)
