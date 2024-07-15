library(dplyr)

Fig1<-read.csv('Tabs/Fig_1_Results.csv')

fig1.6 <- Fig1 |> filter(sd == 0.6 & forecast_point != 'Max')

mean(fig1.6$Mean.eBMI.Error)
median(fig1.6$Median.eBMI.Error)

denom = 65*65
multiplyer = 703/denom
=1/multiplyer

fig1.2.Mostrecent <- Fig1 |> filter(sd == 0.2 & forecast_point == 'Most Recent')
median(fig1.2.Mostrecent$Median.eBMI.Error)
