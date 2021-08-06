#Downloading the packages

rm(list=ls())
library("fpp2")
library(ggplot2)
install.packages("fpp2")
library(readxl)
library(fpp2)
data<-read_excel("C:/Users/Beloved/Downloads/Unideb-20200202T184311Z-001/Unideb/thesis/dATA FOR PRECIPITATION.xlsx")
Y <- ts (data[,13] ,start = c(1901,1), end = c(2019,1),frequency = 1)
autoplot(Y)+
  ggtitle("Time Plot: Precipitation 1901 to 2019")+
  ylab("Precipitation mm")+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank())
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),

DY <- diff(Y)
autoplot(DY)+
  ggtitle("Time plot: Change in Debrecen Precipitation from 1901 to 2019")+
  ylab("Precipitation mm")
#ggseasonplot(DY)+
 # ggtitle("Time plot: Change in Debrecen rainfall from 1901 to 2019")+
 # ylab("Rainfall mm")
#ggsubseriesplot(DY)

fit<-naive(DY)
fit<- snaive(DY)
print(summary(fit))
checkresiduals(fit)

predict(fit, n.ahead = 50*12)

fit_ets<- ets(DY)
print(summary(fit_ets))
checkresiduals(fit_ets)

fit_arima<- arima(DY)
print(summary(fit_arima))
checkresiduals(fit_arima)

fit_arimac<- auto.arima(Y, d-1, D-1, stepwise = FALSE, approximation = FALSE, trace = TRUE)

