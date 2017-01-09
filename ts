APM <- read.csv('AmtrakPassengersMonthly T-Competition.csv')
APM[,3:6]<- NULL
ridership_ts <- ts(APM$Ridership, start = c(1991,1), end = c(2004,3), frequency = 12)
plot(ridership_ts, xlab = 'Time', ylab = "Ridership", ylim = c(1300,2300), bty = 'l')

#install.packages('forecast')
library(forecast)

ridership_lm <- tslm(ridership_ts ~ trend + I(trend^2))
#plot(ridership_lm)
par(mfrow = c(2,1))

plot(ridership_ts, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = 'l')
lines(ridership_lm$fitted, lwd = 2)

#Zooming in 
ridership_ts_zoom <- window(ridership_ts, start = c(1997,1), end = c(2000,12))
plot(ridership_ts_zoom, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = 'l') 


temp <- ts(APM$Ridership, start = c(1997,1), end = c(2000,12), frequency = 12)
plot(temp, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = 'l')


#Validation period 
nValid <- 36 
nTrain <- length(ridership_ts) - nValid 
train.ts <- window(ridership_ts, start = c(1991,1), end = c(1991,nTrain))
valid.ts <- window(ridership_ts, start = c(1991, nTrain+1), end = c(1991, nValid + nTrain))

ridership_lm <- tslm(train.ts ~ trend + I(trend^2))  
ridership_lm_pred <- forecast(ridership_lm, h = nValid, level = 0) 
# h - number of period for forecasting #level - CI for prediction interval  
plot(ridership_lm_pred, ylim = c(1300,2600), ylab = 'Ridership', xlab = 'Time', bty = 'l', xaxt = 'n'
     ,xlim = c(1991,2006.25), main = "", flty = 2) #xaxt = 'n' hides values from x-axis
lines(ridership_lm$fitted, lwd = 2) #model line 
lines(valid.ts)  # line of validation set 

accuracy(ridership_lm_pred$mean, valid.ts)

