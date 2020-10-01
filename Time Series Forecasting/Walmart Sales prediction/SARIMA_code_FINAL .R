## MINI-PROJECT
#STORE TYPE A - 20
store_20 = read.csv("Store20_Prob1.csv")
train1 = store_20$Total_Sales[1:110]
test1 = store_20$Total_Sales[111:142]

#understanding store_20
plot(train1)
plot(ts(train1, start=2010))


#checking for stationarity
#No trend or seasonality, walmart sales are constant throughout the year
library("astsa")
library("forecast")


data1 = ts(train1, frequency = 52, start= c(2010,5), end=c(2012,13))
series1 = diff(log(data1))
plot(series1)
acf2(series1, max.lag = 110)
length(series1)

#remove seasonality
a = decompose(series1, type="additive")
plot(a)
seas.store_20 = a$seasonal
series1.1 = series1 - seas.store_20 
plot(series1.1)
acf2(series1.1, max.lag = 110)

sarima(series1.1,1,0,1,0,0,0,0)

str(data1)
#PREDICTION
predict = predict(arima(test1, order = c(1,0,1)), n.ahead = 32)
error1 = test1 - predict$pred
error1


#MAPE
mape1 = mean((abs(error1)/test1))
mape1            



##STORE TYPE B
store_3 = read.csv("Store3_Prob1.csv")
train2 = store_3$Total_Sales[1:110]
test2 = store_3$Total_Sales[111:142]

#understanding store_20

plot(train2)
plot(ts(train2, start=2010))

#checking for stationarity

data2= ts(train2, frequency = 52, start= c(2010,5), end=c(2012,13))
series2 = diff(log(data2))
plot(series2)
acf2(series2, max.lag = 100)

#remove seasonality
b = decompose(series2, type="additive")
plot(b)
seas.store_3 = b$seasonal
series1.2 = series2 - seas.store_3
plot(series1.2)
acf2(series1.2, max.lag = 110)


sarima(series1.2,3,0,2,0,0,0,0)


#PREDICTION
predict2 = predict(arima(test2, order = c(3,0,2)), n.ahead = 32)
error2 = test2 - predict2$pred
error2


#MAPE
mape2 = mean((abs(error2)/test2))
mape2            


##STORE TYPE c
store_30 = read.csv("Store30_Prob1.csv")
train3 = store_30$Total_Sales[1:110]
test3 = store_30$Total_Sales[111:142]


#understanding store_30

plot(train3)
plot(ts(train3, start=2010))

#checking for stationarity

data3 = ts(train3, frequency = 52, start= c(2010,5), end=c(2012,13))
series3 = diff(log(data3))
plot(series3)
acf2(series3, max.lag = 100)


#remove seasonality
c = decompose(series3, type="additive")
plot(c)
seas.store_30 = c$seasonal
series1.3 = series3 - seas.store_30
plot(series1.3)
acf2(series1.3, max.lag = 110)


sarima(series1.3,2,0,1,0,0,0,0)

#PREDICTION
predict3 = predict(arima(test3, order = c(2,0,1)), n.ahead = 32)
error3 = test3 - predict3$pred
error3


#MAPE
mape3 = mean((abs(error3)/test3))
mape3            

