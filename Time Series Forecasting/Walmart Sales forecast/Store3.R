getwd()
setwd("/Users/pranidhiprabhat/Desktop/MSA/Spring/PA/walmart")
require(astsa)
train <- read.csv("Store3_Prob2.csv")
colnames(train)
#Removing Date
train<-train[-3]
#Removing Type_B & Type_C
train<-train[-17,-18]
#Removing Store
train<-train[-2]
#Removing X & Type_C
train<-train[-1]
train<-train[-15]
colnames(train)
library(forecast)

train.id = 1:110
test.id = 111:142

train_data<-head(train,110)
test_data<-tail(train,33)

# Regression

summary(fit <- lm(Total_Sales ~., data = train_data))
summary(fit <- lm(Total_Sales ~Temperature, data = train_data))

# Try different Regression models



# Run Time Series with the best model
summary(fit1 <- lm(Total_Sales ~Temperature , data = train_data))
series2 = ts(resid(fit1), frequency = 52, start= c(2010,5), end=c(2012,13))
plot(series2)
acf2(resid(fit1),max.lag = 100) #AR1 MA1

#removing seasonality
a2<-decompose(series2)
plot(a2)
season_trend <- a2$seasonal
plot.ts(season_trend)
series2_wos <- series2 - season_trend
plot(ts(series2_wos))
acf2(series2_wos,max.lag = 100) # AR1 MA1 model



# Fittting the model
fit <- Arima(train$Total_Sales[train.id],c(1,0,1),xreg=cbind(train$Temperature,train$MarkDown2,train$MarkDown3,train$month)[train.id,])
fit
fit2 <- Arima(train$Total_Sales[test.id],c(1,0,1),xreg=cbind(train$Temperature,train$MarkDown2,train$MarkDown3,train$month)[test.id,],model = fit)

# Predicting
onestep <- fitted(fit2)
onestep
plot(train$Total_Sales[test.id])
lines(time(train$Total_Sales[test.id]),as.vector(onestep),col="red")
mean((train$Total_Sales[test.id]-as.vector(onestep))^2)

#MAPE
mean(((train$Total_Sales[test.id]-as.vector(onestep))/train$Total_Sales[test.id])^2)


















