getwd()
setwd("C:/Users/nimee/Documents/MSA Spring 2020/8200-Predictive Analytics/MiniProject")
require(astsa)
train <- read.csv("Store30_Prob2.csv")
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

train_data<-head(train,110)
test_data<-tail(train,33)

train.id = 1:110
test.id = 111:142

# Regression

summary(fit <- lm(Total_Sales ~., data = train_data))
summary(fit <- lm(Total_Sales ~Temperature + MarkDown2 + month + year, data = train_data))
summary(fit <- lm(Total_Sales ~Temperature + month + year, data = train_data))

# Try different Regression models

# Run Time Series with the best model
summary(fit1 <- lm(Total_Sales ~Temperature + month + year, data = train_data)) #R-squared: 0.3826
series1 = ts(resid(fit1), frequency = 52, start= c(2010,5), end=c(2012,13))
plot(series1)
acf2(series1,max.lag = 100) # AR0 MA0 

#removing seasonality
a1<-decompose(series1)
plot(a1)
season_trend <- a1$seasonal
plot.ts(season_trend)
series1_wos <- series1 - season_trend
plot(series1_wos)
acf2(series1_wos,max.lag = 100) # AR3 MA3 model


fit <- Arima(train$Total_Sales[train.id],c(3,0,3),
             xreg=cbind(train$Temperature,train$MarkDown2,train$MarkDown3,train$month)[train.id,])
fit #AIC=2490.93 for ARMA(3,3) & AIC=2485.59 for ARMA(3,4)
fit2 <- Arima(train$Total_Sales[test.id],c(3,0,3),
              xreg=cbind(train$Temperature,train$MarkDown2,train$MarkDown3,train$month)[test.id,],model = fit)
fit2 #AIC=719.59 same for ARMA(3,3) & ARMA(3,4)
onestep <- fitted(fit2)
onestep
plot(train$Total_Sales[test.id])
lines(time(train$Total_Sales[test.id]),as.vector(onestep),col="red")
mean(((train$Total_Sales[test.id]-as.vector(onestep))/train$Total_Sales[test.id])^2)

