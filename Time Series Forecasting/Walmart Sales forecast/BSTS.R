####Store 3 modeling
install.packages("csv")
library(csv)

Store3 <- read.csv("C:/Users/keert/Desktop/Semester 2/Predictive/Prpject/walmart-recruiting-store-sales-forecasting/Store3_Prob2.csv")
rownames(Store3) <- Store3$Date
Store3 = subset(Store3, select = -c(Date))
Store3_train <- head(Store3, 114)
Store3_test <- tail(Store3, 52)
Store3_SalesTest <- Store3_test[11] #For storing 
Store3_test <- Store3_test[-11]

install.packages("bsts")
library(bsts)
View(Store3) 
data.frame(colnames(Store3))

#Just the time series plot- local level
just <- ts(Store3_train$Total_Sales, start = c(2010,2,5),
           end = c(2012,10,26), frequency = 52)
plot(just)

ll_ss <- list()
ll_ss <- AddLocalLevel(state.specification = ll_ss, y=just)
ll_model <- bsts(just, state.specification = ll_ss, niter = 1000)


# Adding linear trend and seasonality
# Without regression component
ss <- AddLocalLinearTrend(list(), Store3_train$Total_Sales)
ss <- AddSeasonal(ss, Store3_train$Total_Sales, nseasons = 52)

model1 <- bsts(Store3_train$Total_Sales,state.specification = ss,niter = 1000)

plot(model1)
plot(model1, "components")

pred1 <- predict(model1,newdata = Store3_test, horizon = 12)
plot(pred1, plot.original = 156)

#With regression component
model2 <- bsts(Total_Sales ~ ., state.specification = ss,
               data = Store3_train, niter =  3000, 
               expected.model.size = 1)

plot(model2, "comp")
plot(model2, "coef")

newpred<- predict(model2, newdata =Store3_test, horizon = 52)
plot(newpred, plot.original =90)

#Model3
ss <- AddLocalLinearTrend(list(), Store3_train$Total_Sales)
ss <- AddSeasonal(ss, Store3_train$Total_Sales, nseasons = 52)
model3 <- bsts(Total_Sales ~ ., state.specification = ss, data = Store3_train, niter = 1000,expected.model.size = 5) 


plot(model3, "comp")
plot(model3, "coef")

newpred<- predict(model3, newdata =Store3_test, horizon = 30)
plot(newpred, plot.original =90, main = 'trend')

actual<-Store3_SalesTest$Total_Sales
fitted<-newpred$mean
MAPE=mean(abs(actual-fitted)/actual)
MAPE

#Compare models
CompareBstsModels(list("Model 1" = model1, "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))

##### STORE 20

Store20 <- read.csv("C:/Users/keert/Desktop/Semester 2/Predictive/Prpject/walmart-recruiting-store-sales-forecasting/Store20_Prob2.csv")
rownames(Store20) <- Store20$Date
Store20 = subset(Store20, select = -c(Date))
Store20_train <- head(Store20, 114)
Store20_test <- tail(Store20, 52)
Store20_SalesTest <- Store20_test[11] #For storing 
Store20_test <- Store20_test[-11]


#Without regression component
ss <- AddLocalLinearTrend(list(), Store20_train$Total_Sales)
ss <- AddSeasonal(ss, Store20_train$Total_Sales, nseasons = 52)

model1 <- bsts(Store20_train$Total_Sales,state.specification = ss,niter = 1000)

plot(model1)
plot(model1, "components")

pred1 <- predict(model1,newdata = Store20_test, horizon = 12)
plot(pred1, plot.original = 156)

par("mar")
par(mar=c(1,1,1,1))

#With regression
model2 <- bsts(Total_Sales ~ ., state.specification = ss,data = Store20_train, niter =  3000)

plot(model2, "comp")
plot(model2, "coef")

newpred<- predict(model2, newdata =Store20_test, horizon = 30)
plot(newpred, plot.original =90, main = 'trend')

#Model3
model3 <- bsts(Total_Sales ~ ., state.specification = ss, data = Store20_train, niter = 500,expected.model.size = 5) 


plot(model3, "comp")
plot(model3, "coef")

newpred<- predict(model3, newdata =Store20_test, horizon = 30)
plot(newpred, plot.original =90, main = 'trend')

#Compare models
CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))

actual<-Store20_SalesTest$Total_Sales
fitted<-newpred$mean
MAPE=mean(abs(actual-fitted)/actual)
MAPE

####STORE 30

Store30 <- read.csv("C:/Users/keert/Desktop/Semester 2/Predictive/Prpject/walmart-recruiting-store-sales-forecasting/Store30_Prob2.csv")
rownames(Store30) <- Store30$Date
Store30 = subset(Store30, select = -c(Date))
Store30_train <- head(Store30, 114)
Store30_test <- tail(Store30, 52)
Store30_SalesTest <- Store30_test[11] #For storing 
Store30_test <- Store30_test[-11]


#Withot regression component
ss <- AddLocalLinearTrend(list(), Store30_train$Total_Sales)
ss <- AddSeasonal(ss, Store30_train$Total_Sales, nseasons = 52)

model1 <- bsts(Store30_train$Total_Sales,state.specification = ss,niter = 1000)

plot(model1)
plot(model1, "components")

pred1 <- predict(model1,newdata = Store30_test, horizon = 12)
plot(pred1, plot.original = 156)


#With reg
model2 <- bsts(Total_Sales ~ ., state.specification = ss,data = Store30_train, niter =  10000)

plot(model2, "comp")
plot(model2, "coef")

newpred<- predict(model2, newdata =Store30_test, horizon = 30)
plot(newpred, plot.original =90, main = 'trend')

#Model3
model3 <- bsts(Total_Sales ~ ., state.specification = ss, data = Store30_train, niter = 1000,expected.model.size = 5) 

plot(model3, "comp")
plot(model3, "coef")

newpred<- predict(model3, newdata =Store30_test, horizon = 30)
plot(newpred, plot.original =90, main = 'trend')

#Compare models
CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))

actual<-Store30_SalesTest$Total_Sales
fitted<-newpred$mean
MAPE=mean(abs(actual-fitted)/actual)
MAPE
