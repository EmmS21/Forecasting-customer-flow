install.packages("forecast")
install.packages("prophet")
library(prophet)
library(plyr)
library(dplyr)
library(forecast)
library(lubridate)
library(ggplot2)
library(tseries)
#upload fules
passflow = read.csv("~/Downloads/passflow-w.csv", header = TRUE)
#datetime format and filter incomplete month
passflow <- passflow %>%
  mutate(txdate=as.Date(txdate,format="%Y-%m-%d"))%>%
  filter(txdate < as.Date('2018-09-01'))
#timeseries grouped by month
cleanpass <- passflow %>%
  group_by(floor_date(txdate,"month"))%>%
  dplyr::summarise(passflow=sum(passflow))
#rename column
colnames(cleanpass)[1] <- "dates"
cleanpass$dates <- NULL
#convert to time series
cleanpasstime <- ts(cleanpass,frequency =12,start=2012)
#visualising seasonal trends
cycle(cleanpasstime)
#plotting seasonal means/medians
boxplot(cleanpasstime~cycle(cleanpasstime))
#visualising trend
plot(aggregate(cleanpasstime,FUN=mean))
plot(cleanpasstime,type='o')
#Augmented Dickey-Fuller Test to test for stationarity
lapply(cleanpasstime, function(x) adf.test(x[!is.na(x)], 
                                           alternative='stationary')) 

#KPSS test
#kpss.test(diff(cleanpasstime),lshort=FALSE)
#training and test 
train_pass = window(cleanpasstime,start = c(2012,1),end=c(2018,5))
test_pass = window(cleanpasstime,start=c(2018,6),end=c(2018,8))
#find best values for arima model
vals <- auto.arima(train_pass,stepwise=FALSE,approximation=FALSE,seasonal =TRUE)
#forecasts using ARIMA
fit <- arima(train_pass,c(1,1,1),seasonal=list(order=c(2,0,0),period=12))
predicts<-predict(fit,n.ahead=3*12)
arima_forecasts = forecast(vals,h=3*12)
ts.plot(cleanpasstime,predicts$pred,lty=c(1,3),col=c(5,2),type="o")
#testing accuracy
index <- c(3,5) # MAE and MAPE
arima_forecasts = forecast(vals,h=3*12)
arima_acc<- accuracy(arima_forecasts,test_pass)
arima_acc[, index]
#seasonal naive forecast as benchmark
naive <- snaive(train_pass, h=3*12)
naive_acc<- accuracy(naive,test_pass)
naive_acc[, index]

#lets try forecasting with fbprophet
passflow$weekday <- NULL
#rename columns
colnames(passflow) <- c('ds','y')
#running fbprophet
p <- prophet(passflow,yearly.seasonality = TRUE,seasonality.mode = 'multiplicative')
fut_vals <- make_future_dataframe(p,periods=3*365)
forecasts <- predict(p,fut_vals)
plot(p, forecasts)
prophet_plot_components(p,forecasts)
#cross validation to test accuracy
df <- cross_validation(p,initial=2300,horizon=90,units='days')
index_cv <- c(4,5)
cv_metrics <- performance_metrics(df)
mean(as.matrix(cv_metrics[,index_cv][1])) #mae
mean(as.matrix(cv_metrics[,index_cv][2])) #mape
plot_cross_validation_metric(df,metric='mae')
plot_cross_validation_metric(df,metric='mape')
#plot predictions in clearer plot
test <- NULL
test$date <- forecasts$ds
test$predicts <- forecasts$yhat
test <- data.frame(test)
test <- test %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"))%>%
  group_by(month = floor_date(date,"month"))%>%
  dplyr::summarise(predicts=sum(predicts))
plot(test,type="o")
