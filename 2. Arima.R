## 1. Library
library(tseries)
library(urca)
library(ggplot2)
library(forecast)
library(stats)
library(lmtest)
library(dplyr)
library(Metrics)
library(doBy)

## 2. Data Input
### 2.1 Train Data Set
df_korea=read.csv('data/covid_korea_n.csv')
start_date_Train='2020-01-15'
end_date_Train='2020-11-03'
korea_Train=subset(df_korea, DATE>start_date_Train & DATE<end_date_Train)
korea_Train_ts<-ts(korea_Train$newconfirm, start=1, frequency = 7)

### 2.2 Test Data Set
start_date_Test='2020-11-04'
end_date_Test='2021-02-15'
korea_Test=subset(df_korea, DATE>start_date_Test & DATE<end_date_Test)
korea_Test_ts<-ts(korea_Test$newconfirm, start=1, frequency = 7)

## 3. Stationary Test
#### 3.1 ADF & KPSS
adf.test(korea_Train_ts)
summary(ur.kpss(korea_Train_ts))
#### Timeseries Not Stationary. So, let's try to log transform & difference
#### 3.2 Log transform & Difference, Test ADF & KPSS
##### Log transform & Test
korea_Train_log=log(korea_Train_ts)
korea_Train_log[is.infinite(korea_Train_log)]=0
adf.test(korea_Train_log)
ur.kpss(korea_Train_log)
##### DIffs & Test
ndiffs(korea_Train_ts)
ndiffs(korea_Train_log)
korea_diff<-diff(korea_Train_ts, differences = 1)
adf.test(korea_diff)
ur.kpss(korea_diff)
##### In results, Only log transform accept not diffs. 

## 4. ARIMA Model Select
### 4.1 Empty data.frame create
b2<-as.data.frame(0)
b3<-as.data.frame(0)
b4<-as.data.frame(0)
b5<-as.data.frame(0)
b6<-as.data.frame(0)
b7<-as.data.frame(0)

### 4.2 Test data set log transform
korea_Test_log=log(korea_Test_ts)
korea_Test_log[is.infinite(korea_Test_log)]=0


### 4.2 Model Selection
for(i in 3:0){
  a1<-try(Arima(korea_Train_log, order=c(i,1,0)))
  for(j in 10:0){
    b1<-try(Arima(korea_Train_log, order=c(i,1,j), method='ML'))
    b_test<-try(Box.test(b1$residuals, type="Ljung-Box"))
    b_test2<-try(coeftest(b1))
    b_test3<-try(b_test2[,4])
    b_fcast<-try(forecast(b1, h=length(korea_Test)))
    diffs<- try(exp(rmse(korea_Test_log, ts(b_fcast$mean, frequency=7))))
    b3<- try(as.data.frame(cbind(b_fcast$method, b1$aic, b1$aicc, b1$bic, diffs)))
    b4<- try(bind_rows(b4, b3))
    b4[is.na(b4)]<-0
    print(c('AR',i,'MA', j))
    }
}
names(b4)=c('0', 'model', 'AIC', 'AICc', 'BIC', 'RMSE')
b4_order=orderBy(~RMSE, b4)
head(b4_order)
##Select Model ARIMA(3,1,6) 

### 4.3 Plotting
Arima_316=Arima(korea_Train_log, order=c(2,1,3), method='ML')
ARIMA_forecast=forecast(Arima_316, h=length(korea_Test$newconfirm))
Arima_exp=exp(ARIMA_forecast$fitted[1:length(korea_Test$newconfirm)])
Arima_exp_mean=exp(ARIMA_forecast$mean)
result_table=data.frame(korea_Test$DATE,korea_Test$newconfirm, Arima_exp, Arima_exp_mean)
names(result_table)=c('DATE', 'newconfirm', 'predict', 'mean')
result_table$DATE=as.Date(result_table$DATE)
result_table%>%
  ggplot()+
  geom_line(aes(x=DATE, y=newconfirm), color='red')+
  geom_line(aes(x=DATE, y=predict), color='blue')+
  geom_line(aes(x=DATE, y=mean), color='green')

### 4.4 RMSE
rmse(result_table$newconfirm, result_table$predict)
rmse(result_table$newconfirm, result_table$mean)
