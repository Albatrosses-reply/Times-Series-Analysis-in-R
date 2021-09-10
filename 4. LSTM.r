install.packages('devtools')
devtools::install_github("rstudio/keras")
library(keras)
library(tensorflow)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)

# 1. Data Input
##1.1 Train Data Set
df_korea=read.csv('data/covid_korea_n.csv')
df_korea[is.na(df_korea)]=0
start_date_Train='2020-01-15'
end_date_Train='2020-11-03'
korea_Train=subset(df_korea, DATE>start_date_Train & DATE<end_date_Train)

## 1.2 Test data set
start_date_Test='2020-11-04'
end_date_Test='2021-02-15'
korea_Test=subset(df_korea, DATE>start_date_Test & DATE<end_date_Test)

## 2. Parameter
### 2.1 Scale Factor

scale_factor=c(mean(korea_Train$newconfirm), sd(korea_Train$newconfirm))

### 2.2 prediction parameter
prediction = length(korea_Test$newconfirm)
lag=prediction

### 2.3 Train data set
scale_train_data=df_korea%>%
  select(newconfirm)%>%
  mutate(newconfirm=(newconfirm-scale_factor[1])/scale_factor[2])

scaled_train=as.matrix(scale_train_data)

### 2.4 3D transform
x_train_data <- t(sapply(
  1:(length(scaled_train) - lag - prediction + 1),
  function(x) scaled_train[x:(x + lag - 1), 1]
))
  
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(
    nrow(x_train_data),
    lag,
    1
  )
)

### 2.5 Y 3D transform
y_train_data <- t(sapply(
  (1 + lag):(length(scaled_train) - prediction + 1),
  function(x) scaled_train[x:(x + prediction - 1)]
))

y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(
    nrow(y_train_data),
    prediction,
    1
  )
)

## 3. Test Data
x_test = df_korea$newconfirm[(nrow(scaled_train)-prediction+1):nrow(scaled_train)]

x_test_scaled=(x_test-scale_factor[1])/scale_factor[2]
x_pred_arr <- array(
  data = x_test_scaled,
  dim = c(
    1,
    lag,
    1
  )
)

dim(x_train_arr)
dim(y_train_arr)
head(x_train_arr)
## 4. LSTM model prediction
lstm_model=NULL
lstm_model <- keras_model_sequential()

lstm_model %>%
  layer_lstm(units = 100, # size of the layer
             batch_input_shape = c(1, 12, 1), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  #layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 100,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  #layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'accuracy')

summary(lstm_model)

lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE
)

lstm_forecast <- lstm_model %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

# we need to rescale the data to restore the original values
lstm_forecast <- lstm_forecast * scale_factor[2] + scale_factor[1]

## 5. Fitted Predict Moel

fitted <- predict(lstm_model, x_train_arr, batch_size = 1) %>%
  .[, , 1]

if (dim(fitted)[2] > 1) {
  fit <- c(fitted[, 1], fitted[dim(fitted)[1], 2:dim(fitted)[2]])
} else {
  fit <- fitted[, 1]
}

# additionally we need to rescale the data
fitted <- fit * scale_factor[2] + scale_factor[1]
fitted <- c(rep(NA, lag), fitted)

lstm_forecast <- timetk::tk_ts(lstm_forecast,
                               start = decimal_date(as.Date(start_date_Test)),
                               frequency = 365)

### 5.1 data input 
input_ts=timetk::tk_ts(df_korea$newconfirm, 
                       start=decimal_date(as.Date(start_date_Train)),
                       frequency=365)
### 5.2 Forecast object
forecast_list=list(
  model=NULL,
  method="LSTM",
  mean=lstm_forecast,
  x=input_ts,
  fitted=fitted,
  residuals=as.numeric(input_ts)-as.numeric(fitted)
)

class(forecast_list)="forecast"
forecast::autoplot(forecast_list)

