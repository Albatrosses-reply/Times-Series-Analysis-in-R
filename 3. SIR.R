library(rstan)
library(tidyverse)
library(gridExtra)
library(readr)
library(jsonlite)
library(forecast)
library(loo)
library(Metrics)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 1. Data Input
df_korea=read.csv('data/covid_korea_n.csv')
df_korea[is.na(df_korea)]=0
start_date_Train='2020-01-015'
end_date_Train='2020-11-03'
korea_Train=subset(df_korea, DATE>start_date_Train & DATE<end_date_Train)
korea_Train_ts<-ts(korea_Train$newconfirm, start=1, frequency = 7)

### 2.2 Test Data Set
start_date_Test='2020-11-04'
end_date_Test='2021-02-15'
korea_Test=subset(df_korea, DATE>start_date_Test & DATE<end_date_Test)
korea_Test_ts<-ts(korea_Test$newconfirm, start=1, frequency = 7)

cases=korea_Test$newconfirm

# 2. Input Parameter
N <- 51311292
i0 <- 1
s0 <- N - i0
r0 <- 0
y0 = c(S = s0, I = i0, R = r0)
n_days=length(cases)

# 3. Modeling
data_sir <- list(n_days = n_days, y0 = y0, t0 = t0, ts = t, N = N, cases = cases)
model_sir <- stan_model("code/stan/SIR.stan")
fit_sir <- sampling(model_sir, data_sir, iter=1000, seed=7)

# 3. Predict
sir_pred <- cbind(as.data.frame(summary(fit_sir, pars="pred_cases", probs=c(0.05, 0.5, 0.95))$summary), t)
len_data= length(start_date_Train)

  # 4. Model Test
#seihr_rmse=sqrt(sum((df_korea_first$newconfirm[1:len_data]-smr_pred$`50%`)^2)/length(smr_pred$mean))
sir_rmse=rmse(korea_Test$newconfirm, sir_pred$mean)
sir_mse=sum((korea_Test$newconfirm[1:len_data]-sir_pred$`50%`)^2)/length(sir_pred$mean)
sir_mae=sum(abs(korea_Test$newconfirm[1:len_data]-sir_pred$`50%`))/length(sir_pred$mean)
sir_mape=((sum((abs(korea_Test$newconfirm[1:len_data]-sir_pred$`50%`))/korea_Test$newconfirm))*100)/length(sir_pred$mean)
colnames(sir_pred) = make.names(colnames(sir_pred)) # to remove % in the col names


