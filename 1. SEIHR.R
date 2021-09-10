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
start_date='2020-11-04'
end_date='2021-02-15'
df_korea=read.csv('data/covid_korea_n.csv')
df_korea_first=subset(df_korea, DATE>start_date & DATE<end_date)
df_korea[is.na(df_korea)]=0
cases=df_korea_first$newconfirm
## 1.2 Data Drawing
df_korea_first %>%
  ggplot()+
  geom_bar(mapping=aes(x=DATE, y=newconfirm), fill='orange', color='orange', stat='identity')

## 1.3 Parameter Input
N=51311292
n_days=length(cases)
t=seq(1,n_days, by=1)
t0=0
t=t
i0=1
s0=N-i0
r0=0
y0=c(S=s0, I=i0, R=r0)
date_switch <- start_date # date of introduction of control measures
tswitch <- df_korea_first %>% filter(DATE < date_switch) %>% nrow() + 1 # convert time to number

## 1.4 Add Stan Parameter
data_forcing <- list(n_days = n_days, t0 = t0, ts = t, N = N, cases = cases, tswitch = tswitch)
model_forcing=stan_model('code/covid_fin.stan')

# 2. Modeling
fit_forcing_survey <- sampling(model_forcing, 
                               data_forcing, 
                               iter=1000,
                               seed=7)
# 3. Predict
smr_pred <- as.data.frame(summary(model, pars = "pred_cases", probs = c(0.05, 0.5, 0.95))$summary)

# 4. Model Test
#seihr_rmse=sqrt(sum((df_korea_first$newconfirm[1:len_data]-smr_pred$`50%`)^2)/length(smr_pred$mean))
seihr_rmse=rmse(korea_Test$newconfirm, smr_pred$mean)
seihr_mse=sum((df_korea_first$newconfirm[1:len_data]-smr_pred$`50%`)^2)/length(smr_pred$mean)
seihr_mae=sum(abs(df_korea_first$newconfirm[1:len_data]-smr_pred$`50%`))/length(smr_pred$mean)
seihr_mape=((sum((abs(df_korea_first$newconfirm[1:len_data]-smr_pred$`50%`))/df_korea_first$newconfirm))*100)/length(smr_pred$mean)
colnames(smr_pred) = make.names(colnames(smr_pred)) # to remove % in the col names

# 5. Drawing Plot
ggplot(smr_pred, mapping = aes(x = t[1:(len_data-1)])) +
  geom_ribbon(aes(ymin = X5. , ymax=X95.), fill = 'yellow', alpha=0.5 ) +
  #geom_ribbon(aes(ymin = X2.5. , ymax =X99.), fill = 'yellow', alpha=0.35) +
  #geom_ribbon(aes(ymin = X2.5. , ymax =X97.), fill = 'orange', alpha=0.5) +
  #geom_ribbon(aes(ymin = X2.5. , ymax =X95.), fill = 'green', alpha=0.5) +
  #geom_ribbon(aes(ymin = X2.5. , ymax =X90.), fill = 'blue', alpha=0.5) +
  geom_line(mapping = aes(x = t[1:(len_data-1)], y = X50.), color = 'black') +
  geom_point(mapping = aes(y = cases[1:len_data-1])) +
  labs(x = "Day", y = "Incidence")