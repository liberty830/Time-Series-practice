library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('ggrepel') # visualisation
library('RColorBrewer') # visualisation
library('ggridges') # visualisation

library('data.table') # data manipulation
library('dplyr') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string

library('lubridate') # date and time
library('forecast') # time series analysis
library('sp')


setwd("C:\\Users\\17096\\Desktop\\Data Science\\Nutrogena")

sub <- fread("sub.csv") %>% 
  
  mutate(dt = ymd(dt),
         wday = wday(dt, label = T),
         year = year(dt),
         month = month(dt)) %>% 
  
  filter(year >= 1900) %>% 
  
  as.tibble()

tp <- sub %>% 
  filter(city == "Toronto") %>% 
  mutate(diff = target - lag(target))

p1 <- tp %>% 
  group_by(month) %>% 
  summarise(mean = mean(target, na.rm = T),
            median = median(target, na.rm = T),
            max = max(target, na.rm = T),
            min = min(target, na.rm = T),
            sd = sd(target, na.rm = T)) %>% 
  ggplot(aes(as.integer(month), mean)) + 
  geom_line() +
  geom_point(size = 2, alpha = 0.5, color = 'blue') +
  scale_x_continuous(breaks = 1:12) +
  scale_colour_hue() +
  labs(x = "Month", y = "Mean Temperature")

p2 <- tp %>% 
  group_by(month) %>% 
  summarise(mean = mean(target, na.rm = T),
            median = median(target, na.rm = T),
            max = max(target, na.rm = T),
            min = min(target, na.rm = T),
            sd = sd(target, na.rm = T)) %>% 
  ggplot(aes(as.integer(month), median)) + 
  geom_line() +
  geom_point(size = 2, color = 'blue', alpha = 0.5) +
  scale_x_continuous(breaks = 1:12) +
  scale_colour_hue() +
  labs(x = "Month", y = "Median Temperature")

p3 <- tp %>% 
  group_by(month) %>% 
  summarise(mean = mean(target, na.rm = T),
            median = median(target, na.rm = T),
            max = max(target, na.rm = T),
            min = min(target, na.rm = T),
            sd = sd(target, na.rm = T)) %>% 
  ggplot(aes(as.integer(month), min)) + 
  geom_line() +
  geom_point(size = 2, alpha = 0.5, color = 'blue') +
  scale_x_continuous(breaks = 1:12) +
  scale_colour_hue() +
  labs(x = "Month", y = "Min Temperature")

p4 <- tp %>% 
  group_by(month) %>% 
  summarise(mean = mean(target, na.rm = T),
            median = median(target, na.rm = T),
            max = max(target, na.rm = T),
            min = min(target, na.rm = T),
            sd = sd(target, na.rm = T)
  ) %>% 
  ggplot(aes(as.integer(month), sd)) + 
  geom_point(size = 2, alpha = 0.5, color = 'blue') +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "Variation of Temperature") +
  theme(legend.position = 'none')

grid.arrange(p1,p2,p3,p4, layout_matrix = matrix(1:4, nrow = 2, byrow = T))

# It is obvious that temperature has a cycle along with months.
# The plot above clearly shows this.


# Density plot by year

plt_density <- function(df, target){
  
  p <- df %>% 
    ggplot(aes(target, as.factor(month), fill = month)) +
    geom_density_ridges(bandwidth = 0.3) +
    labs(x = 'Temperature Density by month')
  
  return(p)
}

plt_density(tp, target)

# From the density plots, temperature distributions are very different by month. 
# And in winter season, the dispersion is much more variable than the summber season.
# So, prediction for winter season's temperature should be harder than the summer season.


# Time Series plot

plt_ts <- function(df, m, series1, series2, option){
  
  p1 <- df %>% 
    ggplot(aes(dt, !!sym(series1))) + 
    geom_line() +
    geom_smooth(method = 'loess') +
    labs(y = str_c(series1, ' Temperature')) +
    ggtitle('Times series plot in total period')
  
  p2 <- df %>% 
    filter(month == m) %>% 
    ggplot(aes(dt, !!sym(series1))) + 
    geom_line() +
    geom_smooth(method = 'loess') +
    labs(y = '') +
    ggtitle(str_c('Time series plot in month: ', m))
  
  p3 <- df %>% 
    ggplot(aes(dt, !!sym(series2))) + 
    geom_line() +
    geom_smooth(method = 'loess') +
    labs(y = str_c(series2, ' Temperature')) +
    ggtitle('Times series plot in total period')
  
  p4 <- df %>% 
    filter(month == m) %>% 
    ggplot(aes(dt, !!sym(series2))) + 
    geom_line() +
    geom_smooth(method = 'loess') +
    labs(y = '') +
    ggtitle(str_c('Time series plot in month: ', m)) 
  
  if(option == F){
    
    grid.arrange(p1, p2, layout_matrix = matrix(c(1,2), nrow = 2, byrow = F))
    
  }else{
    
    grid.arrange(p1, p2, p3, p4, layout_matrix = matrix(c(1,2,3,4), nrow = 4, byrow = F))
  }
  
  
}

plt_ts(tp, 1, 'target', 'target', F)

# From the time plots, there looks seasonality and no trend. It should be 12 months cycle, but
# I can check this from periodogram later.


### Modeling

# First, I will use 12th order differencing Or I can try using "Month" as a covariate to address
# the seasonality.

tp$target_diff_12 <- c(rep(0, 12), diff(tp$target, 12))

par(mfrow = c(2,2))
plt_ts(tp, 1, 'target_diff_12', 'target_diff_12', option = F)

# From the plot, 12th order differencing seems to be effective to remove seasonality.
# And I will try "Month" covariate and investigate its residuals as target series, too.

fit_linear <- lm(target ~ as.factor(month), data = tp)
summary(fit_linear)

# As I saw from the plots, month variable is very significant and its R-squared is 96%, very high.
# And I will add its residuals as "target_res", and investigate this time plot.

tp$target_res <- fit_linear$residuals
plt_ts(tp, 1, 'target_res','target_res', F)
plt_ts(tp, 1, 'target_diff_12', 'target_res', T)

# The last 4 plots compare 12th order differenced series vs month effect removed residuals.
# They all seem to effectively treat seasonality, but I can't find out the better one from the plots.
# So, I will use periodogram first to identify periodic cycles, and fit both of these new target
# variables for modeling.


# Periodogram

pdg <- function(target){
  
  pdg <- tp %>% 
    select(!!sym(target)) %>% 
    ts() %>%  
    spectrum(plot = FALSE)
  
  1/pdg$freq[which.max(pdg$spec^2)]
  
  p1 <- tibble(period = 1/pdg$freq, power = pdg$spec^2) %>%
    ggplot(aes(period, power)) +
    geom_line(color = "black") +
    scale_x_log10(breaks = c(0, 12, 50, 100)) +
    ggtitle('Periodogram') +
    theme(legend.position = "none", axis.text.x  = element_text(angle=45))
  
  print(str_c("The most noticeable frequency is: ", 1/pdg$freq[which.max(pdg$spec^2)]))
  plot(p1)
  
}

pdg("target")
pdg("target_diff_12")
pdg("target_res")

# From the three periodogram plots, obviously the cycle of the orginal series is 12 month.
# And the other two new targets show, there is no noticeable periodicity because the y-axis,
# power value reduced very much, and there is no dominant one among all periods. It means I can
# use 12 order differenced series and month effect removed residuals as new targets.
# But the y-axis of 12th order differenced series is much higher than the other one.
# It means seasonality could remain for differenced series.
# So, the next step is investigating ACF/PACF plot to find out dependency structures.


### ACF/PACF

par(mfrow = c(2, 2))

Acf(tp$target_diff_12, type = 'correlation')
Pacf(tp$target_diff_12)

Acf(tp$target_res, type = 'correlation')
Pacf(tp$target_res)

# Two plots in the first row show ACF/PACF of 12th order differenced series and, 
# Two plots in the second row show those of month effect removed residuals series.
# It is interesting that they show very different structure. Differenced one still shows
# 12-month periodic dependencies because the peaks are repeated every 12 lag.
# In contrast, residual series does not show such a cycle, that is, seasonality seems to be
# effectively removed. But, ACF and PACF both are not collapsed to zero. So, I can assume
# ARMA for this series. I will compare two models below from the plots.
# The first model : SARIMA using 12-order differenced series
# The second model : ARIMA using residuals series with "auto.arima" function.


fit_diff_12 <- auto.arima(ts(tp$target_diff_12, frequency = 12))
summary(fit_diff_12)

fit_res <- auto.arima(ts(tp$target_res))
summary(fit_res)

# From the models' summaries, differenced target series was fitted ARIMA(4,0,1)(2,0,0) with
# frequency term = 12, and its BIC = 6028.82. And the residual target series was fitted ARIMA(1,1,2)
# and its BIC = 5584.4 which is lower than the first model. And the second model is much simpler
# than the first model. So, the best model would be ARIMA(1,1,2) using residual target series.
# In addtion, this is temperature data, so there was no outlier which was detected from "tsclean" function.
# The next step is a diagnosis of residuals.

checkresiduals(fit_diff_12)
checkresiduals(fit_res)

# From the residuals plots, the first model ARIMA(4,0,1)(2,0,0) does not show good result.
# This still has seasonality every 12 month cycle, which shows peaks at lag = 12, 24, ...
# In contrast, ARIMA(1,1,2) model on residuals show that almost every lag is within the zero boundary.
# So, obviously ARIMA(1,1,2) on month effect removed series should be chosen.
# And from the coefficient of AR(1)term = 0.57, the unit root is outside 1, which means stationary.

# But, one thing is that from Ljung-Box test, this model show "Lack of fitting" under
# the significance level = 0.05, that is, this residuals do not seems to be a white noise series.
# Maybe other more complicated methods need to be applied to solve this problem. 
# But I will stop here because the diagnostics plots show okay results.


### Forecasting

tp <- tp %>% 
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname))

draw_forecast <- function(fit, pred_len){  
  
  pred_len <- pred_len
  pred_range <- c(nrow(tp)-pred_len+1, nrow(tp))
  pre <- tp %>% head(nrow(tp)-pred_len)
  post <- tp %>% tail(pred_len)
  
  fc <- fit %>% forecast(h = pred_len, level = c(50,95))
  
  p <-  autoplot(fc) +
    geom_line(aes(rowname, target_res), data = post, color = "blue") +
    labs(x = "Time", y = "Forecasting")
  
  return(p)  
}

draw_forecast(fit_res, 60)
forecast(fit_res, 60)

# From the forecasting plot, it does not seem to capture the variation well.
# Except some initial forecasts, the values are all very similar, this is because
# this models contains 1 terms of a AR term, and 2 terms of MA terms with small coefficients.
# So, this model is not likely to show large variations. 
# Other more complicated models should be used to forecast.


# To get temperature data from "Month effect" removed residuals,
# some calculation is needed below.

get_temperature <- function(fit_linear, ini_month, residuals){
  
  coef_linear_fitting <- rep(c(0, fit_linear$coefficients[2:12]), 100)
  
  forecasts <- c()
  for(j in 1:length(residuals)){
    forecasts[j] <- -7.609658 + coef_linear_fitting[j+ini_month-1] + residuals[j]
  }
  
  return(round(forecasts, 1))
  
}

# This is transformed result from month variable added linear model.

get_temperature(fit_linear, 10, forecast(fit_res, 60)$mean)


# Now, I will calculate RMSE between actual temperature and forecasts of last 60 months.

tp %>% slice(1306:1365) %>% tail()

train <- tp %>% 
  filter(dt > ymd("1900-01-01") & dt < ymd("2008-10-01")) 

test <- tp %>% 
  filter(dt > ymd("2008-09-01") & dt < ymd("2013-10-01")) 

fit_val <- arima(train$target_res, order = c(1, 1, 2))


rmse <- function(actual, fitted){
  
  sqrt(sum(actual - fitted)^2)
  
}

# Plot actual residual vs fitted residual in last 60 months

fitted <- get_temperature(fit_linear, 10, forecast(fit_val, 60)$mean)

tibble(actual = test$target, fitted = fitted) %>% 
  rownames_to_column() %>% 
  ggplot() +
  geom_point(aes(rowname, actual), color = "blue") +
  geom_point(aes(rowname, fitted), color = "red") +
  labs(x = 'Time', y = 'Temperature') +
  ggtitle('Actual vs Forecasted Temperature of last 60 months (red = fitted, blue = actual)')

rmse(test$target, fitted)

# RMSE is 26.43 and from the plot, the values are close to actual temperatures.
# So, this forecasting looks good. 

# The limit is that I used test set data to estimate "Month effect" or to find the best
# ARIMA model, so this forecasting could show better result. And from the linear regression 
# summary using "Month" variable, the R-squared was 96%. So, this good forecasting result is
# mainly from linear regression rather than times series model, because in the previous 
# forecasting plot, the model does not capture variation very well.


### Some questions

## Does the time series model improve the precision of forecasting?

# I can check comparing RMSE of two models.
# First model : Simple linear regression using Month variable
# Second model : Apply ARIMA on the residuals of the first model.

rmse(test$target, fit_linear$fitted.values[1306:1365])

fitted2 <- fit_linear$fitted.values[1306:1365]

tibble(actual = test$target, fitted = fitted) %>% 
  rownames_to_column() %>% 
  ggplot() +
  geom_point(aes(rowname, actual), color = "blue") +
  geom_point(aes(rowname, fitted), color = "red") +
  geom_point(aes(rowname, fitted2), color = "green") +
  labs(x = 'Time', y = 'Temperature') +
  ggtitle('Actual vs Forecasted Temperature of last 60 months \n (red = fitted, blue = actual, green = Simple linear)')

# rmse without timeseries fitting is 66.65. This is much higher than times series model(26.43)
# So, for this data, time series model performs well.


## Is forecasting for winter season that has larger variance harder than summer season?

# From the density plots of each month, 
# Winter season : 12, 1, 2, 3 months
# Summer season : 6, 7, 8, 9 months

w <- c(3,4,5,6)
winter <- c(w, w+12, w+24, w+36, w+48)
s <- c(9,10,11,12)
summer <- c(s, s+12, s+24, s+36, s+48)

rmse(test$target[winter], fitted[winter])
rmse(test$target[summer], fitted[summer])

# The result is so interesting, RMSE of winter season: 18.146 is much higher than the 
# summer season: 0.584, so more caution is needed to predict more variable period in the series.



### Summary

# Because this is temperature data, it has seasonality whose the cycle is 12 months.
# So, this should be removed to make the stationary series. I used linear regression and
# 12th order differencing to remove the seasonality. First, month effect removed residual series
# does not remain seasonality, but differencing does not work well. These are checked from
# ACF/PACF plots and periodogram. So, the final model is ARIMA(1,1,2) using residuals series
# The orders are chosen from AIC using "auto.arima" function.
# Regarding forcasting, ARIMA model does not catch the variation well, it just converges to
# a number in the near future. So, the more complicated model is needed to forecast.









