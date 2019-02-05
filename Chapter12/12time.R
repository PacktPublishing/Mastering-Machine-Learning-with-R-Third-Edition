

install.packages("forecast")

set.seed(1966)

ar1 <- arima.sim(list(order = c(1, 0, 0), ar = 0.5), n = 200)

forecast::autoplot(ar1, main = "AR1")

forecast::autoplot(acf(ar1, plot = F), main = "ACF of simulated AR1")

forecast::autoplot(pacf(ar1, plot = F), main = "PACF of simulated AR1")

set.seed(123)

ma1 <- arima.sim(list(order = c(0, 0, 1), ma = -0.5), n = 200)

forecast::autoplot(ma1, main = "MA1")

forecast::autoplot(acf(ma1, plot = F), main = "ACF of simulated MA1")

forecast::autoplot(pacf(ma1, plot = F), main = "PACF of simulated MA1")

# Time Series Analysis ----------------------------------------------------

library(magrittr)
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("aod")
install.packages("tseries")
install.packages("vars")


climate <- readr::read_csv("climate.csv")

str(climate)

climate_ts <- ts(climate[, 2:3],
                 start = 1919,
                 end = 2013)  

plot(climate_ts, main = "CO2 and Temperature Deviation")

cor(climate_ts)

forecast::autoplot(acf(climate_ts[, 2], plot = F), main="Temperature ACF")

forecast::autoplot(pacf(climate_ts[, 2], plot = F), main = "Temperature PACF")

forecast::autoplot(acf(climate_ts[, 1], plot = F), main = "CO2 ACF")

forecast::autoplot(pacf(climate_ts[, 1], plot = F), main = "CO2 PACF")

forecast::autoplot(ccf(climate_ts[, 1], climate_ts[, 2], plot = F), main = "CCF")

tseries::adf.test(climate_ts[, 1])

temp_ts <- ts(climate$Temp, start = 1919, frequency = 1)
# 
# temp_struc <- strucchange::breakpoints(temp_ts ~ 1)
# summary(temp_struc)
# plot(temp_struc)

# forecast::autoplot(temp_struc)
#temperature <- window(temp_ts, start = 1919)

train <- window(temp_ts, end = 2007)
test <- window(temp_ts, start = 2008)

fit.ets <- forecast::ets(train)
fit.ets

forecast::autoplot(fit.ets)

plot(forecast::forecast(fit.ets, h = 6))

lines(test, type = "o")

fit.ets %>% forecast::forecast(h = 6) %>%
  forecast::accuracy(temp_ts)

# forecast::checkresiduals(fit.ets)

fit.arima <- forecast::auto.arima(train)
summary(fit.arima)
plot(forecast::forecast(fit.arima, h = 6))
lines(test, type = "o")

fit.arima %>% forecast::forecast(h = 6) %>%
  forecast::accuracy(temperature)

forecast::checkresiduals(fit.arima)

# ensemble ----------------------------------------------------------------
ETS <- forecast::forecast(forecast::ets(train), h = 6)
ARIMA <- forecast::forecast(forecast::auto.arima(train), h = 6)
NN <- forecast::forecast(forecast::nnetar(train), h = 6)

ensemble.fit <-
  (ETS[["mean"]] + ARIMA[["mean"]] + NN[["mean"]]) / 3

c(ets = forecast::accuracy(ETS, temperature)["Test set", c("Theil's U")],
  arima = forecast::accuracy(ARIMA, temperature)["Test set", c("Theil's U")],
  nn = forecast::accuracy(NN, temperature)["Test set", c("Theil's U")],
  ef = forecast::accuracy(ensemble.fit, temperature)["Test set", c("Theil's U")])

plot(NN)
lines(test, type = "o")

nn.fit <- forecast::nnetar(train)
forecast::checkresiduals(nn.fit)

temp_struc <- strucchange::breakpoints(temp_ts ~ 1)
summary(temp_struc)

train_bp <- window(temp_ts, start = 1963, end = 2007)

fit.arima2 <- forecast::auto.arima(train_bp)
#summary(fit.arima2)
#plot(forecast::forecast(fit.arima2, h = 6))
#lines(test, type = "o")

fit.arima2 %>% forecast::forecast(h = 6) %>%
  forecast::accuracy(temperature)

forecast::checkresiduals(fit.arima2)
# VAR ---------------------------------------------------------------------

fit.lm <- lm(Temp ~ CO2, data = climate)

summary(fit.lm)

forecast::checkresiduals(fit.lm)

climate49 <- window(climate_ts, start = 1949)

forecast::ndiffs(climate49[, 1], test = "adf")

forecast::ndiffs(climate49[, 2], test = "adf")

climate_diff <- diff(climate49)
lag.select <- vars::VARselect(climate_diff, lag.max = 12)

lag.select$selection

fit1 <- vars::VAR(climate_diff, p = 5)

summary(fit1)
vars::serial.test(fit1, type = "PT.asymptotic")

x2y <- vars::causality(fit1, cause = "CO2")
y2x <- vars::causality(fit1, cause = "Temp")
x2y$Granger
y2x$Granger

level.select <- vars::VARselect(climate49, lag.max = 12)
level.select$selection


fit2 <- vars::VAR(climate49, p = 7)
vars::serial.test(fit2, type = "PT.asymptotic")
CO2terms <- seq(1, 11, 2)
Tempterms <- seq(2, 12, 2)
aod::wald.test(
  b = coef(fit2$varresult$Temp),
  Sigma = vcov(fit2$varresult$Temp),
  Terms = c(CO2terms)
) 

aod::wald.test(
  b = coef(fit2$varresult$CO2),
  Sigma = vcov(fit2$varresult$CO2),
  Terms = c(Tempterms)
) 

plot(predict(fit2, n.ahead = 24, ci = 0.95))





