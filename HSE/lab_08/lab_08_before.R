# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 08

library("lubridate") # работа с датами

library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("forecast")

library("quantmod") # загрузка с finance.google.com
library("sophisthse") # загрузка с sophist.hse.ru


# Искусственно сгенерированные стационарные процессы
# 1. Autoregression process
y <- arima.sim(n=100, list(ar=0.7))
plot(y)
Acf(y)
Pacf(y)
tsdisplay(y)
# 2. Moving Average process
y <- arima.sim(n=100, list(ma=-0.8))
tsdisplay(y)
# 3. ARMA(1, 1)
y <- arima.sim(n=100, list(ma=-0.8, ar=0.5))
tsdisplay(y)
y <- arima.sim(n=100, list(ma=-0.8, ar=-0.5))
tsdisplay(y)

# Искусственно сгенерированные нестационарные процессы
# 1. Блуждающий процесс (белый шум)
y <- arima.sim(n=100, list(order=c(0,1,0)))
tsdisplay(y)
# 2. Процесс с трендом
y <- seq(0, 10, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

# ПРИМЕР 1
# Анализ реальных временных рядов на примере уровня озера Гурон
y <- LakeHuron
tsdisplay(y)
# (По графикам ACF, PACF невозможно однозначно определить
# является ли процесс стационарным или нет - признаками служат
# для графика ACF - первые 2-3 коэффициента по модулю больше 0,
# и после резкого падения примерно равны 0, в то время как для
# PACF графика ожидается значение первого коэффициента по модулю
# больше нуля, а все остальные примерно равны нулю.)
mod_1 <- Arima(y, order=c(2,0,0))
mod_2 <- Arima(y, order=c(1,0,1)) # классическая ARMA модель
summary(mod_1)
summary(mod_2)
# Коэффициенты значимы в обеих моделях, но посчитаем коэффициент
# AKAIKE
AIC(mod_1)
AIC(mod_2)
# Рассморти еще модель в которой будет 2 лага по AR и 1 по MA
mod_3 <- Arima(y, order=c(2,0,1))
AIC(mod_3) # Модель стала "хуже" т.к. АКАИКЕ значение выше
# Модели позволяют строить прогнозы
# Попробуем сделать прогноз
prognoz <- forecast(mod_2, h=5) # h=5 - 5 years
prognoz
plot(prognoz)
# Предположим, что ряд нестационарен, построим еще одну модель
# AR(1) но будем рассматривать не yt, а дельту от yt
mod_4 <- Arima(y, order=c(1,1,0))
AIC(mod_4)
prognoz <- forecast(mod_4, h=5)
plot(prognoz)
# Можно автоматически создать (отобрать) лучшую модель, чтобы не
# сравнить каждый раз параметр АКАИКЕ
mod_a <- auto.arima(y)
summary(mod_a)
# Отобразим прогноз по новой авто модели на графике
prognoz_a <- forecast(mod_a, h=5)
plot(prognoz_a)

# ПРИМЕР 2
# Анализ стоимости акций компании Гугл
# и численности населения России
Sys.setlocale("LC_TIME", "C")
# Загружаем данные за 11 месяцев 2014 года по акциям Гугл
getSymbols(Symbols = "GOOG", from = "2014-01-01", to = "2014-12-01")
head(GOOG)
# Выбираем цены закрытия (формируем одномерный временной ряд)
y <- GOOG$GOOG.Close
tsdisplay(y)
# Наблюдаем типичный блуждающий график (т.е. процесс не стационарен)
# вручную возьмем дельта от У
dy <- diff(y)
# Перейдем к дельта У и построим график
tsdisplay(dy)
