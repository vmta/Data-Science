# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

# lab 10

library("spikeslab") # регрессия пик-плато
library("ggplot2") # графики
library("dplyr") # манипуляции с данными
library("reshape2") # перевод таблиц: широкая-длинная
library("MCMCpack") # байесовский подход к популярным моделям
library("quantreg") # квантильная регрессия
library("randomForest") # случайный лес
library("rattle") # визуализация деревьев
library("caret") # стандартный подход к регрессионным и классификационным задачам
library("rpart") # классификационные и регрессионные деревья


f <- read.table("flats_moscow.txt", header = TRUE, sep = "\t", dec = ".")
glimpse(f)

# Оценим модель сразу для 3х квантилей - 10%, 50%, 90% (т.е. для
# дешевых квартир, для медианного квантиля и для дорогих квартир)
model_q01 <- rq(data = f, price ~ totsp, tau = c(0.1, 0.5, 0.9))
summary(model_q01)

# Построим визуализацию
base <- qplot(data = f, totsp, price)

# Отобразим квантильные линии для дешевых и дорогих квартир
base_q <- base +
  stat_smooth(method="rq", tau=0.1, se=FALSE) +
  stat_smooth(method="rq", tau=0.9, se=FALSE)

# Цветом отобразим кирпичность дома
base_q + aes(colour=factor(brick))


# Построим две модели
# 
# - Линейная регрессия
# - Регрессия случайного леса
# 
# Сравним пронозные возможности двух моделей. Чтобы сравнение было
# "честным", предварительно разделим выборку.
# 
# Отбираем (номера) 75% из общей выборки
in_sample <- createDataPartition(f$price, p=0.75, list=FALSE)
# Отбираем в тренировочную выборку (по номерам) 75% наблюдений из
# общей выборки
f_train <- f[in_sample,]
# Наблюдения, не попавшие в тренировочную выборку отбираем в 
# тестовую выборку
f_test <- f[-in_sample,]
# Оценим модель линейной регрессии
model_lm <- lm(data=f_train, price~totsp+kitsp+livesp+brick)
# Оценим модель случайного леса
model_rf <- randomForest(data=f_train, price~totsp+kitsp+livesp+brick)