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
library("lattice")


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
# 
# Прогнозируем...
# 
# Сначала отберем реальные данные из тестовой выборки (т.е. фактически
# наблюдаемые переменные, которые мы попытаемся спрогнозировать исполь-
# зуя модели линейной регрессии и случайного леса).
y <- f_test$price
# Спрогнозируем наблюдаемые переменные моделью линейной регрессии.
y_hat_lm <- predict(model_lm, f_test)
# Спрогнозируем наблюдаемые переменные моделью случайного леса.
y_hat_rf <- predict(model_rf, f_test)
# 
# Оценим RSS (сумму квадратов остатков) прогнозных значений
rss_lm <- sum((y - y_hat_lm)^2)
rss_lm
rss_rf <- sum((y - y_hat_rf)^2)
rss_rf

# Bayesian approach

# Создадим плохой набор данных, в котором для логит-модели оценки коэфф.
# отсутствуют.
bad <- data.frame(y=c(0,0,1), x=c(1,2,3))
bad
model_logit <- glm(data=bad, y~x, family=binomial(link="logit"))
summary(model_logit)

# prior: beta ~ N(0, 50^2)
model_mcmc_logit <- MCMClogit(data=bad, y~x, b0=0, B0=1/50^2)
summary(model_mcmc_logit)

# Spike and Slab regression
# Возьмем исходный набор данных по автомобилям и несколько его видоизменим
h <- mutate(cars, speed=1.61*speed, dist=0.3*dist)
h$junk <- rnorm(nrow(h))
h <- mutate(h, speed2=speed^2)
# Оценим модель линейной регрессии
model_lm <- lm(data=h, dist~speed+junk)
summary(model_lm)
# Оценим модель Spike-and-Slab
model_ss <- spikeslab(data=h, dist~speed+junk, n.iter2 = 4000)
print(model_ss)
model_ss$summary
# 
included_regressors <- melt(model_ss$model)
included_regressors
# 
# (Переменная 1 - скорость включалась во все наблюдения и с
# вероятностью 1 * 100% она влияет на результат - на длину тормозного пути)
sum(included_regressors$value==1)/4000
# (Переменная 2 - мусор(junk, искусственно созданный и включенный нами в
# набор данных) включалась не во все наблюдения и с вероятностью
# 0.409 * 100% она влияет на результат - на длину тормозного пути)
sum(included_regressors$value==2)/4000
