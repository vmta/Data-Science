library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

setwd("~/Documents/workspace_r")

h <- diamonds
glimpse(h)

qplot(data=h, carat, price)
qplot(data=h, log(carat), log(price))
bg <- qplot(data=h, log(carat), log(price))
bg + geom_hex()

f <- read.csv("flats_moscow.txt", sep="\t", header = TRUE, dec=".")
qplot(data = f, totsp, price)
qplot(data = f, log(totsp), log(price))
#mosaicplot(data=f, f$brick+f$walk+f$floor, shade=TRUE)
f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)
qplot(data=f, log(price))
g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)

g2 + facet_grid(~f$floor)


# TESTING MODELS
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))
model_2b <- lm(data=f, log(price)~brick*log(totsp))

sjp.lm(model_2)


# Создадим новый набор данных для оценки прогнозирования
nw <- data.frame(totsp=c(60, 60), brick=factor(c(1, 0)))
predict(model_2, newdata=nw)

# т.к. в модели использован логарифм цены, то необходимо
# провести эспоненциирование для получения точечного прогноза
# в тыс.дол.
exp(predict(model_2, newdata=nw))

# построим доверительный интервал (95% по умолчанию)
exp(predict(model_2, newdata=nw, interval="confidence"))

# т.к. доверительный интервал интересен исключительно 
# в статистическом смысле, построим более интересный
# предиктивный интервал для оценки квартиры
exp(predict(model_2, newdata=nw, interval="prediction"))


# проведем F-тестирование трех моделей (0 модель - наиболее
# ограниченная, 1 - сложнее 0, но проще 2 и соответственно 0я
# проще, чем 2) т.е. проведем три теста и сравним модели:
# 0-1, 0-2, 1-2
#
# в тесте
# Res.Df - количество наблюдений (минус кол-во Df)
# Df - количество степеней свободы
# F - значние F статистики
# Pr(>F) - это P-value (если меньше "умолчания" 5%,
# то нулевая гипотеза отвергается и принимается алтернативная)
waldtest(model_0, model_1) # H_0 true model 0 rejected
waldtest(model_0, model_2) # H_0 true model 0 rejected
waldtest(model_1, model_2) # H_0 true model 0 rejected

gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + aes(col=brick) + stat_smooth(method="lm") + facet_grid(~walk)


#f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)

# Упрощенное сравнение регрессионных моделей методом сравнения
# AIC & BIC
# чем меньше трафной показатель, тем предпочтительнее модель!!!
mtable(model_0, model_1, model_2)


# Проведем еще один тест - на отсутствие в модели "нужных"
# переменных. Тест Рамсея.
# df1 - количество дополнительно включенных переменных
# p-value в данном случае 1%, что означет тот факт, что
# при проверке гипотезы о состоятельности нулевой гипотезы
# состоятельности модели - нулевая гипотеза будет отвергнута,
# хотя при 1% уровне доверия - нулевая гипотеза не будет
# отвергнута
resettest(model_2)
