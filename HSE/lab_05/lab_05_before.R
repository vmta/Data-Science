# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

h <- read.table("flats_moscow.txt", header = TRUE)
qplot(data=h, x=totsp, y=price)

# Не саміе корректніе формули т.к. имеется условная
# гетероскедастичность
model <- lm(data=h, price ~ totsp)
summary(model)
coeftest(model)
confint(model)
vcov(model)

# Проверим гетероскедастичность
h <- augment(model, h)
qplot(data=h, totsp, abs(.resid))
vcovHC(model)
conftable <- coeftest(model, vcovHC(model))
confidenceInterval <- data.frame(estimate=conftable[,1],
                                 se_hc=conftable[,2])
confidenceInterval <- mutate(confidenceInterval,
                             left_ci=estimate-1.96*se_hc,
                             right_ci=estimate+1.96*se_hc)

confidenceInterval

# Тесті на гетероскедастичность

# Тест Уайта
bptest(model)
bptest(model, data = h, varformula = ~ totsp + I(totsp^2))
bptest(model, data = h, varformula = ~ poly(totsp, 2))

# Голдфельда-Квандта
gqtest(model, order.by = ~totsp, data = h, fraction = 0.2)

# смотрим через логарифмирование
qplot(data=h, log(totsp), log(price))
model2 <- lm(data=h, log(price)~log(totsp))

# Еще раз тестируем уже с прологарифмированой моделью
gqtest(model2, order.by = ~totsp, data = h, fraction = 0.2)

