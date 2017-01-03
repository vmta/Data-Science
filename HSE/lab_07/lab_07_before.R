# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

# lab 07

library("dplyr") # манипуляции с данными
library("erer") # расчет предельных эффектов
library("vcd") # графики для качественных данных
library("ggplot2") # графики
library("reshape2") # манипуляции с данными
library("AUC") # для ROC кривой

# при загрузке файлов R автоматом переделывает все строковые переменные в факторные
# эта шаманская команда просит R так не делать :)
options(stringsAsFactors = FALSE)

# читаем данные по пассажирам Титаника
t <- read.csv("titanic3.csv")
# источник и описание:
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html

t <- mutate(t,
            sex=as.factor(sex),
            pclass=as.factor(pclass),
            survived=as.factor(survived))

mosaic(data = t,
       ~ sex + pclass + survived,
       shade = TRUE)
qplot(data = t,
      x = survived, y = age,
      geom = "violin")
qplot(data = t,
      x = survived, y = age,
      geom = "boxplot")

qplot(data = t,
      x = age, y = ..count..,
      fill = survived,
      geom = "density",
      position = "stack")

qplot(data = t,
      x = age, y = ..count..,
      fill = survived,
      geom = "density",
      position = "fill")


# Оценивание моделей

m_logit <- glm(data = t, survived ~ sex + age + pclass + fare,
               family = binomial(link = "logit"),
               x = TRUE)
m_probit <- glm(data = t, survived ~ sex + age + pclass + fare,
               family = binomial(link = "probit"),
               x = TRUE)
summary(m_logit)
summary(m_probit)

# Ковариационная матрица
vcov(m_logit)


# Создадим новый набор данных? для которых будем
# прогнозировать...
newdata <- data.frame(
  age = seq(from = 5, to = 100, length = 100),
  sex = "male",
  pclass = "2nd",
  fare = 100
)
head(newdata)
# Строим прогноз
pr_logit <- predict(m_logit, newdata, se = TRUE)
# Объединяем newdata & pr_logit
newdata_pr <- cbind(newdata, pr_logit)

head(newdata_pr)

# Построение точеченого прогноза вероятности
# и доверительного интервала
newdata_pr <- mutate(newdata_pr,
                     prob = plogis(fit),
                     left_ci = plogis(fit - 1.96 * se.fit),
                     right_ci = plogis(fit + 1.96 * se.fit))
head(newdata_pr)

# Построение графика точечного прогноза вероятности
# и доверительного интервала
qplot(data = newdata_pr, x = age, y = prob, geom = "line")
+ geom_ribbon(aes(ymin = left_ci, ymax = right_ci), alpha = 0.2)

# Берем новый набор данных
t2 <- select(t, sex, age,  pclass, survived, fare) %>% na.omit()
# Строим новую логит-модель
m_logit <- glm(data = t2, survived ~ sex + age,
               family = binomial(link = "logit"), x = TRUE)
# Оцениваем LR
# lrtest(m_logit, m_logit2)
# requires library(erer) which depends on car, which depends on
# pbktest

# Оценка предельных эффектов
# maBina(m_logit) # среднее значение
# maBina(m_logit, x.mean = FALSE)
# requires library(erer) which depends on car, which depends on
# pbktest


# Попробуем провести оценку модели МНК
m_ols <- lm(data = t, as.numeric(survived) ~ sex + age + pclass + fare)
summary(m_ols)

pr_ols <- predict(m_ols, newdata)
head(pr_ols)

# CHOOSE ROC levels
pr_t <- predict(m_logit, t, se = TRUE)
t <- cbind(t, pr_t)
t <- mutate(t, prob = plogis(fit))
select(t, age, survived, prob)

roc.data <- roc(t$prob, t$survived)
str(roc.data)
qplot(x = roc.data$cutoffs, y = roc.data$tpr, geom = "line")
qplot(x = roc.data$cutoffs, y = roc.data$fpr, geom = "line")
qplot(x = roc.data$fpr, y = roc.data$tpr, geom = "line")
