Sys.setlocale("LC_ALL", "UTF-8")
setwd("Y:/Education/Library/Data-Science/HSE/lab_07")

library("dplyr")
library("erer")
library("vcd")
library("ggplot2")
library("reshape2")
library("AUC")
library("lmtest")
library("broom")

options(stringsAsFactors = FALSE)
t <- read.csv("titanic3.csv")

m_logit <- glm(data = t, survived ~ age + I(age^2) + sex
               + fare + sibsp,
               family = binomial(link = "logit"),
               x = TRUE)
summary(m_logit)
confit(m_logit, level=0.95)

nd <- data.frame(age=30, sex="male", sibsp=2, fare=200)
predict(m_logit, newdata = nd, type="response", se.fit=TRUE)

m_logit <- glm(data = t, survived ~ age + I(age^2) + sex + fare + sibsp,
               family = binomial(link = "logit"), x = TRUE)
nd <- data.frame(age=50, sex="male", sibsp=2, fare=200)
nd_pr <- predict(m_logit, newdata=nd, type = "response", se.fit=TRUE)
nd_pr$fit - nd_pr$se.fit


# 17
maBina(m_logit)

# 18
d <- select(t, age, sibsp, sex, fare, parch, survived)
# d_clean <- na.omit(d)
d <- d %>% na.omit()
m_logit1 <- glm(data = d, survived ~ age + I(age^2) + sex + fare + I(fare^2) + sibsp,
		family = binomial(link="logit"), x= TRUE)
m_logit2 <- glm(data = d, survived ~ age + I(age^2) + sex + sibsp,
		family = binomial(link="logit"), x = TRUE)
lrtest(m_logit2, m_logit1)

# 19
d <- select(t, age, sibsp, sex, fare, survived)
d <- na.omit(d)
m_logit <- glm(data = d, survived ~ age + I(age^2) + sex + fare + sibsp,
		family = binomial(link = "logit"), x = TRUE)
d$probs <- predict(m_logit, type = "response")
glimpse(d)

# 20
m_logit <- glm(data = t, survived ~ age + I(age^2) + sex + fare + sibsp,
		family = binomial(link = "logit"), x = TRUE)
vcov(m_logit)
