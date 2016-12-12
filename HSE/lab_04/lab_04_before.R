# ESLI RUSSKIE BUKVI NE VIDNI --->
# File -- Reopen with encoding --- utf8 --- set as default --- ok


library("HSAUR") # из этого пакета возьмем набор данных по семиборью
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тесты для линейных моделей
library("glmnet") # LASSO + ridge
library("ggplot2") # графики
library("car") # vif

h<-cars
model<-lm(data=h, dist~speed)
h <- mutate(h, speed2 = speed^2, speed3 = speed^3)
model_mk<-lm(data=h, dist~speed+speed2+speed3)

nd<-data.frame(speed=10, speed2=100, speed3=1000)

predict(model, newdata = nd, interval = "prediction")
predict(model_mk, newdata = nd, interval = "prediction")

confint(model)
confint(model_mk)


# Ридже и ЛАССО
y <- h$dist
x0 <- model.matrix(data=h, dist~0+speed+speed2+speed3)
lambdas <- seq(50,0.1,length=30)

# LASSO
m_lasso <- glmnet(x0,y,alpha=1,lambda=lambdas)

plot(m_lasso, xvar = "lambda", label = TRUE)
plot(m_lasso, xvar = "dev", label = TRUE)
plot(m_lasso, xvar = "norm", label = TRUE)

coef(m_lasso,s=c(0.1,1))

# RIDGE
m_ridge <- glmnet(x0,y,alpha=0,lambda=lambdas)

plot(m_ridge, xvar = "lambda", label = TRUE)
plot(m_ridge, xvar = "dev", label = TRUE)
plot(m_ridge, xvar = "norm", label = TRUE)

coef(m_ridge,s=c(0.1,1))

# cross validation
cv <- cv.glmnet(x0,y,alpha=1)
plot(cv)

cv$lambda.min
cv$lambda.1se

coef(cv, s="lambda.1se")


# PCA (Метод главних компонент)
h <- heptathlon
#glimpse(h)
h <- select(h, -score)
#describe(h)
#cor(h)
h.pca <- prcomp(h, scale = TRUE)

# витащим первую главную компоненту
pca1 <- h.pca$x[,1]
# веса первой главной компоненти
v1 <- h.pca$rotation[,1]

cor(heptathlon$score, pca1)
plot(h.pca)
biplot(h.pca, xlim=c(-1,1))
