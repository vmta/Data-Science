library("dplyr")
library("caret")
library("AER")
library("ggplot2")
library("sandwich")
library("ivpack")
library("memisc")

data("diamonds")
h <- diamonds
set.seed(12345)
train_ind <- createDataPartition(h$price, p=0.8, list=FALSE)
h_train <- h[train_ind,]
h_test <- h[-train_ind,]

model <- lm(log(price) ~ log(carat) + log(depth) + log(table) + clarity,
		data = h_train)

aprice <- h_test$price
aprice_hat <- predict(model, h_test)
sum(aprice - aprice_hat)/1000000000




data("CollegeDistance")
d <- CollegeDistance
glimpse(d)

model_iv <- ivreg(data = d, wage ~ region + gender + unemp + education
	| region + gender + unemp + distance)
coeftest(model_iv)
