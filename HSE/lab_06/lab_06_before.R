# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("lubridate") # работа с датами

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

library("quantmod") # загрузка с finance.google.com
library("rusquant") # загрузка с finam.ru
library("sophisthse") # загрузка с sophist.hse.ru
library("Quandl") # загрузка с Quandl



# Dates
x <- c("2012-04-15", "2013-11-05")
y <- ymd(x)
y + days(20)
y - years(10)
year(y)

# 
x <- rnorm(5)
y <- ymd("2014-01-01") + days(0:4)
ts <- zoo(x, order.by=y) # построение объекта с данными
                         # привязанными к дате
lag(ts, -1) # (показатели на предыдущий день)
lag(ts, 1) # (показатели на будущий день)
diff(ts) # разница значений между предыдущей и настоящей датой


# Квартальный регулярный ряд
ts2 <- zooreg(x, start = as.yearqtr("2014-01"), freq = 4)

# Ежемесячный регулярный ряд
ts3 <- zooreg(x, start = as.yearmon("2014-01"), freq = 12)


data("Investment")
start(Investment)
end(Investment)
time(Investment)

## Количество данных во временном ряду
#max(time(Investment))-min(time(Investment)-1)
#time(Investment)[length(time(Investment))]-time(Investment)[1]+1
#nrow(Investment)

coredata(Investment) # Core Data, no Time association

# Создадим новый набор с искуственно обнуленными данными
dna <- Investment
dna[1, 2] <- NA
dna[5, 3] <- NA
# Два метода заполнения НА полей
# 1 - аппроксимация усреднением соседних значений
# 2 - копирование предыдущего значения
na.approx(dna)
na.locf(dna)

# загрузка данных HSE
hse <- sophisthse("POPNUM_Y")

# загрузка данных с Quandl
b <- Quandl("FRED/GNP")

# finance.google.com
# Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "AAPL", from = "2016-01-01",
           to = "2016-12-14", src = "google")

getSymbols(Symbols = "GOOG", from = "2016-01-01",
           to = "2016-12-14", src = "yahoo")

#getSymbols(Symbols = "GAZP", from = "2016-01-01",
#           to = "2016-12-14", src = "Finam")

# построение простых графиков временных рядов
plot(AAPL)
autoplot(AAPL[,1:4])
autoplot(AAPL[,1:4], facets = NULL)
chartSeries(AAPL)

# Построение робастных доверительных интервалов!!!
d <- as.zoo(Investment)
autoplot(d[, 1:2], facets = NULL)

model <- lm(data = d, RealInv ~ RealInt + RealGNP)

summary(model)
coeftest(model)
confint(model)

d_aug <- augment(model, as.data.frame(d))
glimpse(d_aug)
qplot(data = d_aug, lag(.resid), .resid)

vcov(model)
vcovHAC(model)

conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate = conftable[,1],
                 se_ac = conftable[,2])
ci <- mutate(ci,
             left_95 = estimate - 1.96 * se_ac,
             right_95 = estimate + 1.96 * se_ac)

# Durbin-Watson TEST
# H0 - no autocorrelation
# H1 - is autocorrelated
dwtest(model)
res <- dwtest(model)
res$statistic
res$p.value
res$alternative

# Breush-Godfree test
# Ho - автокорреляции нет
# Ha - автокорреляция k-го порядка
res <- bgtest(model, order = 2)
res$statistic
res$p.value
