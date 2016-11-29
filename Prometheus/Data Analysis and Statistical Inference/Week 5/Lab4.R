library(lubridate)
library(dplyr)
library(ggplot2)

setwd("~/Documents/Education/Prometheus/Data Science/Week 5")
crime <- read.csv("crimes.csv", header = TRUE)
moon <- read.csv("moon.csv", header = TRUE)

crime$POSIX <- ymd_hms(as.character(crime$Dates))
crime$Dates <- as.Date(ymd_hms(as.character(crime$Dates)))
moon$date <- as.Date(moon$date, "%m/%d/%Y")
#full_data <- merge(crime, moon, by.x = "Dates", by.y = "date")
full_data <- inner_join(crime, moon, by = c("Dates" = "date"))
date_phase <- full_data %>%
  group_by(Dates, phase) %>%
  count() %>%
  arrange(desc(n))
#glimpse(date_phase)
ggplot(date_phase, aes(Dates, n)) +
  geom_line(alpha = 0.5) +
  labs(title = "Злочини в Сан-Франциско (2003-2015)",
       x = "Дата",
       y = "Кількість злочинів") +
  geom_point(data = date_phase[date_phase$phase == "Full Moon", ],
             color = "red") +
  geom_smooth()
# Знайдемо середнє значення в фазі повного місяця
# x <- mean(date_phase$n[date_phase$phase == "Full Moon"])
# Знайдемо середнє значення в решті фаз крім повного місяця
# mu <- mean(date_phase$n[date_phase$phase != "Full Moon"])
# Знайдемо розмір вибірки
# n <- length(date_phase$n[date_phase$phase == "Full Moon"])
# Знайдемо середньоквадратичне відхилення вибірки
# s <- sd(date_phase$n[date_phase$phase == "Full Moon"])
# Обчислимо Р-значення для
#   t = (x - mu)/(s/sqrt(n)) (=0.839)
#   df = n - 1 (=76)
# p_val <- 2*pt(0.839, df = 76, lower.tail = FALSE) (=0.4041006)
# Враховуючи, що альфа = 0,05 (5%), а Р-значення відповідно
# 40,41006%, побудуємо довірчий інтервал для нульової гіпотези
x_vector <- date_phase$n[date_phase$phase == "Full Moon"]
t.test(x_vector, mu = 391.75, alternative = "two.sided", conf.level = 0.95)

# Самостійно дослідити питання впливу дня тижня на
# кількість злочинів

day_of_week_crimes <- full_data %>%
  group_by(DayOfWeek) %>%
  count()
ggplot(data=day_of_week_crimes, aes(x=DayOfWeek, y=n)) +
  geom_bar(stat="identity", fill="lightblue")
# Змінимо порядок слідування днів на звичний
day_of_week_crimes$DayOfWeek <- factor(day_of_week_crimes$DayOfWeek,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
             "Saturday", "Sunday"))
ggplot(data=day_of_week_crimes, aes(x=DayOfWeek, y=n)) +
  geom_bar(stat="identity", fill="lightblue")

# Перевіримо чи середня кількість злочинів, скоєних по п'ятницях
# відрізняється від середнього значення 391.75. Встановимо
# альфа = 99% (0.01)
crimes_by_day <- full_data %>%
  group_by(Dates, DayOfWeek) %>%
  count()
sample_vector <- crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"]
# Обчислимо середнє значення та середньоквадратичне відхилення
# для вибірки:
x <- mean(crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"])
s <- sd(crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"])
p_val <- 2*pt(4.4, df = 45, lower.tail = FALSE)