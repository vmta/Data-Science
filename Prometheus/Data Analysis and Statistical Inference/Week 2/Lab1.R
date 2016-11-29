#install.packages('dplyr', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)

#library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

#setwd("~/Documents/Education/Prometheus/Data Science/Week 2")

#flatsCSV <- read.csv("flats1", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ",")
#class(flatsCSV)
#str(flatsCSV)

flats <- read_excel("Lab1_flats.xls")
class(flats)
str(flats)

# Excersice
dim(flats)
head(flats, 6)
head(flats, 15)
tail(flats, 6)
names(flats)

# Box Plot
ggplot(flats, aes(group=City, x=City, y=Price)) + geom_boxplot() + coord_flip()
ggplot(flats, aes(group=Rooms, x=Rooms, y=Price)) + geom_boxplot() + coord_flip()
# Point Plot
ggplot(flats, aes(x=Area, y=Price)) + geom_point()
# Histogram Plot
ggplot(flats, aes(x=Price)) + geom_histogram(binwidth=50000, fill = "lightblue", col = "grey") + ylab('Qty')