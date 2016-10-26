library("dplyr")
library("ggplot2")
library("GGally")
library("psych")

#Задание:
#1) Определить средний коэффициент эластичности для каждого года
#2) Определить Бюджет за 3 года
#3) Построить функции спроса для каждого года
#4) Определить полезность для каждого года



data <- read.csv("C:/my/econometrics/labs/DataS.csv")

elasticity1 <- (data$DemandA[2:365] - data$DemandA[1:364]) / (data$PriceA[2:365] - data$PriceA[1:364])
elasticity2 <- (data$DemandB[2:365] - data$DemandB[1:364]) / (data$PriceB[2:365] - data$PriceB[1:364])
elasticity3 <- (data$DemandC[2:365] - data$DemandC[1:364]) / (data$PriceC[2:365] - data$PriceC[1:364])

meanElasticity1 <-  mean(elasticity1)
meanElasticity2 <- mean(elasticity2)
meanElasticity3 <-  mean(elasticity3)


budget <- sum(data$DemandA * data$PriceA) + sum(data$DemandB * data$PriceB) + sum(data$DemandC * data$PriceC)

plot(elasticity1, type = "l")
plot(elasticity2, type = "l")
plot(elasticity3, type = "l")

demand1 <- lm(DemandA~PriceA, data = data)
demand2 <- lm(DemandB~PriceB, data = data)
demand3 <- lm(DemandC~PriceC, data = data)
