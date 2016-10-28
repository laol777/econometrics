library("dplyr")
library("ggplot2")
library("GGally")
library("psych")
library(gdata)
library(Metrics)
library(tseries)


#########################################baseSetting#################################

absolutePath <- "C:/my/econometrics/solution/"
nameDataFrame = "data.csv"
step = 1 #|__M__|___   -> __|__M__|_ step movement M sample 
M = 40
P = 15

#______train______|a|_______test___________|b|___________validation________
a <- 0.51
b <- 0.95
####################################################################################


source(paste(absolutePath, "function.R", sep = ""))
data = read.csv(paste(absolutePath, nameDataFrame, sep = ""))


#01.01.2004-31.12.2015
#plot(data$value, type = "l")
#plot(data$value[1:730], type = "l")
#length(data$value)
#4382


#################################basePart#############################################

train <- data$value[1:round(length(data$value)*a)]
test <- data$value[(round(length(data$value)*a) + 1): round(length(data$value)*b)]
validation <- data$value[(round(length(data$value)*b) + 1): length(data$value)]
 
dataDelta <- GetDelta(data$value)
 
trainDelta <- dataDelta[1:round(length(dataDelta)*a)]
testDelta <- dataDelta[(round(length(dataDelta)*a) + 1): round(length(dataDelta)*b)]
validationDelta <- dataDelta[(round(length(dataDelta)*b) + 1): length(dataDelta)]


matchingIndex <- EnumerableDeltaData(trainDelta)

resDelta <- GetPredictForSeparateDelta(trainDelta, M, P, step)
pred <- last(train) + resDelta



x  <- c(1:P)
y1 <- pred
y2 <- test[1:P]
df <- data.frame(x,y1,y2)

ggplot(df) +                  
  geom_line(aes(y=y1, x = x, color='Предсказанное' ),  size=1) +
  geom_line(aes(y=y2, x = x, color='Тестовое' ),size=1)



print(paste("MAE: ", toString(abs(sum(y1 - y2)) / length(y1)), sep = ""))

print(resDelta)
