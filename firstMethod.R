library("dplyr")
library("ggplot2")
library("GGally")
library("psych")
library(gdata)
library(Metrics)


step = 10
M = 30
P = 10

data = read.csv("C:/my/econometrics/data.csv")

#01.01.2004-31.12.2015
plot(data$value, type = "l")
plot(data$value[1:730], type = "l")
length(data$value)
#4382

cor <- acf(data$value, lag.max = 4382)

GetDelta <- function(data)
{
  result <- c()
  for (i in 1:(length(data) - 1))
  {
    result[i] <- data[i+1] - data[i]
  }
  return(result)
}

GetAproxSample <- function(trainData, M)
{
  return(trainData[(length(trainData) - M + 1):length(trainData)])
}
GetTrainSample <- function(trainData, M)
{
  return(trainData[1:(length(trainData) - M)])
}

GetIndexStartMostSimilarSample <- function(trainData, approximationSample, step) {
  
  positionCor <- length(trainData) - length(approximationSample)
  
  maxCorrelation <- 0
  indexMaxCorrelation <- 0
  
  while(positionCor > 0)
  {
    correl <- cor(trainData[(positionCor + 1):(positionCor + length(approximationSample))],
                  approximationSample)
    if(correl > maxCorrelation)
    {
      maxCorrelation <- correl
      indexMaxCorrelation <- positionCor
    }
    #print(correl)
    positionCor <- positionCor - step
  }
  
  return(indexMaxCorrelation)
}

GetCountPositive <- function(data)
{
  return(sum(data >= 0))
}


GetNewHistoryData <- function(trainData, index, countPositive, countNegative)
{
  indexPos <- 0
  indexNeg <-  0
  newData <-  c()
  i <-  1
  j <- 0
  while((indexPos < countPositive) | (indexNeg < countNegative))
  {
    if((trainData[index] >= 0) & (indexPos < countPositive)) 
    {
      newData[i] <-  trainData[index]
      indexPos <- indexPos + 1
      i <-  i + 1
    }
    if((trainData[index] < 0) & (indexNeg < countNegative)) 
    {
      newData[i] <-  trainData[index]
      indexNeg <- indexNeg + 1
      i <-  i + 1
    }
    index <- index + 1
  }
  return(newData)
}

GetStartPredictPoint <- function(trainData, index, countPositive, countNegative)
{
  indexPos <- 0
  indexNeg <-  0
  newData <-  c()
  i <-  1
  j <- 0
  while((indexPos < countPositive) | (indexNeg < countNegative))
  {
    if((trainData[index] >= 0) & (indexPos < countPositive)) 
    {
      newData[i] <-  trainData[index]
      indexPos <- indexPos + 1
      i <-  i + 1
    }
    if((trainData[index] < 0) & (indexNeg < countNegative)) 
    {
      newData[i] <-  trainData[index]
      indexNeg <- indexNeg + 1
      i <-  i + 1
    }
    index <- index + 1
  }
  return(index + 1)
}

dataDelta <- GetDelta(data$value)

train <- data$value[1:round(length(data$value)*0.4)]
test <- data$value[(round(length(data$value)*0.4) + 1): round(length(data$value)*0.8)]

trainDelta <- dataDelta[1:round(length(data$value)*0.4)] #0:40%
testDelta <- dataDelta[(round(length(data$value)*0.4) + 1): round(length(data$value)*0.8)] #40:80%
validationDelta <- dataDelta[round(length(data$value)*0.8): length(data$value)] #80:100%



plot(trainDelta[1:100], type = "l")



trainData <- GetTrainSample(trainDelta, M)
approxSample <- GetAproxSample(trainDelta, M)
length(trainData)


similarIndex <- GetIndexStartMostSimilarSample(trainData, approxSample, 5)

plot(approxSample, type = 'l')
plot(trainData[similarIndex: (similarIndex + M)], type = 'l')

countPositiveM <- GetCountPositive(approxSample)
countNegativeM <- M - countPositiveM

countPositiveOffsetSample <- GetCountPositive(trainData[similarIndex: (similarIndex + M - 1)])
countNegativeOffsetSample <- M - countPositiveOffsetSample

if(countPositiveM != countPositiveOffsetSample)
{
  print("Warning : Incorrect size approximation and history data")
  print(paste("approximation positive : ", toString (countPositiveM), sep = ""))
  print(paste("history positive : ", toString (countPositiveOffsetSample), sep = ""))
}

newData <- GetNewHistoryData(trainDelta, similarIndex, countPositiveM, countNegativeM)
startPredictPoint <- GetStartPredictPoint(trainDelta, similarIndex, countPositiveM, countNegativeM)

dataForLmModel <- data.frame(M = approxSample, history = newData)

modelPos <- lm(M ~ history, data=subset(dataForLmModel, M >= 0))
modelPos$coefficients[1] #offset
modelPos$coefficients[2] #k

modelNeg <- lm(M ~ history, data=subset(dataForLmModel, M < 0))
modelNeg$coefficients[1] #offset
modelNeg$coefficients[2] #k

predictedValue = c(last(train))

j <- 2
for( i in startPredictPoint:(startPredictPoint + P - 1))
{
  if(trainData[i] >= 0)
  {
    predictedValue[j] <- predictedValue[j - 1] +  trainData[i] * modelPos$coefficients[2] + modelPos$coefficients[1]
  }
  else
  {
    predictedValue[j] <- predictedValue[j - 1] +  trainData[i] * modelNeg$coefficients[2] + modelNeg$coefficients[1]
  }
  j <- j + 1
}

predictedValue <- predictedValue[c(2:11)]


plot(predictedValue, type = 'l')
plot(test[1:10], type = 'l')

mae(predictedValue, test[1:10])

