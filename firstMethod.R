library("dplyr")
library("ggplot2")
library("GGally")
library("psych")
library(gdata)


step = 10
M = 30
k = 0

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

dataDelta <- GetDelta(data$value)

trainDelta <- dataDelta[1:round(length(data$value)*0.4)] #0:40%
testDelta <- dataDelta[(round(length(data$value)*0.4) + 1): round(length(data$value)*0.8)] #40:80%
validationDelta <- dataDelta[round(length(data$value)*0.8): length(data$value)] #80:100%



plot(trainDelta[1:100], type = "l")


#1
trainData <- GetTrainSample(trainDelta, M)
approxSample <- GetAproxSample(trainDelta, M)
length(trainData)

#3
similarIndex <- GetIndexStartMostSimilarSample(trainData, approxSample, 5)


