library("dplyr")
library("ggplot2")
library("GGally")
library("psych")
library(gdata)
library(Metrics)

absolutePath <- "C:/my/econometrics/"
step = 50
M = 80
P = 8

source(paste(absolutePath, "function.R", sep = ""))
data = read.csv(paste(absolutePath, "data.csv", sep = ""))


#01.01.2004-31.12.2015
plot(data$value, type = "l")
plot(data$value[1:730], type = "l")
length(data$value)
#4382


#################################basePart#############################################

train <- data$value[1:round(length(data$value)*0.4)]
test <- data$value[(round(length(data$value)*0.4) + 1): round(length(data$value)*0.8)]
validation <- data$value[(round(length(data$value)*0.8) + 1): length(data$value)]
 
dataDelta <- GetDelta(data$value)
 
trainDelta <- dataDelta[1:round(length(dataDelta)*0.4)] #0:40%
testDelta <- dataDelta[(round(length(dataDelta)*0.4) + 1): round(length(dataDelta)*0.8)] #40:80%
validationDelta <- dataDelta[(round(length(dataDelta)*0.8) + 1): length(dataDelta)] #80:100%


plot(trainDelta[1:100], type = "l")

correlat <- c()
countPos <- c()
countPosAprox <- c()

for(i in 3:730)
{
  trainData <- GetTrainSample(trainDelta, i)
  approxSample <- GetAproxSample(trainDelta, i)
  
  
  similarIndex <- GetIndexStartMostSimilarSample(trainData, approxSample, 1)
  
  length(trainData[similarIndex: (similarIndex + i - 1)])
  
  newData <- trainData[similarIndex: (similarIndex + i - 1)]
  
  cor(approxSample, newData)
  
  plot(approxSample, type = 'l')
  plot(newData, type = 'l')
  
  sum(approxSample > 0)
  sum(newData > 0)
  
  
  correl = data.frame( value = GetCorel(trainData, approxSample, 1))
  max(correl$value)
  
  correlat[i] <- max(correl$value)
  countPos[i] <- sum(newData > 0)
  countPosAprox[i] <- sum(approxSample > 0)
  
}  

result <- data.frame(M = 1:730, corralation = correlat, 
                     countPositiveHistory = countPos, countPositiveAprox= countPosAprox)

write.csv(result, row.names=FALSE, file = paste(absolutePath, "result.csv", sep = ""))

#  
# countPositiveM <- GetCountPositive(approxSample)
# countNegativeM <- M - countPositiveM
# 
# countPositiveOffsetSample <- GetCountPositive(trainData[similarIndex: (similarIndex + M)])
# countNegativeOffsetSample <- M - countPositiveOffsetSample
# 
# if(countPositiveM != countPositiveOffsetSample)
# {
#   print("Warning : Incorrect size approximation and history data")
#   print(paste("approximation positive : ", toString (countPositiveM), sep = ""))
#   print(paste("history positive : ", toString (countPositiveOffsetSample), sep = ""))
# }
# 
# newData <- GetNewHistoryData(trainDelta, similarIndex, countPositiveM, countNegativeM)
# 
# trainDelta[similarIndex: (similarIndex + M)]




GetCorel <- function(trainData, approximationSample, step) {
  
  positionCor <- length(trainData) - length(approximationSample)
  
  maxCorrelation <- 0
  indexMaxCorrelation <- 0
  
  collerat <- c()
  k <- 1
  
  while(positionCor > 0)
  {
    correl <- cor(trainData[(positionCor + 1):(positionCor + length(approximationSample))],
                  approximationSample)
    collerat[k] <- correl
    if(correl > maxCorrelation)
    {
      maxCorrelation <- correl
      indexMaxCorrelation <- positionCor + 1
    }
    #print(correl)
    positionCor <- positionCor - step
    k <- k + 1
  }
  
  return(collerat)
}






# startPredictPoint <- GetStartPredictPoint(trainDelta, similarIndex, countPositiveM, countNegativeM)
# 
# dataForLmModel <- data.frame(M = approxSample, history = newData)
# 
# modelPos <- lm(M ~ history, data=subset(dataForLmModel, M >= 0))
# modelPos$coefficients[1] #offset
# modelPos$coefficients[2] #k
# 
# modelNeg <- lm(M ~ history, data=subset(dataForLmModel, M < 0))
# modelNeg$coefficients[1] #offset
# modelNeg$coefficients[2] #k
# 
# predictedValue = c(last(train))
# 
# j <- 2
# for( i in startPredictPoint:(startPredictPoint + P - 1))
# {
#   if(trainData[i] >= 0)
#   {
#     predictedValue[j] <- predictedValue[j - 1] +  trainData[i] * modelPos$coefficients[2] + modelPos$coefficients[1]
#   }
#   else
#   {
#     predictedValue[j] <- predictedValue[j - 1] +  trainData[i] * modelNeg$coefficients[2] + modelNeg$coefficients[1]
#   }
#   j <- j + 1
# }
# 
# predictedValue <- predictedValue[c(2:11)]
# 
# 
# plot(predictedValue, type = 'l')
# plot(test[1:10], type = 'l')
# 
# mae(predictedValue, test[1:10])



