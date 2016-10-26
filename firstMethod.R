library("dplyr")
library("ggplot2")
library("GGally")
library("psych")
library(gdata)
library(Metrics)
library(tseries)


#########################################baseSetting#################################

absolutePath <- "C:/my/econometrics/"
nameDataFrame = "data.csv"
step = 1 #|__M__|___   -> __|__M__|_ step movement M sample 
M = 20
P = 8

#______train______|a|_______test___________|b|___________validation________
a <- 0.4
b <- 0.8
####################################################################################


source(paste(absolutePath, "function.R", sep = ""))
data = read.csv(paste(absolutePath, nameDataFrame, sep = ""))


#01.01.2004-31.12.2015
plot(data$value, type = "l")
plot(data$value[1:730], type = "l")
length(data$value)
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


positiveDelta <- trainDelta[trainDelta >= 0]

trainData <- GetTrainSample(positiveDelta, M)
MSample <- GetAproxSample(positiveDelta, M)

posInd <- GetIndexStartMostSimilarSample(trainData, MSample, step)
posLmModel <- lm(trainData[posInd: (posInd + M - 1)] ~ MSample)
kPos <- posLmModel$coefficients[[2]]
offsetPos <- posLmModel$coefficients[[1]]

result <- c(1:P)

for( i in c(1:P) )
{
  tmpIndex <- matchingIndex$index[ matchingIndex$positive == posInd ]
  predIndex <- matchingIndex$positive[ ( matchingIndex$index > tmpIndex ) 
                                       & ( matchingIndex$index <= (tmpIndex + i) ) 
                                       & ( matchingIndex$positive != 0 )  ]
  
  predictPositiveDelta <- sum(positiveDelta[predIndex])
  result[i] <- predictPositiveDelta
}

GetPredictForSeparateDelta(trainDelta, M, P, step)

#adf.test(trainDelta[1:5], k=1)

# correlat <- c()
# countPos <- c()
# countPosAprox <- c()
# 
# for(i in 3:730)
# {
#   trainData <- GetTrainSample(trainDelta, i)
#   approxSample <- GetAproxSample(trainDelta, i)
#   
#   
#   similarIndex <- GetIndexStartMostSimilarSample(trainData, approxSample, 1)
#   
#   length(trainData[similarIndex: (similarIndex + i - 1)])
#   
#   newData <- trainData[similarIndex: (similarIndex + i - 1)]
#   
#   cor(approxSample, newData)
#   
#   plot(approxSample, type = 'l')
#   plot(newData, type = 'l')
#   
#   sum(approxSample > 0)
#   sum(newData > 0)
#   
#   
#   correl = data.frame( value = GetCorel(trainData, approxSample, 1))
#   max(correl$value)
#   
#   correlat[i] <- max(correl$value)
#   countPos[i] <- sum(newData > 0)
#   countPosAprox[i] <- sum(approxSample > 0)
#   
# }  
# 
# result <- data.frame(M = 1:730, corralation = correlat, 
#                      countPositiveHistory = countPos, countPositiveAprox= countPosAprox)
# 
# write.csv(result, row.names=FALSE, file = paste(absolutePath, "result.csv", sep = ""))


#countPositiveM <- GetCountPositive(MSample)
#countNegativeM <- M - countPositiveM

#countPositiveP <- GetCountPositive(testDelta[1:P])
#countNegativeP <- P - countPositiveP

#countPositiveOffsetSample <- GetCountPositive(trainData[similarIndex: (similarIndex + M)])
#countNegativeOffsetSample <- M - countPositiveOffsetSample

#if(countPositiveM != countPositiveOffsetSample)
# {
#   print("Warning : Incorrect size approximation and history data")
#   print(paste("approximation positive : ", toString (countPositiveM), sep = ""))
#   print(paste("history positive : ", toString (countPositiveOffsetSample), sep = ""))
# }

#newData <- GetNewHistoryData(trainDelta, similarIndex, countPositiveM, countNegativeM)

#trainDelta[similarIndex: (similarIndex + M)]




