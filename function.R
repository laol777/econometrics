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
      indexMaxCorrelation <- positionCor + 1
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