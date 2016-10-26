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
GetIndexStartMostSimilarSample <- function(trainData, approximationSample, step) 
{
  
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


EnumerableDeltaData <- function(dataDelta)
{
  pos <- c(1 : length(dataDelta))
  neg <- c(1 : length(dataDelta))
  posIndex <- 1
  negIndex <- 1
  for(i in 1:length(dataDelta))
  {
    if(dataDelta[i] >= 0)
    {
      pos[i] <- posIndex
      neg[i] <- 0
      posIndex <- posIndex +1
    }
    else
    {
      pos[i] <- 0
      neg[i] <- negIndex
      negIndex <- negIndex + 1
    }
  }
  return(data.frame(index = c(1 : length(dataDelta)), positive = pos, negative = neg))
}

GetPredictForSeparateDelta <- function(trainDelta, M, P)
{
  matchingIndex <- EnumerableDeltaData(trainDelta)
  
  positiveDelta <- trainDelta[trainDelta >= 0]
  negativeDelta <- trainDelta[trainDelta < 0]
  
  trainData <- GetTrainSample(positiveDelta, M)
  MSample <- GetAproxSample(positiveDelta, M)
}


