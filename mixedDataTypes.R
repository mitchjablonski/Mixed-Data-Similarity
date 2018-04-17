calcSimMatrixNominal <- function(currData)
{
  myMat = matrix(data = NA, nrow = length(currData), ncol = length(currData))
  for (i in 1: length(currData))
  {
    for (j in 1:length(currData))
    {
      if (currData[i] == currData[j])
      {
        myMat[i,j] = 1
      }
      else
      {
        myMat[i,j] = 0
      }
    }
  }
  return(myMat)
}

convertOrdinalMetric <- function(myData)
{
  myData = tolower(gsub(' ', '', myData))
  ordConv= matrix(,length(myData))
  ordConv[myData == 'excellent'] = 3
  ordConv[myData == 'good'] = 2
  ordConv[myData == 'fair'] = 1
  ordConv[myData == 'poor'] = 0
  ordConv = minMaxScale(ordConv)
  return(findSimMatrix(ordConv))
}

minMaxScale <- function(dataToScale)
{
  return( (dataToScale - min(dataToScale)) / (max(dataToScale) - min(dataToScale)) )
}

findSimMatrix <- function(dataForSim)
{
  myMat = matrix(data = NA, nrow = length(dataForSim), ncol = length(dataForSim))
  for (i in 1: length(dataForSim))
  {
    for (j in 1:length(dataForSim))
    {
      myMat[i,j] = abs(dataForSim[i] - dataForSim[j])
    }
  }
  ##we computed a distance matrix, convert to similairty
  return(1 - myMat)
}

computeAsym <- function(dataForASym)
{
  myMat = matrix(data = NA, nrow = length(dataForASym), ncol = length(dataForASym))
  for (i in 1: length(dataForASym))
  {
    for (j in 1:length(dataForASym))
    {
      iValue = toupper(gsub(' ', '',dataForASym[i])) 
      jValue = toupper(gsub(' ', '',dataForASym[j]))
      if (i == j)
      {
        myMat[i,j] = 1
      }
      else if(iValue == 'Y' || jValue  == 'Y')
      {
        if (iValue == 'Y' && jValue == 'Y')
        {
          myMat[i,j] = 1
        }
        else
        {
          myMat[i,j] = 0
        }
      }
      else
      {
        myMat[i,j] = NaN
      }
    }
  }
  return(myMat)
}

combineAsymWithNormal <-function(mySimMat)
{
  myrows = dim(mySimMat)[1]
  mycols = dim(mySimMat)[2]
  nelem = dim(mySimMat)[3]
  myMat = matrix(data = NA, nrow = myrows, ncol = mycols)
  for (i in 1:myrows)
  {
    for (j in 1:mycols)
    {
      currElem = mySimMat[i,j,1:nelem]
      myMat[i,j] = mean(currElem[!is.nan(currElem)])
    }
  }
  return(myMat)
}


##
##Notes, compute 5 similiarity/ distance matrices
##Covert all to similiarity matrics
##add all 5 together, giving twice weight to nominal and asymmetric binary matrices, divide each entry by 7 for a single 6x6 between 0-1
##if not 1s along diagonal something wrong
##Main.

#Decode:
#symBinary = 0
#nominal = 1
#ordinal = 2
#numeric = 3
#asym binary = 4

library("lsa")
library("gdata")
getwd()
setwd("C:/Users/mitch/Desktop/Masters/MixedData")
mixedData=read.csv("mixeddatacsv.csv",header=TRUE,sep=",")
head(mixedData)
#pull out info
columns = colnames(mixedData)
simColumns= columns[2:length(columns)]
##Values for type of data
dataTypes = c(0,1,1,2,3,4,4)
numUsers = length(mixedData$Name)
numFeatures = length(simColumns)
simMat = array(data = NA,dim = c(numUsers,numUsers,numFeatures))

##easy t/f either equal or not
for (i in 1:numFeatures)
{
  ##symmetric binary and nominal
  if (dataTypes[i] == 0 || dataTypes[i] == 1)
  {
    ##nominal data that is either equal or not all of equal value
    currSimMat = calcSimMatrixNominal(mixedData[[simColumns[i]]])
    simMat[,,i] = currSimMat
  }
  ##Ordinal
  if (dataTypes[i] == 2)
  {
    ##Health has levels, good excelent, poor fair, each with a meaning unlike a color, assign numeric values
    ##oridinal will handle vlaues excellent good fair and poor, it mixmaxscales and computes sim for us
    simMat[,,i] = convertOrdinalMetric(mixedData[[simColumns[i]]]) 
  }
  if (dataTypes[i] ==  3) 
  {
    ##numeric data needs to be minmaxedscalled
    simMat[,,i] = findSimMatrix(minMaxScale(mixedData[[simColumns[i]]]))
  }
  if (dataTypes[i] == 4)
  {
    simMat[,,i] = computeAsym(mixedData[[simColumns[i]]])
  }
}
##Here in combine, we handle whether we considered the 2 Asymmetric fields or not, by checking for NaNs and dividing by 5/7
matCompiledWithAsym = combineAsymWithNormal(simMat)
matCompiledWithAsym

