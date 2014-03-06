library(e1071)
library(kernlab)




Gatwick = data[[2]]
Heathrow = data[[3]]
London_City = data[[4]]
Luton = data[[5]]
Stansted = data[[7]]
Edinburgh = data[[8]]
Manchester = data[[9]]
RIL = data$RainIntensity
TempL = data$TempLevel
IRL = data$IRLevel
CPIL = data$CPILevel


TempL = CPIL

Gatwick = (Gatwick - mean(Gatwick))/sd(Gatwick)
Heathrow = (Heathrow - mean(Heathrow))/sd(Heathrow)
London_City = (London_City - mean(London_City))/sd(London_City)
Luton = (Luton - mean(Luton))/sd(Luton)
Stansted = (Stansted - mean(Stansted))/sd(Stansted)
Edinburgh = (Edinburgh - mean(Edinburgh))/sd(Edinburgh)
Manchester = (Manchester - mean(Manchester))/sd(Manchester)

TempL = matrix(TempL)

matrix = cbind(Gatwick,Heathrow,London_City,Luton,
               Stansted,Edinburgh,Manchester)
n = length(matrix[,1])
nVal = round(n*.15)
nTest = round(n*.15)  
nTrain = n - nTest - nVal

validIndex = sample(n,nVal)
xVal <- matrix[validIndex,]
yVal <- TempL[validIndex]

testIndex = sample(setdiff(1:n,validIndex),nTest)
xTest <- matrix[testIndex,]
yTest <- TempL[testIndex]

trainIndex = setdiff(1:n,union(testIndex,validIndex))
xTrain <- matrix[trainIndex,]
yTrain <- TempL[trainIndex]

#cost = 100

#svp <- ksvm(matrix,TempL,type="C-svc",kernel='vanilladot',C=cost,scaled=c())
#yPred = predict(svp,xTest)
#table(yTest,yPred)
#sum(yTest==yPred)/length(yPred)



Cset = seq(20,150, 1) #For IR prediction
Sset = rev(seq(0.1,0.5, .1)) #For IR prediction

Cset = seq(60,100, 0.2) #For Temp prediction
Sset = rev(seq(0.04,0.10, .02)) #For Temp prediction


bestEfficiency = 0
bestC = Cset[1]
experi <- {}

for(sig in Sset){ 
  for(cost in Cset){
  print("*****************")
  print(cost)
  print(sig)
  print(bestEfficiency)
  print("*****************")
  
  for(i in 1:10){
    validIndex = sample(n,nVal)
    xVal <- matrix[validIndex,]
    yVal <- TempL[validIndex]
    
    testIndex = sample(setdiff(1:n,validIndex),nTest)
    xTest <- matrix[testIndex,]
    yTest <- TempL[testIndex]
    
    trainIndex = setdiff(1:n,union(testIndex,validIndex))
    xTrain <- matrix[trainIndex,]
    yTrain <- TempL[trainIndex]
    
    svp <- ksvm(xTrain,yTrain,type="C-svc",kernel='rbf', 
                kpar = list(sigma=sig), C=cost)
    yPred = predict(svp,xTest)
    table(yTest,yPred)
    experi[i] = sum(yTest==yPred)/length(yPred)
    
  }    
  
  effi = mean(experi)
  if(effi > bestEfficiency){
    bestC = cost
    bestS = sig
    bestEfficiency = effi
  }
  }
}




cost = bestC
sig = bestS

nVal = 0


validIndex = sample(n,nVal)
xVal <- matrix[validIndex,]
yVal <- TempL[validIndex]

testIndex = sample(setdiff(1:n,validIndex),nTest)
xTest <- matrix[testIndex,]
yTest <- TempL[testIndex]

trainIndex = setdiff(1:n,union(testIndex,validIndex))
xTrain <- matrix[trainIndex,]
yTrain <- TempL[trainIndex]

svp <- ksvm(xTrain,yTrain,type="C-svc",kernel='rbf', 
            kpar = list(sigma=sig), C=cost)
yPred = predict(svp,xTest)
table(yTest,yPred)
sum(yTest==yPred)