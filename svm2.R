library(e1071)
library(kernlab)




Gatwick = data[[2]]
Heathrow = data[[3]]
London_City = data[[4]]
Luton = data[[5]]
Stansted = data[[7]]
Edinburgh = data[[8]]
Manchester = data[[9]]
TempL = data$TempLevel

Gatwick = (Gatwick - mean(Gatwick))/sd(Gatwick)
Heathrow = (Heathrow - mean(Heathrow))/sd(Heathrow)
London_City = (London_City - mean(London_City))/sd(London_City)
Luton = (Luton - mean(Luton))/sd(Luton)
Stansted = (Stansted - mean(Stansted))/sd(Stansted)
Edinburgh = (Edinburgh - mean(Edinburgh))/sd(Edinburgh)
Manchester = (Manchester - mean(Manchester))/sd(Manchester)

TempL = matrix(TempL)

matrix = cbind(Gatwick,Heathrow,London_City,Luton,Stansted,Edinburgh,Manchester)
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





Cset = seq(89.2,92, 0.2)
bestEfficiency = 0
bestC = Cset[1]
experi <- {}

for(cost in Cset){
    print("*****************")
    print(cost)
    print(bestEfficiency)
    print("*****************")
    
    for(i in 1:100){
      validIndex = sample(n,nVal)
      xVal <- matrix[validIndex,]
      yVal <- TempL[validIndex]
      
      testIndex = sample(setdiff(1:n,validIndex),nTest)
      xTest <- matrix[testIndex,]
      yTest <- TempL[testIndex]
      
      trainIndex = setdiff(1:n,union(testIndex,validIndex))
      xTrain <- matrix[trainIndex,]
      yTrain <- TempL[trainIndex]
      
      svp <- ksvm(xTrain,yTrain,type="C-svc",kernel='vanilladot', 
                  kpar = "automatic", C=cost,scaled=c())
      yPred = predict(svp,xTest)
      table(yTest,yPred)
      experi[i] = sum(yTest==yPred)/length(yPred)
      
    }    
    
    effi = mean(experi)
    if(effi > bestEfficiency){
      bestC = cost
      bestEfficiency = effi
    }
}




cost = bestC

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

svp <- ksvm(xTrain,yTrain,type="C-svc",kernel='vanilladot',
            kpar = "automatic",C=cost,scaled=c())
yPred = predict(svp,xTest)
table(yTest,yPred)
sum(yTest==yPred)/length(yPred)