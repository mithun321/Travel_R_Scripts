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

matrix = cbind(Gatwick,Heathrow,London_City,Luton,Stansted,Edinburgh,Manchester)
trainset = data.frame(Gatwick = Gatwick, Heathrow = Heathrow, 
                      London_City = London_City, Luton = Luton, 
                      Stansted = Stansted, Edinburgh = Edinburgh,
                      Manchester = Manchester, TempRange = TempL)
n = length(trainset[,1])
nVal = round(n*.15)
nTest = round(n*.15)  
nTrain = n - nTest - nVal

validIndex = sample(n,nVal)
xVal <- trainset[validIndex,]

testIndex = sample(setdiff(1:n,validIndex),nTest)
xTest <- trainset[testIndex,]

trainIndex = setdiff(1:n,union(testIndex,validIndex))
xTrain <- trainset[trainIndex,]
  
#testset = trainset[sample(1:n,5),]

#svm.model <- svm(TempRange ~ ., data = trainset, cost = 100, gamma = 1)
#svm.pred <- predict(svm.model, testset[,-8])
Cset = seq(73,76, 0.1)
Gset = seq(0.01,.1, 0.01)
bestEfficiency = 0
bestC = Cset[1]
bestG = Gset[1]
experi <- {}

for(C in Cset){
  for(G in Gset){
    print("*****************")
    print(C)
    print(G)
    print(bestEfficiency)
    print("*****************")
    
    for(i in 1:10){
      validIndex = sample(n,nVal)
      xVal <- trainset[validIndex,]
      
      testIndex = sample(setdiff(1:n,validIndex),nTest)
      xTest <- trainset[testIndex,]
      
      trainIndex = setdiff(1:n,union(testIndex,validIndex))
      xTrain <- trainset[trainIndex,]
      
      svm.model <- svm(TempRange ~ ., data = xTrain, cost = C, gamma = G)
      svm.pred <- predict(svm.model, xVal[,-8])
      obs =  as.vector(xVal[,8])
      pred =  as.vector(svm.pred) 
      experi[i] = sum(obs==pred)/length(pred)
      
    }    
    
    effi = mean(experi)
    if(effi > bestEfficiency){
      bestC = C
      bestG = G
      bestEfficiency = effi
    }
  }
}

bestC
bestG

C = bestC
G = bestG
nVal = 0

validIndex = sample(n,nVal)
xVal <- trainset[validIndex,]

testIndex = sample(setdiff(1:n,validIndex),nTest)
xTest <- trainset[testIndex,]

trainIndex = setdiff(1:n,union(testIndex,validIndex))
xTrain <- trainset[trainIndex,]


svm.model <- svm(TempRange ~ ., data = xTrain, cost = C, gamma = G)

svm.pred <- predict(svm.model, xVal[,-8])
obs =  as.vector(xVal[,8])
pred =  as.vector(svm.pred) 
t1 = table(pred,obs)
#t1
#print("Validation Accuracy")
#sum(obs==pred)/length(pred)


svm.pred <- predict(svm.model, xTest[,-8])
obs =  as.vector(xTest[,8])
pred =  as.vector(svm.pred) 
t1 = table(pred,obs)
t1
print("Test Accuracy")
sum(obs==pred)/length(pred)

