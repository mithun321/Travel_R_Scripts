rm(list=ls())

library(xts)
endyear = 2013
endQ = c("1_Jan-Mar", "2_Apr-Jun")
delay <- {}
k=1

for(p in 2007:endyear){
  if(p!=endyear){
    for(q in c("1_Jan-Mar", "2_Apr-Jun", "3_Jul-Sep", "4_Oct-Dec")){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name)
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Delays"){
          print("Found")
          delay[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  }
  else{
    for(q in endQ){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name) 
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Delays"){
          print("Found")
          delay[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  } 
}


delay = ts(delay, frequency = 4, start = c(2007, 1))
plot.ts(delay, ylab = "#Passenger Complains", 
        main = "Passenger Complain because of Flight Delay")

cancel <- {}
k=1

for(p in 2007:endyear){
  if(p!=endyear){
    for(q in c("1_Jan-Mar", "2_Apr-Jun", "3_Jul-Sep", "4_Oct-Dec")){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name)
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Cancellations"){
          print("Found")
          cancel[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  }
  else{
    for(q in endQ){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name) 
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Cancellations"){
          print("Found")
          cancel[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  } 
}


cancel = ts(cancel, frequency = 4, start = c(2007, 1))
plot.ts(cancel, ylab = "#Passenger Complains", 
        main = "Passenger Complain because of Flight Cancellation")



MissedConnection   <- {}
k=1

for(p in 2007:endyear){
  if(p!=endyear){
    for(q in c("1_Jan-Mar", "2_Apr-Jun", "3_Jul-Sep", "4_Oct-Dec")){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name)
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Missed connections"){
          print("Found")
          MissedConnection[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  }
  else{
    for(q in endQ){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name) 
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Missed connection"){
          print("Found")
          MissedConnection[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  } 
}


MissedConnection = ts(MissedConnection, frequency = 4, start = c(2007, 1))
plot.ts(MissedConnection, ylab = "#Passenger Complains", 
        main = "Passenger Complain because of Missed Connections")




baggage   <- {}
k=1

for(p in 2007:endyear){
  if(p!=endyear){
    for(q in c("1_Jan-Mar", "2_Apr-Jun", "3_Jul-Sep", "4_Oct-Dec")){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name)
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Mishandled baggage"){
          print("Found")
          baggage[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  }
  else{
    for(q in endQ){
      name = paste(p, "_Q", q, "_reason.csv", sep = "")
      print(paste("Reading...", name))
      x <- read.csv(name) 
      for(i in 1:length(x[[1]])){
        if(x[[3]][i]=="Baggage"){
          print("Found")
          baggage[k]=x[[4]][i]
          k = k+1
        }
      }
    }
  } 
}


baggage = ts(baggage, frequency = 4, start = c(2007, 1))
plot.ts(baggage, ylab = "#Passenger Complains", 
        main = "Passenger Complain because of Baggage Issues")







ts.plot(delay, cancel, MissedConnection, baggage, col = c(1:4), 
        ylab = "#Passenger Complains", 
        main = "Passenger Complain for several reasons", lwd = 2)

legend('topleft', c("Delay", "Cancellation", "Missed Connection", "Baggage Issues"), 
       col=1:4, lty=1, cex=.55, lwd = 2)
x = as.vector(delay)
y = as.vector(cancel)
z = as.vector(MissedConnection)  

library(scatterplot3d) 
library(lattice)
library(car)

DF = data.frame("Delay" = x, "Cancellation" = y, 
                "Missed Connection" = z, "Baggage Issues" = as.vector(baggage))
#plot(DF, pch = 20)
scatterplotMatrix(DF,  pch = 20, spread=FALSE)

#plot(x, y, xlab="Delay", ylab="Cancellations", pch=20)
#res = lm(y~x)
#abline(res, col="red")
#lines(lowess(x,y), col="blue") 

s3d <- scatterplot3d(delay,cancel,MissedConnection, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot with Regression plane", angle = 30,
              xlab = "Delay", ylab = "Cancellation", zlab = "Missed Connection")

fit <- lm(MissedConnection ~ delay+cancel) 
s3d$plane3d(fit)
