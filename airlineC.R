rm(list=ls())

endyear = 2013
endQ = c("1_year_to_end_of_March", "2_year_to_end_of_June")
data <- list()
l=1
for(p in 2008:endyear){
  if(p!=endyear){
    for(q in c("1_year_to_end_of_March", "2_year_to_end_of_June", 
               "3_year_to_end_of_September", "4_year_to_end_of_December")){
      name = paste(p, "_Q", q, "_airline.csv", sep = "")
      print(paste("Reading...", name))
      data[[l]] <- read.csv(name)
      l = l+1
    }
  }
  else{
    for(q in endQ){
      name = paste(p, "_Q", q, "_airline.csv", sep = "")
      print(paste("Reading...", name))
      data[[l]] <- read.csv(name)
      l = l+1
    }
  } 
}
n = length(data)

BarComplain <- function(index){
  abc = data[[index]]
  airline =  head(abc[[3]], (length(abc[[3]])-3))
  cpmp = head(abc[[9]], (length(abc[[9]])-3))
  
  good = complete.cases(airline,cpmp)
  airline = airline[good]
  cpmp = cpmp[good]
  
  df = data.frame("Airlines" = airline, 
                  "Complaints per Million passenger" = cpmp)
  m = paste("Complaints rate distribution by airlines", ":", abc[[2]][1])
  color = (index%%4)+1
  par(mar= c(8,4,3,1))
  mp <- barplot(df[[2]], axes = FALSE, axisnames = FALSE, 
                col = color)
  axis(2)
  axis(1, at = mp, labels = df[[1]], cex.axis = 0.6, las = 2)
  title(main = m)
  
  #barplot(df[[2]], names.arg = df[[1]], 
  #ylab = "Complaints per Million passenger", 
  #        xlab = "Airlines", main = m, las = 2)
}

for(i in 1:n){
  BarComplain(i)
}

BarPlotbyQ <- function(txt){
  for(i in 1:n){
    abc = data[[i]]
    if(abc[[2]][1] == txt)
      BarComplain(i)
  }
}

m2 = "British Airways plc" 
m1 =  "British Airways" 

complainTimeB <- {}
for(j in 1:n){
  abc = data[[j]]
  for(i in 1:length(abc[[1]])){
    if(abc[[3]][i]== m1|| abc[[3]][i]==m2){
      complainTimeB[j] = abc[[9]][i]
      #print(j)
    }
  }
}

complainTimeB = ts(complainTimeB, frequency = 4, start = c(2008, 1))
plot.ts(complainTimeB, ylab = "Passenger Complaints rate", 
        main = paste("Passenger Complaints rate for",m1), lwd = 2)

m1 = "EasyJet"
m2 = "Easyjet Airline Company"

complainTimeE <- {}
for(j in 1:n){
  abc = data[[j]]
  for(i in 1:length(abc[[1]])){
    if(abc[[3]][i]== m1|| abc[[3]][i]==m2){
      complainTimeE[j] = abc[[9]][i]
      #print(j)
    }
  }
}

complainTimeE = ts(complainTimeE, frequency = 4, start = c(2008, 1))
plot.ts(complainTimeE, ylab = "Passenger Complaints rate", 
        main = paste("Passenger Complaints rate for",m1), lwd = 2)

m1 = "Ryanair"
m2 = "Ryanair"


complainTimeR <- {}
for(j in 1:n){
  abc = data[[j]]
  for(i in 1:length(abc[[1]])){
    if(abc[[3]][i]== m1|| abc[[3]][i]==m2){
      complainTimeR[j] = abc[[9]][i]
      #print(j)
    }
  }
}

complainTimeR = ts(complainTimeR, frequency = 4, start = c(2008, 1))
plot.ts(complainTimeR, ylab = "Passenger Complaints rate", 
        main = paste("Passenger Complaints rate for",m1), lwd = 2)

ts.plot(complainTimeB, complainTimeE, complainTimeR, col = c(1:3), 
        ylab = "#Passenger Complaits Rate", 
        main = "Passenger Complaints Rate for different Airways", lwd = 2)

legend('topleft', c("British Airways", "Easy Jet", "Ryanair"), 
       col=1:3, lty=1, cex=.55, lwd = 2)


x = as.vector(complainTimeB)
y = as.vector(complainTimeE)
z = as.vector(complainTimeR)  

library(scatterplot3d) 
library(lattice)
library(car)

DF = data.frame("British Airways" = x, "Easy Jet" = y, "Ryanair" = z)
#plot(DF, pch = 20)
scatterplotMatrix(DF,  pch = 20, spread=FALSE)



s3d <- scatterplot3d(x,y,z, pch=16, highlight.3d=TRUE,
                     type="h", main="3D Scatterplot with Regression plane", 
                     angle = 45,
                     xlab = "British Airways", ylab = "Easy Jet", 
                     zlab = "Ryanair")

fit <- lm(z ~ x+y) 
s3d$plane3d(fit)
