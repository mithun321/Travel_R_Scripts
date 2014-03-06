rm(list=ls())


library(ggplot2)
library(Hmisc)

data <- read.csv("passenger.csv")

Tlevels = 3
Rlevels = 3
Clevels = 3
Ilevels = 3



#CPI = data$CPI
#Inflation_Rate = data$Inflation_Rate
#Rainfall = data$Rainfall

#CPI = ts(CPI, frequency = 12, start = c(1998, 1))
#Inflation_Rate = ts(Inflation_Rate, frequency = 12, start = c(1998, 1))
#Rainfall = ts(Rainfall, frequency = 12, start = c(1998, 1))

cutpoints <- quantile(data$Rainfall, 
                      seq(0,1, length =(Rlevels+1)), na.rm = TRUE)
data$RainIntensity <- cut(data$Rainfall, cutpoints)
levels(data$RainIntensity )
for(i in 1:length(data$RainIntensity)){
  if (is.na(data$RainIntensity[i]) == TRUE){
    data$RainIntensity[i] = levels(data$RainIntensity )[1]
  }
}

cutpoints <- quantile(data$CPI, seq(0,1, length =(Clevels+1)), na.rm = TRUE)
data$CPILevel <- cut(data$CPI, cutpoints)
levels(data$CPILevel )
for(i in 1:length(data$CPILevel)){
  if (is.na(data$CPILevel[i]) == TRUE){
    data$CPILevel[i] = levels(data$CPILevel )[1]
  }
}


cutpoints <- quantile(data$Inflation_Rate, 
                      seq(0,1, length =(Ilevels+1)), na.rm = TRUE)
data$IRLevel <- cut(data$Inflation_Rate, cutpoints)
levels(data$IRLevel )
for(i in 1:length(data$IRLevel)){
  if (is.na(data$IRLevel[i]) == TRUE){
    data$IRLevel[i] = levels(data$IRLevel )[1]
  }
}

qplot(GATWICK, data = data, facets = CPILevel~., colour = CPILevel,
      main = "Passenger Count w.r.t. CPI")
qplot(GATWICK, data = data, facets = RainIntensity~., colour = RainIntensity,
      main = "Passenger Count w.r.t. Rainfall")
qplot(GATWICK, data = data, facets = IRLevel~., colour = IRLevel,
      main = "Passenger Count w.r.t. Inflation Rate")
qplot(GATWICK, data = data, facets = CPILevel~RainIntensity, 
      colour = RainIntensity,
      main = "Passenger Count w.r.t. Rainfall and CPI")
qplot(GATWICK, data = data, facets = IRLevel~RainIntensity, 
      colour = RainIntensity,
      main = "Passenger Count w.r.t. Rainfall and Inflation Rate")
qplot(GATWICK, data = data, facets = IRLevel~CPILevel, 
      colour = IRLevel,
      main = "Passenger Count w.r.t. CPI and Inflation Rate")

g <- ggplot(data, aes(GATWICK, HEATHROW))
g = g + geom_point(aes(color = RainIntensity))+geom_smooth()  
g = g + facet_wrap(CPILevel~ RainIntensity) 
g + labs(title = "Passenger Count w.r.t. CPI and Rainfall")

qplot(GATWICK, HEATHROW, data = data, facets = CPILevel~RainIntensity, 
      colour = RainIntensity, geom = c("smooth", "auto"), method = "loess",
      main = "Passenger Count w.r.t. Rainfall and CPI")

g <- ggplot(data, aes(GATWICK, HEATHROW))
g = g + geom_point(aes(color = RainIntensity))+geom_smooth()  
g = g + facet_wrap(IRLevel~ RainIntensity) 
g + labs(title = "Passenger Count w.r.t. Inflation Rate and Rainfall")

g <- ggplot(data, aes(GATWICK, HEATHROW))
g = g + geom_point(aes(color = IRLevel))+geom_smooth()  
g = g + facet_wrap(IRLevel~ CPILevel) 
g + labs(title = "Passenger Count w.r.t. Inflation Rate and CPI")





Temperature <- read.csv("Temp.csv")
data$Temperature = Temperature$Temperature
#Temperature = data$Temperature
#Temperature = ts(Temperature, frequency = 12, start = c(1998, 1))

#g <- ggplot(data, aes(GATWICK, Temperature))
#g = g + geom_point() + geom_smooth()  
#g + labs(title = "GATWICK Passenger vs Temperature")



cutpoints <- quantile(data$Temperature, 
                      seq(0,1, length = (Tlevels+1)), na.rm = TRUE)
data$TempLevel <- cut(data$Temperature, cutpoints)
levels(data$TempLevel )
for(i in 1:length(data$TempLevel)){
  if (is.na(data$TempLevel[i]) == TRUE){
    data$TempLevel[i] = levels(data$TempLevel )[1]
  }
}

qplot(GATWICK, data = data, facets = TempLevel~., colour = TempLevel,
      main = "Passenger Count w.r.t. Temperature")

qplot(GATWICK, data = data, facets = TempLevel~RainIntensity, 
      colour = RainIntensity,
      main = "Passenger Count w.r.t. Rainfall and Temperature")



g <- ggplot(data, aes(GATWICK, HEATHROW))
g = g + geom_point(aes(color = RainIntensity))+geom_smooth()  
g = g + facet_wrap(TempLevel~ RainIntensity) 
g + labs(title = "Passenger Count w.r.t. Temperature and Rainfall")

g <- ggplot(data, aes(CPI, Inflation_Rate))
g = g + geom_point(aes(color = RainIntensity)) 
g = g + geom_smooth(method = "loess", span = 0.1)  
g + labs(title = "CPI vs Inflation Rate")

qplot(CPI, Inflation_Rate, data = data, 
      geom = c("smooth", "auto"), method = "loess", span = .1,
      #colour = RainIntensity, 
      main = "CPI vs Inflation Rate")

g <- ggplot(data, aes(LONDONCITY, Inflation_Rate))
g = g + geom_point(aes(color = CPILevel)) + geom_smooth()  
g + labs(title = "LONDONCITY Count vs Inflation Rate")

g <- ggplot(data, aes(LONDONCITY, CPI))
g = g + geom_point(aes(color = IRLevel)) + geom_smooth()  
g + labs(title = "LONDONCITY Count vs CPI")

qplot(LONDONCITY, Inflation_Rate, data = data, 
      geom = c("smooth", "auto"), method = "loess", span = .1,
      #colour = RainIntensity, 
      main = "LONDONCITY Count vs Inflation Rate")
qplot(LONDONCITY, CPI, data = data, 
      geom = c("smooth", "auto"), method = "loess", span = .1,
      #colour = RainIntensity, 
      main = "LONDONCITY Count vs Inflation Rate")

AirportAnalysis <- function(txt = "HEATHROW", txt2 = "HEATHROW"){
  pdf(paste(txt,".pdf",sep = ""), height = 12, width = 20)
  plot.ts(ts(data[[txt]], frequency = 12, start = c(1998, 1)), 
          ylab = "#Terminal Passenger", lwd = 3,
          main = paste("Terminal Passenger count w.r.t. Time in", txt))
  minor.tick(nx=5)
  dev.off()
  
  a0 = qplot(get(txt), data = data, colour = CPILevel,
             xlab = txt,
             main = "Passenger Count Histogram")
  ggsave(paste(txt,0,".pdf",sep = ""), height = 12, width = 20) 
 
  
  a1 = qplot(get(txt), data = data, facets = CPILevel~., colour = CPILevel,
             xlab = txt,
             main = "Passenger Count w.r.t. CPI")
  ggsave(paste(txt,1,".pdf",sep = ""), height = 12, width = 20) 
  
  
  a2 = qplot(get(txt), data = data, facets = RainIntensity~., 
             colour = RainIntensity,
             xlab = txt,
             main = "Passenger Count w.r.t. Rainfall")
  ggsave(paste(txt,2,".pdf",sep = ""), height = 12, width = 20) 
  
  a3 = qplot(get(txt), data = data, facets = IRLevel~., colour = IRLevel,
             xlab = txt,
             main = "Passenger Count w.r.t. Inflation Rate")
  ggsave(paste(txt,3,".pdf",sep = ""), height = 12, width = 20) 
  
  a4 = qplot(get(txt), data = data, facets = TempLevel~., colour = TempLevel,
             xlab = txt,
             main = "Passenger Count w.r.t. Temperature")
  ggsave(paste(txt,4,".pdf",sep = ""), height = 12, width = 20) 
  
  a5 = qplot(get(txt), data = data, facets = CPILevel~RainIntensity, 
             colour = RainIntensity,
             xlab = txt,
             main = "Passenger Count w.r.t. Rainfall and CPI")
  ggsave(paste(txt,5,".pdf",sep = ""), height = 12, width = 20) 
  
  a6 = qplot(get(txt), data = data, facets = IRLevel~RainIntensity, 
             colour = RainIntensity,
             xlab = txt,
             main = "Passenger Count w.r.t. Rainfall and Inflation Rate")
  ggsave(paste(txt,6,".pdf",sep = ""), height = 12, width = 20) 
  
  a7 = qplot(get(txt), data = data, facets = IRLevel~CPILevel, 
             colour = IRLevel,
             xlab = txt,
             main = "Passenger Count w.r.t. CPI and Inflation Rate")
  ggsave(paste(txt,7,".pdf",sep = ""), height = 12, width = 20) 
  
  a8 = qplot(get(txt), data = data, facets = TempLevel~RainIntensity, 
             colour = RainIntensity,
             xlab = txt,
             main = "Passenger Count w.r.t. Rainfall and Temperature")
  
  
  ggsave(paste(txt,8,".pdf",sep = ""), height = 12, width = 20) 
  
 if(txt != txt2){
   

  a9 = qplot(get(txt), get(txt2), data = data, facets = IRLevel~CPILevel, 
        colour = CPILevel, geom = c("smooth", "auto"), method = "loess",
        xlab = txt,
        ylab = txt2,     
        main = "Passenger Count w.r.t. CPI and Inflation Rate")
  ggsave(paste(txt,9,".pdf",sep = ""), height = 12, width = 20) 
  
  a10 = qplot(get(txt), get(txt2), data = data, 
              facets = TempLevel~RainIntensity, 
        colour = RainIntensity, geom = c("smooth", "auto"), method = "loess",
        xlab = txt,
        ylab = txt2,
        main = "Passenger Count w.r.t. Rainfall and Temperature")
  ggsave(paste(txt,91,".pdf",sep = ""), height = 12, width = 20) 
  
 }
  
  system(paste("pdfjoin ", txt, "*", sep = ""))
  system(paste("mv *joined* ", "Analysis",txt, ".pdf", sep = ""))
  system(paste("rm ", txt, "*", sep = ""))
  
  
  
}



qplot(LONDONCITY, data = data, colour = CPILevel,
      geom = c("auto"),
      binwidth = 8000,
      main = "Passenger Count Density")
qplot(LONDONCITY, data = data, colour = CPILevel,
      geom = c("density"),
      main = "Passenger Count Histogram")