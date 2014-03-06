rm(list=ls())

library(stringr)
library(Hmisc)

maxvec <- function(X,Y){
  z <- {}
  for(i in 1:length(X)){
    z[i] = max(X[i],Y[i])
  }
  return(z)
}

name <- function(v1) {
  deparse(substitute(v1))
}

data <- read.csv("passenger.csv")

Date = data[[1]]
Gatwick = data[[2]]
Heathrow = data[[3]]
London_City = data[[4]]
Luton = data[[5]]
Southend = data[[6]]
Stansted = data[[7]]
Edinburgh = data[[8]]
Manchester = data[[9]]

Gatwick = ts(Gatwick, frequency = 12, start = c(1998, 1))
Heathrow = ts(Heathrow, frequency = 12, start = c(1998, 1))
London_City = ts(London_City, frequency = 12, start = c(1998, 1))
Luton = ts(Luton, frequency = 12, start = c(1998, 1))
Southend = ts(Southend, frequency = 12, start = c(1998, 1))
Stansted = ts(Stansted, frequency = 12, start = c(1998, 1))
Edinburgh = ts(Edinburgh, frequency = 12, start = c(1998, 1))
Manchester = ts(Manchester, frequency = 12, start = c(1998, 1))

### Select Airport ###

txt = "Gatwick"
o = c(12, 2, 12)


tmseries = get(txt)
Name = txt

#tmseries = Heathrow
#Name = name(Heathrow)

plot.ts(tmseries, ylab = "#Terminal Passenger", 
        main = paste("Terminal Passenger count in", Name))
minor.tick(nx=5)

monthlycount = as.vector(tmseries)
backup = tmseries

valperiod = 12
foreperiod = 12
#foreStartDate = c(2012, 11)
foreStartDate = end(ts(
  backup[1:(length(backup)-foreperiod+1)], 
  frequency = 12, 
  start = c(1998, 1)
))


abc = monthlycount
abc = head(abc, (length(abc)-foreperiod))
newmonth = abc

abc = as.ts(abc[1:(length(abc)-valperiod)])
abc.fit = arima(abc, order = o, method="ML")

#abc.fit = arima(abc,order = o,optim.control = list(maxit = 500))
abc.fore = predict(abc.fit, n.ahead=valperiod) 

U = abc.fore$pred + 2*abc.fore$se
L = maxvec(abc.fore$pred - 2*abc.fore$se, rep(0,valperiod))
L = ts(L, start = start(U)[1], end = end(U)[1])
#L = abc.fore$pred - 2*abc.fore$se

minx=min(abc,L)
maxx=max(abc,U)
ts.plot(abc,abc.fore$pred,col=1:2, ylim=c(minx,maxx), 
        ylab = "Passenger count", lwd = 1:2,
        main = "Passenger count forecast", xlab = "Time in Months")
minor.tick(nx=5)
lines(U, col="blue")
lines(L, col="blue") 

error = 0
for(i in 1:valperiod){
  error = error + 
    ((tail(newmonth, valperiod)-as.vector(abc.fore$pred))/
       tail(newmonth, valperiod))[i]^2
}

paste("Validation Error: ",error*100,"%")







abc = monthlycount

#abc = as.ts(abc)
abc = as.ts(abc[1:(length(abc)-foreperiod)])

abc.fit = arima(abc,order = o, method="ML")

#abc.fit = arima(abc,order = o,optim.control = list(maxit = 500))
abc.fore = predict(abc.fit, n.ahead=foreperiod) 

U = abc.fore$pred + 2*abc.fore$se
L = maxvec(abc.fore$pred - 2*abc.fore$se, rep(0,foreperiod))
L = ts(L, start = start(U)[1], end = end(U)[1])
#L = abc.fore$pred - 2*abc.fore$se

foreseries = ts(abc.fore$pred, frequency = 12, start = foreStartDate)
Useries = ts(U, frequency = 12, start = foreStartDate)
Lseries = ts(L, frequency = 12, start = foreStartDate)


minx=min(abc,L)
maxx=max(abc,U)
#ts.plot(as.ts(monthlycount),abc.fore$pred,col=1:2, ylim=c(minx,maxx), 
#        ylab = "Passenger count", lwd = 1:2,
#        main = "Passenger count forecast", xlab = "Time in Months")
#minor.tick(nx=5)
ts.plot(tmseries,foreseries,col=1:2, ylim=c(minx,maxx), 
        ylab = "Passenger count", lwd = 1:2,
        main = "Passenger count forecast", xlab = "Time in Years")
axis(side = 1, at = c(1999:2014))
lines(Useries, col="blue")
lines(Lseries, col="blue") 

error = 0
for(i in 1:foreperiod){
  error = error + 
    ((tail(monthlycount, foreperiod)-as.vector(abc.fore$pred))/
       tail(monthlycount, foreperiod))[i]^2
}

paste("Forecasting Error: ",error*100,"%")


