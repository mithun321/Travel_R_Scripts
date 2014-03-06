rm(list=ls())

library(Hmisc)
library(ggplot2)
library(car)


colist = c("orange", "lightblue3", "black", "blue", 
           "red", "blueviolet", "chartreuse4", "chocolate4"
           )

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

plot.ts(Gatwick, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in Gatwick")
minor.tick(nx=5)

plot.ts(Heathrow, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in Heathrow")
minor.tick(nx=5)

plot.ts(London_City, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in London_City")
minor.tick(nx=5)

plot.ts(Luton, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in Luton")
minor.tick(nx=5)

plot.ts(Southend, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in Southend")
minor.tick(nx=5)

plot.ts(Stansted, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in Stansted")
minor.tick(nx=5)

plot.ts(Edinburgh, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in Edinburgh")
minor.tick(nx=5)

plot.ts(Manchester, ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in Manchester")
minor.tick(nx=5)

ts.plot(Gatwick ,
        Heathrow ,
        London_City ,
        Luton ,
        Southend ,
        Stansted ,
        Edinburgh ,
        Manchester ,
        col = colist, 
        ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in different UK Airports", lwd = 2)
axis(side = 1, at = c(1998:2014))

legend('topleft', c("Gatwick" ,
                    "Heathrow" ,
                    "London_City" ,
                    "Luton" ,
                    "Southend" ,
                    "Stansted" ,
                    "Edinburgh" ,
                    "Manchester"), 
       col= colist, lty=1, cex=.55, lwd = 2)


ts.plot(Gatwick ,
        Heathrow ,
        London_City ,
        Luton ,
        Southend ,
        Stansted ,
        Edinburgh ,
        Manchester ,
        col = colist, 
        ylab = "#Terminal Passenger", 
        main = "Terminal Passenger count in different UK Airports", lwd = 2)
axis(side = 1, at = c(1998:2014))

DF = data.frame("Gatwick" = Gatwick,
                "Heathrow" = Heathrow,
                "London_City" = London_City,
                "Luton" = Luton,
                #"Southend" = Southend,
                "Stansted" = Stansted,
                "Edinburgh" = Edinburgh,
                "Manchester"= Manchester)
#plot(DF, pch = 20)
scatterplotMatrix(DF,  pch = 20, spread=FALSE)