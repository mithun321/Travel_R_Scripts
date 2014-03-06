m1 = "EasyJet"
m2 = "Easyjet Airline Company"

passengerB <- {}
for(j in 1:n){
  abc = data[[j]]
  for(i in 1:length(abc[[1]])){
    if(abc[[3]][i]== m1|| abc[[3]][i]==m2){
      passengerB[j] = abc[[8]][i]
      #print(j)
    }
  }
}

passengerB = ts(passengerB, frequency = 4, start = c(2008, 1))
plot.ts(passengerB, ylab = "#Passenger", 
        main = paste("Passenger distribution for",m1), lwd = 2)