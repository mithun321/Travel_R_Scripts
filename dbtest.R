rm(list=ls())

library(RODBC)
#library(foreign)
#library('ProjectTemplate')

library("RSQLite")
#library(RMySQL)

#a = read.dbf("free.dbf")
con <- dbConnect(drv="SQLite", dbname="reuters.db")




tables <- dbListTables(con)
tables <- tables[tables != "sqlite_sequence"]
lDataFrames <- vector("list", length=length(tables))

for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, 
      statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

database = lDataFrames[[1]]

library(stringr)


data(mtcars)

# car name is data.frame's rownames. 
#Let's split into manufacturer and model columns:

mtcars$mfg = str_split_fixed(rownames(mtcars), ' ', 2)[,1]
mtcars$mfg[mtcars$mfg=='Merc'] = 'Mercedes'
mtcars$model = str_split_fixed(rownames(mtcars), ' ', 2)[,2]

# connect to local MySQL database (host='localhost' by default)
con2 = dbConnect(drv="SQLite", "testdb")

dbWriteTable(con2, 'motortrend1', mtcars)

dbDisconnect(con2)


