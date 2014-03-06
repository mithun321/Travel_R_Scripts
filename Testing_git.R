# this code sucks!!!11 don't look at it please!

library(qgraph)

input_data <- read.csv('A.csv')
usageNetwork <- input_data

#usageNetwork <- I(
#  data.frame(
#	USERS=I(c('Peter','David','Rita','Janis','Rita','Janis')), 
#	REPORTS_ACCESSED=I(c('Usage Adoption','Marketing','Sales','Sales','Usage Adoption','Usage Adoption')),
#	TIMES_ACCESSED=c(13,5,10,10,1,6)
#	)
#)

#vUsers <- unique(usageNetwork[,1])
#vReports <- unique(usageNetwork[,2])

vUsers <- levels(usageNetwork[,1])
vReports <- levels(usageNetwork[,2])

vNodes <- c(vUsers, vReports)

numNodes <- length(vNodes)
vLinks <- rep(0, numNodes^2)

m <- matrix(vLinks, nrow=numNodes, ncol=numNodes, dimnames = list(vNodes, vNodes))

for(u in vUsers)
  for(r in vReports) {
    if(length(usageNetwork[usageNetwork$USERS==u & usageNetwork$REPORTS_ACCESSED==r,3]) == 1)
      m[u,r] <- usageNetwork[usageNetwork$USERS==u & usageNetwork$REPORTS_ACCESSED==r,3]
  }


qgraph(
  m, 
  groups = list(
    'Users'=1:length(vUsers) ,
    'Reports'=(1+length(vUsers)):(length(vUsers)+length(vReports))
  ),
  
  labels=c(vUsers,vReports),
  edge.labels=TRUE,
  posCol = "black",		# positive edge color
  layout="circle",
  vsize=c(6, 12),
  shape="circle",
  pastel=TRUE
)
