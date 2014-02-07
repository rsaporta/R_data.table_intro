
unique.data.table takes unique rows BY KEY!
Unless it is specified to do otherwise 

DT <- data.table(CJ(B=c("Yes", "No"), M=c("Yes", "No"), R=c("Yes", "No")), val=rep(c(1:2), each=8))
DT[, Col5 := rep(c("Hi", "There", "World"), times=c(8, 7, 1))]
setkey(DT, B, M, R)
DT

# Default is by key
unique(DT)

## Notice that row 1 & 2 are unique relative to each other, 
##   but still, row 2 is missing since they are not unique by key
DT[1:2]
unique(DT[1:2])


## To get unique rows by ALL COLUMNS, set the by argument to NULL
unique(DT, by=NULL)

## Setting by to FALSE is an error
unique(DT, by=FALSE) ## ERROR

## You can also specify which columns to use to check for uniqueness
## If there is a tie (relative to the columns not-being used), the first row is used
unique(DT, by=c("B", "R"))
unique(DT, by=c("M", "R"))
unique(DT, by="val")
unique(DT, by="Col5")
