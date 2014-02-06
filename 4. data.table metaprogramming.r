While your functions certainly look interesting, I believe you are asking if there are other ways to go about it.    
Personally, I like to use something like this: 


    ## SAMPLE DATA
    DT1 <- data.table(id=sample(LETTERS[1:4], 20, TRUE), Col1=1:20, Col2=rnorm(20))
    DT2 <- data.table(id=sample(LETTERS[3:8], 20, TRUE), Col1=sample(100:500, 20), Col2=rnorm(20))
    DT3 <- data.table(id=sample(LETTERS[19:20], 20, TRUE), Col1=sample(100:500, 20), Col2=rnorm(20))

## ACCESSING A TABLE BY REFERENCE TO THE TABLE NAME:
This is straightforward, much like any object in `R`

    # use strings to select the table
    tablesSelected <- "DT3"

    # use get to access them 
    get(tablesSelected)

    # and we can perform operations:
    get(tablesSelected)[, list(C1mean=mean(Col1), C2mean=mean(Col2))]

## SELECTING COLUMNS BY REFERENCE
To select columns by reference to their names, use the `.SDcols` argument. 
Given a vector of column names:  

    columnsSelected <- c("Col1", "Col2")

Assign that vector to the .SDcols argument: 

    ## Here we are simply accessing those columns
    DT3[, .SD, .SDcols = columnsSelected]

We can also apply a function to each column named in the string vector: 

    ## apply a function to each column
    DT3[, lapply(.SD, mean), .SDcols = columnsSelected]

Note that if our goal is simply to output the columns we can turn off `with`: 
    
    # This works for displaying
    DT3[, columnsSelected, with=FALSE]

However, if using `with=FALSE`, we cannot then operate directly on the columns in the usual fashion

    ## This does NOT work: 
    DT3[, colMeans(columnsSelected), with=FALSE]

    ## This DOES work: 
    DT3[, colMeans(.SD), .SDcols=columnsSelected]

    ## This also works, but is less ideal, ie assigning to new columns is more cumbersome
    DT3[, columnsSelected, with=FALSE][, colMeans(.SD)]


We can also use `get`, but it is a bit trickier. 
_I am leaving it here for reference, but `.SDcols` is the way to go_
    
    ## we need to use `get`, but inside `j`
    ##   AND IN A WRAPPER FUNCTION     <~~~~~ THIS IS VITAL

    DT3[, lapply(columnsSelected, function(.col) get(.col))]

    ## We can execute functions on the columns:
    DT3[, lapply(columnsSelected, function(.col) mean( get(.col) ))]


    ## And of course, we can use more involved-functions, much like any *ply call:
    # using .SDcols 
    DT3[, lapply(.SD, function(.col) c(mean(.col) + 2*sd(.col), mean(.col) - 2*sd(.col))), .SDcols = columnsSelected]

    # using `get` and assigning the value to a var.  
    #   Note that this method has memory drawbacks, so using .SDcols is preferred
    DT3[, lapply(columnsSelected, function(.col) {TheCol <- get(.col); c(mean(TheCol) + 2*sd(TheCol), mean(TheCol) - 2*sd(TheCol))})]


For reference, if you try the following, you will notice that they do not produce the results we are after. 

        ## this DOES NOT work
        DT3[, columnsSelected]

        ## netiher does this
        DT3[, eval(columnsSelected)]

        ## still does not work: 
        DT3[, lapply(columnsSelected, get)]


If you want to change the name of the columns: 

    # Using the `.SDcols` method:  change names using `setnames`  (lowercase "n")
    DT3[, setnames(.SD, c("new.Name1", "new.Name2")), .SDcols =columnsSelected]

    # Using the `get` method:  
    ##  The names of the new columns will be the names of the `columnsSelected` vector
    ##  Thus, if we want to preserve the names, use the following: 
    names(columnsSelected) <- columnsSelected    
    DT3[, lapply(columnsSelected, function(.col) get(.col))]

    ## we can also use this trick to give the columns new names
    names(columnsSelected) <- c("new.Name1", "new.Name2")
    DT3[, lapply(columnsSelected, function(.col) get(.col))]

_Clearly, using .SDcols is easier and more elegant._




### What about `by`?

    # `by` is straight forward, you can use a vector of strings in the `by` argument. 

    # lets add another column to show how to use two columns in `by`
    DT3[, secondID := sample(letters[1:2], 20, TRUE)]

    # here is our string vector: 
    byCols <- c("id", "secondID")

    # and here is our call
    DT3[, lapply(columnsSelected, function(.col) mean(get(.col))), by=byCols]


---

## PUTTING IT ALL TOGETHER    

We can access the data.table by reference to its name and then select its columns also by name: 

    get(tablesSelected)[, .SD, .SDcols=columnsSelected]
    
    ## OR WITH MULTIPLE TABLES
    tablesSelected <- c("DT1", "DT3")
    lapply(tablesSelected, function(.T) get(.T)[, .SD, .SDcols=columnsSelected])

    # we may want to name the vector for neatness, since
    #  the resulting list inherits the names. 
    names(tablesSelected) <- tablesSelected

### THIS IS THE BEST PART: 
Since so much in `data.table` is pass-by-reference, it is easy to have a list of tables, a separate list of columns to add and yet another list of columns to operate on, and put all together to add perform similar operations -- but with different inputs -- on all your tables. 
As opposed to doing something similar with `data.frame`, there is no need to reassign the end result. 


    newColumnsToAdd <- c("UpperBound", "LowerBound") 
    FunctionToExecute <- function(vec) c(mean(vec) - 2*sd(vec), mean(vec) + 2*sd(vec))

    # note the list of column names per table! 
    columnsUsingPerTable <- list("DT1" = "Col1", DT2 = "Col2", DT3 = "Col1")
    tablesSelected <- names(columnsUsingPerTable)
    byCols <- c("id")

    # TADA: 
    dummyVar <- # I use `dummyVar` because I do not want to display the  output
    lapply(tablesSelected, function(.T) 
      get(.T)[, c(newColumnsToAdd) := lapply(.SD, FunctionToExecute), .SDcols=columnsUsingPerTable[[.T]], by=byCols ]  )

    # Take a look at the tables now: 
    DT1
    DT2
    DT3


