LLL <- do.call(paste0, replicate(3, LETTERS, simplify=FALSE))

N.IDs <- 200
IDs <- sample(4e7:9e7, N.IDs, FALSE)

LLL <- paste0(LETTERS, LETTERS, LETTERS)

=========================================

#             "CEATE A data.table"
# 
# N.rows <- 20
# 
# set.seed(1)
# DT <- data.table(ID    = sample(IDs, N.rows, TRUE)
#                , Group = sample(LLL, N.rows, TRUE)
#                , Value = round(rnorm(N.rows), 4)
#                )
# 
# setkey(DT, ID, Group)

=========================================

            "CEATE A data.table"


# A set of hypothetical IDs
IDs <- c("AB123", "CD345", "EF623", "HJ537", "KP635", "RS723", "XY789")

# Descriptive information for the set of IDs
DT.info <- data.table(ID=IDs[-2], Group=paste0("Grp", seq_along(IDs[-2])), Type=rep(c("A","B"), length(IDs)/2)
                     , key=c("ID","Group"))


## We will create some fake data
# The number of rows for our sample data
N <- 20 ## 1e5

# Have a look at the table
set.seed(1)
DT <- data.table(ID=sample(IDs[-7], N, TRUE), Value=abs(round(rnorm(N)*100)))

# Have a look at the table
DT

# Let's set the key to ID
setkey(DT, ID)  ## No "quotes" around ID

# Have a look at the table
DT

# We could have set the key upon creation 
set.seed(1)
DT <- data.table(ID=sample(IDs, N, TRUE), Value=abs(round(rnorm(N)*100))
                , key="ID")  ## Yes "quotes" around "ID"
                             ## Ask why in class (Hint: R is lazy)
DT

# Note that we had set the key for DT.info when we created it. 
# We can use key() to check 
key(DT.info)

## Note that key(.) returns NULL if no key is set
DT.with.no.key <- data.table(A=1:5, B=LETTERS[1:5])
key(DT.with.no.key)

# Let's have a look at which tables are currently in our environment
tables()


Create a small data.table using ...

=========================================
 
             `:=`()

## Adding a column
DT[, Gender := sample(c("M", "F"), nrow(DT), TRUE)]

# Introducing .N.  We will cover it in detail shortly
DT[, Gender := sample(c("M", "F"), .N, TRUE)]

DT

## Select a column
DT$Gender
DT[["Gender"]]
## This method is specific to data.table
DT[, Gender]

## Notice that 'Gender' is a string. 

## Let's convert it to a factor

DT[, Gender := factor(Gender)]
# Have a look at the coloumn
DT[, Gender]

=========================================

        by=...

## Find an average value per group
DT[, median(Value), by=Gender]

## Total per group
DT[, sum(Value), by=Gender]

## What if a group is a Gender--ID combination?
DT[, median(Value), by=list(Gender, ID)]

# The columns in 'by=...' are returned first, and in the order stated
DT[, median(Value), by=list(ID, Gender)]
DT[, median(Value), by=list(Gender, ID)]


THREE DIFFERENT SYNTAX OPTIONS. 
(I suggest to pick one and stick with it, but they each have an advantage)

DT[, median(Value), by=list(ID, Gender)]
DT[, median(Value), by=c("ID", "Gender")]
DT[, median(Value), by="ID,Gender"]

    by="ID,Gender"
    ## A single string, comma separated
    ## Spaces will give you an error, or unexpected results
    ##    (it will be considered part of the column name)
    ## I use this method when working interactively
    ##   but generally avoid in scripts & functions.

    by=c("ID", "Gender")
    ## A vector of strings of strings
    ## Useful for when assigning this vector ahead of time. 
    ## eg:  
    ##    by.cols <- c("ID", "Gender")
    ##    DT[, ..., by=by.cols]
    ## 

    by=list(ID, Gender)
    ## A list of UNQUOTED column names
    ## Most versatile
    ## I mostly use this method
    "But wait, there's more!"
    ## If you use a NAMED LIST, will rename the by-columns
    DT[, sum(Value), by=list(Name=ID, Gender)]

# The 'by=...' argument has some intricacies and pitfalls that we will cover later

=========================================

        DT[i=.., j=.., by=.. ]

        SQL Analogy:
        DT[WHERE, SELECT, GROUPBY]

## Let's find the average value for ID "CD345"
DT[ID=="CD345"
  , mean(Value)
  ]

## Let's find the average value for IDs "CD345" or "EF623"
DT[ID %in% c("CD345", "EF623")
  , mean(Value)
  ]

## Let's find the average value for IDs "CD345" or "EF623"
##    grouping by ID
DT[ID %in% c("CD345", "EF623")
  , mean(Value)
  , by=ID
  ]


           The 'j' Argument.  
           -----------------
## When speaking aobut data.table
##    we often refer to 'i' & 'j' (often without quotes)
## DT[i=.., j=.., by=..]
##  
## j is not to be confused with J(), a joining function


## Let's find the mean and ±2 SD of the value, by ID
DT[ , {mn  <- mean(Value)
       std <- sd(Value)
       list('-2SD'=mn-(2*std), Mean=mn, '+2SD'=mn+(2*std))
        }
    , by=ID]


         VERY IMPORTANT CONCEPT
         ----------------------
## The 'j' argument is an expression. 
## It is NOT the name or number of a column
DT[, 5, by=ID]
DT[, 5]
## Once this is grok'd everything else becomes a lot easier
DT[, cat("Hello, I am an expression in 'j'\n")]  ## Question: Why the NULL?
DT[, cat("Hello, I am an expression in 'j'\n"), by=ID]
DT[, 2 + 3]

## The 'j' expression is evaluated within the DT environment
DT[, myVar <- 1:20]
DT
myVar  ## not found

## However, we can create variables inside of the 'j' expression
##   ... and use them!
##
## As we did in the example above
DT[ , { mn  <- mean(Value)
        std <- sd(Value)
        list('-2SD'=mn-(2*std), Mean=mn, '+2SD'=mn+(2*std))
        }
    , by=ID]

## THERE IS NO "drop" ARGUMENT IN data.table
DT[, Gender, drop=FALSE]

## Instead, if we want to return a data.table, use list() 
DT[, list(Gender)]

## Simliarly, use list() if you want to select more than one column
DT[, list(ID, Gender)]

## The old fashioned-way is no more
DT[, c("ID", "Gender")]  #Why this output?
## ... well, not quite the case. You can always use 'with=FALSE'
DT[, c("ID", "Gender"), with=FALSE]

## using 'with=FALSE' is useful when the column vector is assigne elsewhere
using.cols <- c("ID", "Gender")
DT[, using.cols, with=FALSE] 

## Generally though, stick to list()
DT[, list(ID, Gender)]

## If 'j=..' evaluates to _NAMED_ list, the names will be the names of the columns
##   otherwise, they will be named V1, V2, etc... (contrast with X1, X2, .. in data.frame)
DT[, list(mean(Value))]
DT[, list(Value=mean(Value))]




        Updating the columns use `:=`
        (Not just about cleaner syntax, but also memory efficiency)
## 
DT[ , MeanVal := mean(Value)]
DT
DT[ , MeanVal.PerId := mean(Value), by=ID]
DT
## recall to drop columns, use DT[, colName := NULL]
##    or for multiple columns at once, use:
DT[, c("MeanVal", "MeanVal.PerId") := NULL]

## Dropping a column that DNE gives a warning
DT[, ColumnABCDEF := NULL]

## To assign multiple columns, two ways to use `:=`
##    (always in 'j')
1. `:=`(colName1=Values_1, colName2=Values_2, etc)
2.  <vector of col names> := <list of values>

## Examples: 
# 1. 
DT[, `:=`(  MeanVal.PerId=mean(Value)
               , SD.PerID=sd(Value)
         )
   , by=ID]

## The latter case works much like a named-list. 
## Regular scoping rules apply

## Recall using braces inside an expression
x <- 1:10
list( mn = mean(x), sd=sd(x))
list( mn  = {mn <- mean(x);
             mn}
    , '-2sd'= {std <- sd(x); 
               -2*std + mn
              }
    , '+2sd'= +2*std + mn
    )

## Braces are your friend 
DT[, `:=`(   mn  = {mn <- mean(Value);
                    mn
                   }
         , '-2sd'= {std <- sd(Value); 
                    -2*std + mn
                   }
         , '+2sd'= +2*std + mn
         )
   , by=ID]

## 2.
DT[, c("MeanVal.PerId", "SD.PerID") := list(mean(Value), sd(Value)), by=ID]
DT
## recall how to drop columns
DT[, c("MeanVal.PerId", "SD.PerID") := NULL]
DT

## The vector on the left can be any standard vector
## The RHS (right hand side), any standard list
##
##  The RHS is recycled to match the LHS   <~~~~ IMPORTANT

## RHS is too short, relative to LHS
## RHS will be recycled
DT[, paste("Col", 1:3) := list(sample(letters, 20), seq(20))]
DT
## (remove the columns)
DT[, paste("Col", 1:3) := NULL]
DT

## RHS is too short
## RHS will be CROPPED! (With a warning, of course)
DT[, paste("Col", 1:2) := list(sample(letters, 20), seq(20), LETTERS[1:20])]
DT

    
    ==========================================================================


                Joining Fun

basic join syntax: 
X[Y]

L <- c("A", "B", "C", "D")

DT1 <- data.table(Group=rep(L[1:3], 2)
                , Gender=rep(c("F", "M"), 3)
                , Score=16:11
                , key=c("Group", "Gender") )

# Let's drop one group, for the purpose of demonstrating Left / Right joins
DT1 <- DT1[!.("B", "M")]   # Dropping Group B, Male

DT2 <- data.table(Group=L[c(1:2, 4)]
                , Class=c("m", "n", "p")
                , key=c("Group") )

## Joins are RIGHT OUTER by default
DT1[DT2]   # Group "D" is in DT2, but not in DT1. 
DT2[DT1]   # Group "C" is in DT1, but not in DT2
## Notice that the columns from the LEFT DT are filled with NA when there is no match

## INNER JOIN: USE 'nomatch=0 '
DT1[DT2, nomatch=0]
## default value for nomatch is NA
DT1[DT2, nomatch=NA]

    ## NOTE: `nomatch` is NOT a fill-value, it simply a flag. 
    ##        Values other than NA or 0 (or FALSE) are an error
        DT1[DT2, nomatch=1]
        DT1[DT2, nomatch=""]
        DT1[DT2, nomatch=FALSE]  # same as nomatch=0
        DT1[DT2, nomatch=TRUE]

## FULL OUTER JOIN:  use merge(.., all=TRUE)
merge(DT1, DT2, all=TRUE)
?merge.data.table

## by default, merging is done on the keys, NOT column names
DT2[, Gender := c("F", "F", "F")]

merge(DT1, DT2)
merge(DT1, DT2, by=c("Group", "Gender"))
merge(DT1, DT2, by=c("Group", "Gender"), all.x=TRUE)
merge(DT1, DT2, by=c("Group", "Gender"), all.y=TRUE)
merge(DT1, DT2, by=c("Group", "Gender"), all=TRUE)

## VERY IMPORTANT:  
## VERY IMPORTANT:   Merging is by KEY,  *NOT* by column name
## VERY IMPORTANT:  
##   
## This can bite you, if caught unaware

setkey(DT1, Group, Gender)
setkey(DT2, Group, Class)  
## NOTICE that DT2 has a column named Gender, but it is not keyed by this column
DT1[DT2]  # Look at the Gender column
DT2[DT1]  # Look at the Class column

setdiff(key(DT1), key(DT2))
stopifnot( setdiff(key(DT1), key(DT2)) )
## REMEMBER: in DT1[DT2]  we are selecting from DT1

    --------------------------------------------------

                by-without-by

## Let's go back to our DT / DT.info example from before
DT
## Remove all columns EXCEPT ID and Value
DT[, setdiff(names(DT), c("ID", "Value")) := NULL]

DT
DT.info
tables()

## Simply to reinforce, there is a difference
##   between X[Y] and Y[X]
DT.info[DT]
DT[DT.info]

What is by-without-by ??
DT[, mean(Value)]
DT[, list(mean(Value))]
DT[DT.info, mean(Value)]

## by-without-by is a design choice and there is an ongoing 
## discussion to determine if this should be changed so that 
## it has to be turned on explicitly
## (See http://bit.ly/DT_byWOby to follow the discussion)
## 
## Currently, in the event you want to NOT have this behavior, 
##   simply daisy chain the `[ ]` as in:
DT[DT.info][, mean(Value)]
DT[DT.info][, mean(Value, na.rm=TRUE)]
# or 
DT[DT.info, nomatch=0][, mean(Value)]

## More on daisy chaining after we cover selecting


        Updating DT from a join
        -----------------------

DT
DT[DT.info, Group := Group]
DT

## Recall, Drop a column by setting it NULL
DT[, Group := NULL]


## Add multiple columns
DT[DT.info, c("Group", "Value") := list(Group, Value)]
DT

## Cannot drop columns during a join, BUT will NOT throw an error.
DT[DT.info, c("Group", "Value") := NULL]
DT

## Remember, to drop column, leave i blank
DT[ , c("Group", "Value") := NULL]

## What if DT already has a column with the Same name?
## No prob! use 'i.COL' notation

## Let's create a different column named "Group" in DT
DT[, Group := sample(c("ZZZ", "YYY"), .N, TRUE)]
DT

## What happens we join
DT[DT.info]
## different suffix from merge
merge(DT, DT.info)
merge(DT, DT.info, suffixes=c("", ".1")) 
## ... but merge is rarely what we will use
DT[DT.info]
DT[DT.info, Group.1]
DT[DT.info, Group.y]
## Use the "i."
DT[DT.info, i.Group]
DT[DT.info, NewGroup := i.Group]
DT[DT.info, ConcatGroup := paste(Group, i.Group, sep="_")]
DT
## Notice that ID "CD345" has NA for ConcatGroup as well.  Why?
## If you get this concept, you are getting X[Y] !!

## If we want to add two groups at once, use c() := list()
DT[DT.info, c("NewGroup", "Type") := list(i.Group, Type)] # Why i.Group but not i.Type ??
DT

=========================================


## fix the concat group



            Selecting
            ----------

    The general form is :

      DT[ where, select|update, group by ][ having ][ order by ][ ]...[ ]


    ## Let's add some more columns
    DT[, Score := {set.seed(7); sample(20:100, .N)} ]
    DT[, State := {set.seed(7); sample(c("NY", "CA", "PA"), .N, TRUE)} ]
    DT

         SELECTING 
    ## Several ways to do it
    ## 
    ## 1. Vector Search, using standard logical vectors
    ##    Combine them using `&` and `|` as you would in an 'if (clause1 & clause2)'
    DT[ID == "EF623"]
    DT[ID == "EF623" | ID == "RS723"]
    DT[(ID == "EF623" | ID == "RS723") & Score > 50]
    #
    ## 2. using `%in%`
    DT[ID %in% c("RS723", "EF623")]
    #
    ## 3. using `%chin%`, which leverages R's internal string cache
    DT[ID %chin% c("RS723", "EF623")]

    ## 4. Using keys. 

          .( )   syntax
          J( )  # same thing, older syntax
          -------------

    setkey(DT, ID)
    DT[.(c("RS723", "EF623")) ]
    setkey(DT, ID, State)
    DT[.("RS723", "NY")]
    DT[.("RS723", c("NY", "CA"))]  

    ## What if we want to select all rows where the
    ##        ID    is "RS723" or "EF623"
    ##   AND  State is "NY" or "CA"   ??
    ## 
    ## We might try... 
    DT[.(c("RS723", "EF623") , c("NY", "CA"))] # Wrong way
    ## But this is not what we want... 
    ##  the argument inside .( ) is similar to
    DT[, list(c("RS723", "EF623") , c("NY", "CA"))]

          CJ() == "CROSS JOIN"
          -----------
    ## Like    setkey( as.data.table( expand.grid(...) ) )
    CJ(c("RS723", "EF623") , c("NY", "CA"))    

    DT[CJ(c("RS723", "EF623") , c("NY", "CA"))]

    ## What if we want those same values but only with Score > 50
    DT[CJ(c("RS723", "EF623") , c("NY", "CA")) & Score > 30]  # Wrong way

    ## Daisychain 
    DT[CJ(c("RS723", "EF623") , c("NY", "CA"))][Score > 30]  

            Logical Vectors in the data.table
            ------------------------------------
    DT[, Approved := {set.seed(1); sample(c(TRUE, FALSE), .N, TRUE)}]
    DT

    DT[Approved]  # Wrong way

    ## If 'i=..' is a single variable, its scope is the same environment as where DT resides
    ## eg:
    Approved <- c(1, 3)
    DT[Approved] ## NOT what we want

    ## If 'i=..' is an expression, then its scope is the data.table
    DT[Approved==TRUE]
    DT[c(Approved)]
    ## .. or even just using plain parens
    DT[(Approved)]


    ## Remember, the general form is :
    ## 
    ##   DT[ where, select|update, group by ][ having ][ order by ][ ]...[ ]
    RowsUsing <- CJ(c("RS723", "EF623") , c("NY", "CA"))
    DT[RowsUsing] [Score > 30] [order(Type, Group)]

    DT[RowsUsing] [Score > 30, mean(Score), by=Type]

