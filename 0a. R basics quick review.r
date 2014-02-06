


MyList <- list(   A = 1:10
                , B = 101:120
                , C = 200:230
              )


lapply(MyList, mean)

lapply(MyList, function(x) {
            -2 * sd(x) + mean(x)
      })

lapply(MyList, function(x) {
          mn  <- mean(x)
          std <- sd(x)
          c('-2SD'=mn-(2*std), Mean=mn, '+2SD'=mn+(2*std))
      })


lapply(MyList, function(x) {
          print("Hi, I am in a function!")
          if (length(x) > 10) {
                    mn  <- mean(x)
                    std <- sd(x)
                    c('-2SD'=mn-(2*std), Mean=mn, '+2SD'=mn+(2*std))
                  }
      })

=============================

## a data.frame is simply a collection of lists

MyDF <- data.frame(A=1:10, B=11:20, C=21:30)
MyDF
lapply(MyDF, mean)

=============================


## binary operators

3 + 5
`+`(3, 5)  # 3 + 5
`*`(3, 5)  # 3 * 5

`==`(3, 5)  # 3 == 5
`==`(5, 5)  # 5 == 5

x <- 777
x
`<-`("x", 1:20)  # x <- 1:20
x


## Backticks
##
## We can create functions with our own funky symbols
## Just use backticks (or quotes, if you prefer)
'7!:xKL3#$' <- function(x) {
  cat("\nThis function is called '7!:xKL3#$'\nNotice we can even use hashes ('#')\nWhat a silly function... x is ", x, "\n\n")
} 

`7!:xKL3#$`(23)
'7!:xKL3#$'(23)
"7!:xKL3#$"(23)


=============================

## Difference between NULL & NA
## (very) loosely speaking:
NULL : "It's like I dont exist"
NA   : "I exist. But my value is missing, unknown or invalid"

      Types of NA:

## NA is logical
is(NA)
## What if we want to specify, say, a string? 
is(NA_character_)

## From the documentation: 
?NA
   "NA is a logical constant of length 1 which contains a missing value
    indicator. NA can be coerced to any other vector type except raw. 

    There are also constants NA_integer_, NA_real_, NA_complex_ and 
    NA_character_ of the other atomic vector types which support missing
     values: all of these are reserved words in the R language.
   "

## To recap, the NA constants are: 
NA_integer_
NA_real_
NA_complex_
NA_character_

## Question: Why is it 'NA_real_' and not 'NA_numeric_' ??
## Answer:   Because this is open source. 


