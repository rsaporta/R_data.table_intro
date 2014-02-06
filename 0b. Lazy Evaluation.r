Lazy Evaluation



Func1 <- function(x=5, x2=x^2) {
  x <- x * 100
  y <- x2

  cat("x  is : ",  x,  "\n") 
  cat("y  is : ",  y,  "\n\n") 

}

Func2 <- function(x=5, x2=x^2) {
  y <- x2
  x <- x * 100

  cat("x  is : ",  x,  "\n") 
  cat("y  is : ",  y,  "\n\n") 
}


Func1()
Func2()

==========================================

Scoping & get()


Func3 <- function(y) {
  x * y
}

Func4 <- function(y) {
  x <- 7.7
  x * y
}

x <- 5
Func3(10)
Func4(10)

Func5 <- function(y) {
  x <- 7.7
  get("x") * y
}

Func6 <- function(y) {
  x <- 7.7
  get("x", envir=parent.frame()) * y
}

x <- 5
Func5(10)
Func6(10)

###   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ###
 "GOTCHA to watch out for with get(.) "
###   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ###

    ## get(.)  requires a STRING
    ##
    ## IF the variable you are getting has a string value
    ##   AND you forget to quote it in get(.)
    ##   AND its first value is also the name of another object
    ## THEN you will get unexpected results, 
    ##      possibly even a hard-to-catch bug 

    ID <- c("A", "B", "C") 
    A <- 123456
    B <- 777432

    get(ID)  # no error thrown, but not what is expected
    get("ID")

==========================================

        { BRACES }

# clear any previous x
rm(x)


## What will x be? 
x <- {
        y = 10
        z = y * 7.7
        z
     } 

rm(x)
x <- {
        y = 10
        z = y * 7.7
     } 
# Why?  

rm(x)
x <- {
      if (TRUE)
        3 + 5
     }
x

rm(x)
x <- {
      if (FALSE)
        3 + 5
     }
x


rm(x)
x <- {
      if (FALSE)
        3 + 5
      else
        "spaghetti"
     }
x

## Last one 
x <- {
        yy <- 101:107
        bb <- LETTERS[1:7]
        list(Ys=yy, Bs=bb)
     }
x

















