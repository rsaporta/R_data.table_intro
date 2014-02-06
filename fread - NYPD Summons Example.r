

## Download: 
## http://nypd.openscrape.com/#/summons.csv.gz
## http://nypd.openscrape.com/#/collisions.csv.gz
## http://nypd.openscrape.com/#/summons.csv.gz


  summons    <- fread("~/Desktop/DTCLASS/Data Sets/summons.csv")
  collisions <- fread("~/Desktop/DTCLASS/Data Sets/collisions.csv")

  summons <- as.data.table(reshape2::melt(summons, id.vars =c("geo", "year", "month")))


  system.time({
    summons[, c("description", "period") := 
                as.list(strsplit(as.character(variable), "_(?=[ym]td)", perl=TRUE)[[1L]])
            , by=variable]
    summons
  })

  system.time({
    summons[, c("description", "period") := 
                as.data.table(do.call(rbind, strsplit(as.character(variable), "_(?=[ym]td)", perl=TRUE)))
            ]
    summons
  })


  are(summons)
  summons[, c("description", "period") := 
              as.list(strsplit(as.character(variable), "_(?=[ym]td)", perl=TRUE)[[1L]])
          , by=variable]
  summons[, variable := NULL]

# ========================================= #

  cols <- c("borocode", "precinct", "year", "month", "lon", "lat", "street1", "street2")
  collisions <- as.data.table(reshape2::melt(collisions, id.vars=cols))
  setnames(collisions, "variable", "description")

# ========================================= #


desc.s <- unique(summons[["description"]])
desc.c <- levels(collisions[["description"]])


setkey(collisions, precinct)
collisions[.(60)]
collisions[.(123)] [!is.na(value)]

collisions[.(123)][, sum(value, na.rm=TRUE), by=list(precinct, description)]

intersect(desc.c, desc.s)
intersect(desc.c, desc.s)




br <- TRUE
(splat2 <- summons[, {.SD; browser(expr=br); 
list(setattr(strsplit(as.character(variable), "_(?=[ym]td)", perl=TRUE)[[1L]], "names", c("t1", "t2")))
#            unlist(strsplit(as.character(variable), "_(?=[ym]td)", perl=TRUE), use.names=FALSE)
           }
           , by=variable])


br <<- FALSE
c

rbind(setattr(strsplit(as.character(variable), "_(?=[ym]td)", perl=TRUE)[[1L]], "names", c("t1", "t2")))
variable




summons[, c("t1", "t2") :=  NULL]
(summons)

is(splat)
dim(splat)

c4(splat)
head(summons[, strsplit(as.character(variable), "_(?=[ym]td)", perl=TRUE)])


strsplit(as.character(variable), "(?=.)_([ym]=>?)", perl=TRUE)
