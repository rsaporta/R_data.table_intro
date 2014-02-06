On stackoverflow today:
http://stackoverflow.com/questions/21611040/reshape-in-r-without-aggregation-for-example-mturk-response-strings

## DATA: 
mturk <- data.table(structure(list(AssignmentStatus = c("Approved", "Approved", "Approved","Submitted", "Approved", "Approved"), Input.id = c(134231L, 134231L,134231L, 134812L, 134812L, 134812L), Input.State = c("NY", "NY","NY", "CA", "CA", "CA"), Answer.Q1thing = c("Myguess", "Myguess","BadGuess", "Another", "Another", "Another")), .Names = c("AssignmentStatus","Input.id", "Input.State", "Answer.Q1thing"), class = c("data.table","data.frame"), row.names = c(NA, -6L)))

If your data is in a `data.table` it is a one-liner:

    library(data.table)    
    mturk.dt <- as.data.table(mturk)

    mturk.dt[, as.list(
                rbind(c(Answer.Q1thing, AssignmentStatus))
               )
            , by=list(Id=Input.id, State=Input.State)]


Note that the `by` argument handles the name-changing too!   

-----

If you want to properly name the other columns, use `setnames` after the fact or, more dynamically, using `setattr`  within the `j=..` argument as follows: 

### After the Fact: 

    ## Assuming 'res' is the reshaped data.table form above:
    ## Change the names of the six V1, V2.. columns 
    setnames(res, paste0("V", 1:6), c(paste0("Answer", 1:3), paste0("Status", 1:3)))

### Dynamically, in `j=..`

    ## Use `as.data.table` instead of `as.list`, to preserve new names
    mturk.dt[, as.data.table(
             rbind(c(
                  setattr(Answer.Q1thing,   "names", paste0("Answer", seq(Answer.Q1thing  )))
                , setattr(AssignmentStatus, "names", paste0("Status", seq(AssignmentStatus)))
                ))
             )
            , by=list(Id=Input.id, State=Input.State)]

           Id State Answer1 Answer2  Answer3  Status1  Status2  Status3
    1: 134231    NY Myguess Myguess BadGuess Approved Approved Approved
    2: 134812    CA Myguess Myguess BadGuess Approved Approved Approved


mturk.dt[, as.list(rbind(c(Answer.Q1thing, AssignmentStatus))), by=list(Id=Input.id, State=Input.State)]