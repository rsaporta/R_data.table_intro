            ------------------------------------
             SPECIAL CONSTANTS WITHIN '[.data.table'
            ------------------------------------
            .I   =  index number (row number)
            .N   =  nrow() OF THE GROUP (not the whole DT)
            .SD  =  Subset of the Data
            .BY  =  By columns current value (for that group)
            .GRP =  an integer counter, which group are we upto
            ------------------------------------
            These values are _read only_
            They are available only in 'j=..'

sdcols
DT[  DT[<filter>, .I, by=..]$V1  ]


