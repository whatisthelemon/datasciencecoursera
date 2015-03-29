best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #convert everything to uppercase so don't have to worry about case
    names(data) <- toupper(names(data))
    state <- toupper(state)
    outcome <- toupper(outcome)
    
    #convert "outcome" to proper column name
    outcome <- strsplit(outcome, split=" ")
    outcome <- outcome[[1]]
    #add period in between if it's two words
    if (length(outcome) == 2) outcome <- paste(outcome[1], outcome[2], sep=".")
    outcome <- paste("HOSPITAL.30.DAY.DEATH..MORTALITY..RATES.FROM.", outcome, sep="")
    
    ## Check that state and outcome are valid
    if(! state %in% data$STATE) stop ("invalid state")
    if(! outcome %in% colnames(data)) stop ("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    #first, make sure column is numeric
    data[,outcome] <- as.numeric(data[,outcome])
    
    #find lowest value
    minval <- min(data[,outcome][data$STATE == state], na.rm=TRUE)
    
    #find names that have lowest value
    hospnames <- data[,"HOSPITAL.NAME"][data[,outcome] == minval & data[,"STATE"] == state]
    hospnames <- hospnames[!is.na(hospnames)]
    
    #return the one that's first in alphabetical order
    return(min(hospnames))
}