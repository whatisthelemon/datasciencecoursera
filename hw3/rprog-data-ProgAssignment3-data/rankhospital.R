rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    #truncate data to only state we're interested in
    data <- data[data[,"STATE"] == state,]
    
    #at this point, make sure there are enough values for ranking
    if(is.numeric(num) && (num>length(data[,outcome]) || num < 1) ) return (NA)
    
    #make sure column is numeric
    data[,outcome] <- as.numeric(data[,outcome])
    
    #get the ordering permutation
    ordered <- order(data[,outcome], data[,"HOSPITAL.NAME"])
    #for help: http://stackoverflow.com/questions/2315601/understanding-the-order-function-in-r
    
    rank<-if(num=="best"){
        1
    }
    else if(num=="worst"){
        sum(!is.na(data[,outcome]))
    }
    else{
        as.numeric(num)
    }
    
    index <- ordered[rank]
    
    return (data[index, "HOSPITAL.NAME"])
}