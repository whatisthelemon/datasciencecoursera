rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #convert everything to uppercase so don't have to worry about case
    names(data) <- toupper(names(data))
    outcome <- toupper(outcome)
    
    #convert "outcome" to proper column name
    outcome <- strsplit(outcome, split=" ")
    outcome <- outcome[[1]]
    #add period in between if it's two words
    if (length(outcome) == 2) outcome <- paste(outcome[1], outcome[2], sep=".")
    outcome <- paste("HOSPITAL.30.DAY.DEATH..MORTALITY..RATES.FROM.", outcome, sep="")
    
    ## Check that outcome is valid
    if(! outcome %in% colnames(data)) stop ("invalid outcome")
    
    #make sure column is numeric
    data[,outcome] <- as.numeric(data[,outcome])
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    #get state names
    state <- sort(unique(data[,"STATE"]))
    hospital <- rep(NA, length(state))
    ret <- cbind(hospital, state)
    
    i <- 0
    
    for(curState in state){
        i <- i + 1
        #find ranking hospital for that state
        stateData <- data[data[,"STATE"] == curState,]
        
        #check if there are enough values
        if(is.numeric(num) && (num>length(stateData[,outcome]) || num < 1) ) next
        
        #figure out desired rank from input
        rank<-if(num=="best"){
            1
        }
        else if(num=="worst"){
            sum(!is.na(data[,outcome]))
        }
        else{
            as.numeric(num)
        }
        
        #get the ordering permutation
        ordered <- order(-stateData[,outcome], stateData[,"HOSPITAL.NAME"])
        
        index <- ordered[rank]
        
        ret[i,"hospital"] <- stateData[index,"HOSPITAL.NAME"]
    }
    
    return (ret)
}