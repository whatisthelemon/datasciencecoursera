best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(! state %in% data$State) stop ("invalid state")
    if(! outcome %in% colnames(data)) stop ("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    
    
    ## rate
    
}