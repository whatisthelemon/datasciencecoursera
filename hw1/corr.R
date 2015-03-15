corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    
    #extract data from csv
    data <- extractmulticsv(directory)
    
    #find list of IDs
    ids <- unique(data$ID)
    
    #initialize vector to return
    cr <- vector('numeric')
    
    #loop through each id
    for(i in ids){
        #set temporary data
        tempdata <- data[data$ID==i,]
        
        #check if we exceed threshold
        completes <- complete.cases(tempdata)
        if(sum(completes) < threshold) next
        
        #prune data and compute correlation
        tempdata <- tempdata[completes,]
        cr <- c(cr, cor(tempdata$sulfate, tempdata$nitrate))
    }
    
    #remove nas and return
    cr <- cr[!is.na(cr)]
    return(cr)
}


extractmulticsv <- function(directory="."){
    filenames <- list.files(path=directory)
    filenames <- paste(directory, "/", filenames, sep="")
    data <- read.csv(filenames[1])
    for(i in 2:length(filenames)){
        data <- rbind(data, read.csv(filenames[i]))
    }
    return (data)
}