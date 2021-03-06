complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    data <- extractmulticsv(directory)
    completes <- complete.cases(data)
    
    nobs <- vector('integer')
    
    for(i in id){
        temp <- data$ID==i & completes
        s <- sum(temp)
        nobs <- c(nobs, s)
    }
    
    return(data.frame(id, nobs))
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