pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    #open from directory
    data <- extractmulticsv(directory)
    
    #figure out which ids
    onestoselectfrom <- data$ID %in% id
    
    #extract mean
    return (mean(data[, pollutant][onestoselectfrom], na.rm=TRUE))
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
    