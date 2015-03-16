corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    acceptedFileSlots = sapply(list.files(directory, full.names = TRUE), enough.cases, threshold)
    result <- sapply(list.files(directory, full.names = TRUE)[acceptedFileSlots], file.cor, USE.NAMES = FALSE)
    if (length(result) == 0) {
        return(numeric(0))
    }
    else {
        return(result)
    }
}

enough.cases <- function(datafile, requiredCases) {
    table <- read.csv(datafile)
    nrow(table[complete.cases(table),]) > requiredCases
}

file.cor <- function(datafile) {
    table <- read.csv(datafile)
    completeSlots <- complete.cases(table)
    cor(table[completeSlots,'sulfate'], table[completeSlots,'nitrate'])
}