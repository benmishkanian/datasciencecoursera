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
    
    dataframe <- data.frame(id = as.numeric(character()),
                            nobs = as.numeric(character()),
                            stringsAsFactors=FALSE)
    for (filenumber in id) {
        # construct file name
        datafile <- if (filenumber < 10) {
            paste(directory, "/00", filenumber, ".csv", sep="")
        }
        else if (filenumber < 100) {
            paste(directory, "/0", filenumber, ".csv", sep="")
        }
        else {
            paste(directory, "/", filenumber, ".csv", sep="")
        }
        # read file
        table <- read.csv(datafile)
        # count non-NA entries
        goodnitratestable <- table[!is.na(table$nitrate),]
        goodentries <- length(goodnitratestable[!is.na(goodnitratestable$sulfate),"sulfate"])
        # add results to table
        dataframe <- rbind(dataframe, data.frame(id = filenumber, nobs = goodentries))
    }
    dataframe
}