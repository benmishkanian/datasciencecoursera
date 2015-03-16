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
    
    sum <- 0
    count <- 0
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
        # Remove NA entries
        cleandata <- table[!is.na(table[,pollutant]),pollutant]
        # Count entries
        count <- count + length(cleandata)
        # get sum
        sum <- sum + sum(cleandata)
    }
    # return the average, rounded to 3 decimal places
    round(sum / count, 3)
}