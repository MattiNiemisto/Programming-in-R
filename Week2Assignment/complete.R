complete <- function(directory, id = 1:332){
    ##Directory is the folder where the data files are
    ##it is a character vector of length 1
    ##
    ##id is a integer vector indicating the monitor points to
    ##to be observed
    ##
    ##Function will calculate how many complete observations there
    ##are in the given monitor set. Result format:
    ##ID, nobs
    ##where ID is the monitor number and nobs # complete observations

    ##Check that the directory exists
     if(!dir.exists(directory)) {
        print("Directory not found! The value for directory is not valid" )
        return(-1);
    }
    ##OK, directory found. Check that requested files are available
    datafiles <- dir(directory)
    ##Create empty data.frame with correct header
    x <- c("id", "nobs")
    nobs <- as.data.frame(matrix(,0,length(x)))
    names(nobs) <- x
    ##Loop through the given data set
    for(x in id){
        if (x < 10) {
            csvName <- paste(directory, "/00", x, ".csv", sep="" )
        } else if (x < 100) {
            csvName <- paste(directory, "/0", x, ".csv", sep="" )
        } else {
            csvName <- paste(directory, "/", x, ".csv", sep="" )
        }
        if(!file.exists(csvName))
        {
            ErrorMessage = paste("ERROR: specified file ", csvName, " not found in data library")
            print(ErrorMessage)
            return(-1)
        } else {
            ##Read in the file
            df <- read.csv(csvName)
            ## Build logic vector of both nitrate and sulfate presenting if the value is present
            nitrate <- !is.na(df$nitrate)
            sulfate <- !is.na(df$sulfate)
            ##Complete cases are the one where both are present, hence the AND operation
            ##to get the sum we also need to, well, sum up the result
            calNobs <- sum(nitrate & sulfate)
            ##store value
            nobs[nrow(nobs) + 1,] <- c(x, calNobs)
        }
        ##Clear csv name as the data is already acquired
        csvName <- ""
    }
    ##Return nobs
    nobs
    ##And we're done!!!
}