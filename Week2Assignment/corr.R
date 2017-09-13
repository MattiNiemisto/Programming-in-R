corr <- function(directory, treshold = 0){
     ##Check that the directory exists
     if(!dir.exists(directory)) {
        print("Directory not found! The value for directory is not valid" )
        return(-1);
    }
    ##OK, directory found. Check that requested files are available
    datafiles <- dir(directory)
    ##Create empty data.frame with correct header
    cordata<-numeric(0)
    ##Loop through all data files
    for(x in datafiles){
        csvName <- paste(directory, "/", x, sep="" )
            
        ##Read in the file
        df <- read.csv(csvName)
        ## Build logic vector of both nitrate and sulfate presenting if the value is present
        nitrate <- !is.na(df$nitrate)
        sulfate <- !is.na(df$sulfate)
        ##Complete cases are the one where both are present, hence the AND operation
        ##to get the sum we also need to, well, sum up the result
        calNobs <- sum(nitrate & sulfate)
        ##store value if we are above the treshold
        if(calNobs > treshold) {
            ##Calculate correlation for this monitor
            cordata[length(cordata) + 1] <- cor(df$sulfate, df$nitrate, use="na.or.complete")
        }
        ##Clear csv name as the data is already acquired
        csvName <- ""
    }

    ##Return the results
    cordata
    ##And we're done!!!
}