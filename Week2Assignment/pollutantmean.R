#Coursera Week2 Programming Assignment
##Air Pollution in USA
pollutantmean <- function(directory, pollutant, id=1:332){
    ##directory is the path for the data file. Char vector with lenght of 1.
    ##pollutant is the pollutant we want to calculate the mean for. 
    ##pollutant is a Character vector with length of 1.
    ##id is the identifier for the measurement place. 
    ##id is an Integer vector between 1 and 332.
    ##Function returns the mean for a pollutant across all monitors list
    ##in the id vector. NA values are ignored.
    ##Like instructed results have not been rounded

    ##First check that the input is valid
    if(!dir.exists(directory)) {
        print("Directory not found! The value for directory is not valid" )
        return(-1);
    }
    if(!(pollutant=="sulfate" || pollutant=="nitrate")) {
        print("Pollutant not found! Check pollutant spelling, only sulfate and nitrate are supported.")
        return(-1);
    }
    if(id[1]<1 || id[length(id)] > 332 || (is.numeric(id) & (id<1 || id>332))) {
        print("The given value is beyond measurement points. Please specify a range or single value between 1 to 332.")
        return(-1);
    }
    ##OK, all input checks passed, lets start the data crunching!
    numMonitors <- length(id)
    dataFrames <- list(numMonitors)
    means <- numeric(numMonitors)
    numValidMeasurements <- numeric(numMonitors)
    i<-1
    for(x in id) {
        if (x < 10)
        {
            csvName <- paste(directory, "/00", x, ".csv", sep="" ) 
        }
        else if (x < 100)
        {
            csvName <- paste(directory, "/0", x, ".csv", sep="" )
        }
        else 
        {
            csvName <- paste(directory, "/", x, ".csv", sep="" )
        }
        df <- read.csv(csvName)
        ##calculate mean for each monitor
        ##Check that there is atleast one correct measurement
        if(sum(!is.na(df[[pollutant]])) > 0){
            means[i]<-mean(df[[pollutant]], na.rm=TRUE)
            ##calculate also number of valid measurements, used to weight the score
            numValidMeasurements[i] <- sum(!is.na(df[[pollutant]]))
            i <- i + 1
        }
    }
    ##Calculate the mean of means. Weighted by # of valid data points
    ##NOTE:if numValidMeasurements is 0, then return 0
    if(sum(numValidMeasurements) == 0)
    {
        return(0)
    } 
    else 
    {
        allMeans <- means * numValidMeasurements / sum(numValidMeasurements)
        sum(allMeans)
    }
    ##And we're done!!!
}