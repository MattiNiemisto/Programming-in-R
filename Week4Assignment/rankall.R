rankall <- function(outcome, num = "best") {
    ## Read in the data
    df <- read.csv("datafiles/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that the num is valid
    if (!(num == "best" || num == "worst" || is.numeric(num))) {
        stop("Invalid rank")
    }
    ## Check whether we want ascending or descending sorting.
    asc <- TRUE
    if ( num == "worst") {
        asc <- FALSE
    } 
    ## Now check that the outcome is valid
    ## from reading the pdf we know that the heart attack column is 11, 
    ## heart failure column is 17 and pneumonia column is 23
    if (outcome == "heart attack") {
        bh<-sortHospitals(df[, c(2, 7, 11)], asc)
    } else if (outcome == "heart failure") {
        bh<-sortHospitals(df[, c(2, 7, 17)], asc)
    } else if (outcome == "pneumonia") {
        bh<-sortHospitals(df[, c(2, 7, 23)], asc)
    } else {
        stop("Invalid outcome")
    }
    ## Change column names to match what was asked for in the assignment
    colnames(bh) <- c("Hospital", "State", "Rank")
    ## Use lapply to iterate through the states. First split using the state as key
    rdf <- lapply(split(bh, bh$State), function(x, y, num){
        if (num == "best") {
            if (is.na(x[1, 3])) {
                data.frame("Hospital" = "NA", "State" = x[1,2], "Rank" = "NA")
            } else {
                x[1, ]
            }
        } else if (num == "worst") {
            if (is.na(x[1, 3])) {
                data.frame("Hospital" = "NA", "State" = x[1,2], "Rank" = "NA")
            } else {
                ## MIght seem counter intuitive to return the first item but
                ## remember we changed sorting order for num = worst
                x[1, ]
            }            
        } else {
            if (is.na(x[num, 3])) {
                data.frame("Hospital" = "NA", "State" = x[1,2], "Rank" = "NA")
            } else {
                x[num, ]
            }
        }
        }, num = num)
    resultFrame <- do.call("rbind", rdf)
    return(resultFrame)
}

sortHospitals <- function(df, asc)  {
    ## Convert values to numeric, don't care for NA's at this phase
    df[, 3] <- as.numeric(df[, 3])
    ## sort results and return sorted list
    if(asc == TRUE) {
        ## Sort ascending for num = best and num = numeric value
        bh<-df[order(df[2], df[3], df[1]), ]
    } else {
        ## sort descending for num = worst
        bh<-df[order(df[2], -df[3], df[1]), ]
    }
    return(bh)
}