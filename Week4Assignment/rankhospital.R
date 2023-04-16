rankhospital <- function(state, outcome, num = "best") {
    ## Read in the data
    df <- read.csv("datafiles/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that the state is valid
    if (!(state %in% df$State)) {
        stop("Invalid state")
    }
    ## Now check that the outcome is valid
    ## from reading the pdf we know that the heart attack column is 11, 
    ## heart failure column is 17 and pneumonia column is 23
    if (outcome == "heart attack") {
        bh<-findBest(df[, c(2, 7, 11)] ,state)
    } else if (outcome == "heart failure") {
        bh<-findBest(df[, c(2, 7, 17)],state)
    } else if (outcome == "pneumonia") {
        bh<-findBest(df[, c(2, 7, 23)],state)
    } else {
        stop("Invalid outcome")
    }
    ## Check that we have enough valid hospitals in the data set
    if (is.numeric(num) & (num < nrow(bh))) {
        return(bh[as.numeric(num), 1])
    } else if (num == "best") {
        return(bh[1, 1])
    } else if (num == "worst") {
        return(bh[nrow(bh), 1])
    } else {
        return("NA")
    }
}

findBest <- function(df, state)  {
    ## Convert values to numeric, don't care for NA's at this phase
    df[, 3] <- as.numeric(df[, 3])
    ## now get rid of the NA's
    df <- df[which(df[,2] == state & df[,3] != "NA"), ]
    ## sort results and return sorted list
    df[order(df[3], df[1], df[2]), ]
}