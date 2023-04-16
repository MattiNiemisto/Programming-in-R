best <- function(state, outcome) {
    ## Read in the data
    df <- read.csv("datafiles/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that the state is valid
    if (!(state %in% df$State)) {
        msg<-paste(state, "not found in dataset!") 
        message(msg)
        return(-1)
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
        msg<-paste(outcome, "is not valid, please check your spelling!") 
        message(msg)
        return(-1)
    }
    ## Return the best Hospital
    bh[1, 1]
   

}

findBest <- function(df, state)  {
    ## Convert values to numeric, don't care for NA's at this phase
    df[, 3] <- as.numeric(df[, 3])
    ## now get rid of the NA's
    df <- df[which(df[,2] == state & df[,3] != "NA"), ]
    ## sort results and return sorted list
    df[order(df[3], df[1], df[2]), ]
}