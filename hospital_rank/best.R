best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    stateData <- data$State[data$State == state]
    if(length(stateData) == 0){
        stop("invalid state");
    }
    
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    if(outcome == validOutcome[1]){
        colno <- 11
    }else if (outcome == validOutcome[2]){
        colno <- 17
    }else if (outcome == validOutcome[3]){
        colno <- 23
    }else{
        stop("invalid outcome");
    }
    ## Return hospital name in that state with lowest 30-day death rate
    subData <- subset(data,State == state & data[, colno] != "Not Available")
    rates <- as.numeric(subData[,colno])
    bestValue <- min(rates)
    bestHospitals <- subData$"Hospital.Name"[as.numeric(subData[,colno]) == bestValue]
    bestHospitals <- sort(bestHospitals)
    bestHospitals[1]
}