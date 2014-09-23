rankhospital <- function(state, outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character", na.strings = "Not Available")
    
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
    
    ## Return hospital name in that state with the given rank 30-day death rate
    subData <- subset(data,State == state & !is.na(data[, colno]),
                      c("State", "Hospital.Name", names(data)[colno]), )
    rankResult <- subData[ order(as.numeric(subData[,3]),
                                 subData$"Hospital.Name"), ]
    if (num == "best"){
        result <- rankResult$"Hospital.Name"[1]
    }else if (num == "worst"){
        result <- rankResult$"Hospital.Name"[nrow(rankResult)]
    }else {
        result <- rankResult$"Hospital.Name"[num]
    }
    
    result
}