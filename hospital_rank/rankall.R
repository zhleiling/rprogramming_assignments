rankall <- function(outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",
                     colClasses = "character", na.strings = "Not Available")
    
    ## Check that state and outcome are valid
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    f <- factor(data$"State")
    l <- levels(f)
    result <- data.frame(hospital = vector(), state = vector())
    
    for(i in 1:length(l)){
        state <- l[i]
        subData <- subset(data,State == state & !is.na(data[, colno]),
                          c("State", "Hospital.Name", names(data)[colno]), )
        rankResult <- subData[ order(as.numeric(subData[,3]),
                                     subData$"Hospital.Name"), ]
        if (num == "best"){
            subresult <- rankResult$"Hospital.Name"[1]
        }else if (num == "worst"){
            subresult <- rankResult$"Hospital.Name"[nrow(rankResult)]
        }else {
            subresult <- rankResult$"Hospital.Name"[num]
        }
        result[i,] <- c(subresult, state)
        row.names(result)[i] <- state
    }
    
    result
}