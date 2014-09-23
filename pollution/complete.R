complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    files <- dir(directory)
    
    count <- vector()
    i <- 1
    
    for(x in id){
        v <- read.csv(paste(directory,"/",files[x],sep=""))
        count[i] <- length(v[[1]][!is.na(v[[2]]) & !is.na(v[[3]])])
        i <- i + 1
    }
    
    result <- data.frame(id=id,nobs=count)
    result
    
}