corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    files <- dir(directory)
    
    cr <- vector(mode = "numeric")
    i <- 1
        
    for(x in files){
        v <- read.csv(paste(directory,"/",x,sep=""))
        notna <- !is.na(v[[2]]) & !is.na(v[[3]])
        count <- length(v[[1]][notna])
        if(count > threshold){
            cr[i] <- cor(v[[2]][notna],v[[3]][notna])
            i <- i + 1
        }
    }
    
    cr
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
}