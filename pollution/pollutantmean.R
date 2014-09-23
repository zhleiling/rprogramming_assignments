pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    files <- dir(directory)

    v_total = vector()
    
    for (x in id) {
        dataframe <- read.csv(paste(directory,"/",files[x],sep=""))
        colnames <- names(dataframe)
        for(i in 1:length(colnames)){
            if(colnames[i] == pollutant)   
                index <- i
        }
        v_total <- append(v_total,dataframe[[index]])
    }
    
    round(mean(v_total, na.rm = TRUE),3)
    
}