# Write a function that reads a directory full of files and reports the number of completely observed 
# cases in each data file. The function should return a data frame where the first column is the name
# of the file and the second column is the number of complete cases.

#
# UNIT TESTS
#
# source("complete.R")
# complete("specdata", 1)
##   id nobs
## 1  1  117
# complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
# complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
# complete("specdata", 3)
##   id nobs
## 1  3  243

complete <- function(directory, id = 1:332) {
    prepareFileNames <- function(id = 1:332) {
        filename <- formatC(id, width=3, format="d", flag="0")
        paste(directory, "/", filename, ".csv", sep = "")
    }  
    filesToProcess <- prepareFileNames(id)
    
    df = data.frame()
    for (file in filesToProcess) {    
        t <- read.csv(file)
        good <- complete.cases(t)
        g <- t[good,]
        r <- data.frame(as.numeric(substr(file, 10, 12)), nrow(g))
        df <- rbind(df, r)
    }
    
    names(df) <- c("id", "nobs")
    df
}
