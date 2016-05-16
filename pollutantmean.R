# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate)
# across a specified list of monitors.  The function 'pollutantmean' takes three arguments:
# 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that
# monitors' particulate matter data from the directory specified in the 'directory' argument and
# returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

#
# UNIT TESTS
#
# source("pollutantmean.R")
# pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
# pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
# pollutantmean("specdata", "nitrate", 23)
## [1] 1.281
#
pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    prepareFileNames <- function(id = 1:332) {
        filename <- formatC(id, width=3, format="d", flag="0")
        paste(directory, "/", filename, ".csv", sep = "")
    }  
    filesToProcess <- prepareFileNames(id)

    df = data.frame()
    for (file in filesToProcess) {
        t <- read.csv(file)
        df <- rbind(df, t)
    }

    c <- df[(!is.na(df[[pollutant]])),][,]
    mean(c[[pollutant]])
}
