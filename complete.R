complete <- function(directory, id = 1:332)
{
        # checking if the current directory is the same as the current
        # if so, we dont have to set the working directory
        # setting same directory results in error
        # assuming that if the current workspace ends with the directory entered
        # it is the same
        currDir <-
                substr(getwd(), nchar(getwd()) - nchar(directory) + 1, nchar(getwd()))
        if (!(currDir == directory))
        {
                #changing the directory to where the file is
                setwd(directory)
        }
        
        #initializing objects
        filetoload <- character()
        outputValue <- data.frame()
        
        for (i in seq_along(id))
        {
                # file names have a min of 3 characters, hence padding 0s
                filetoload <-
                        paste(sprintf("%03d", id[i]), ".csv", sep = "")
                # loading the file using the read.csv command
                srcData <- read.csv(filetoload)
                #appending the id and the complete.cases result
                outputValue = rbind(outputValue, c(id[i], nrow(srcData[complete.cases(srcData),])))
        }
        
        # setting column names
        colnames(outputValue) = c("id", "nobs")
        
        # displaying the final output
        print(outputValue)
}