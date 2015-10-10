corr <- function(directory, threshold = 0) 
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
        outputValue <- numeric()
        
        # looping through all the files
        for (filetoload in list.files())
        {
                # loading the file
                currFile <- read.csv(filetoload)
                # considering only complete cases (ommiting NAs)
                currFile <- currFile[complete.cases(currFile),]
                if(nrow(currFile) > threshold)
                {
                        #print(paste (filetoload, "selected", nrow(currFile)))
                        # appending values
                        outputValue <- c(outputValue, cor(currFile$sulfate, currFile$nitrate))
                }
                else
                {
                        #print(paste (filetoload, "ignored", nrow(currFile)))
                }
                #break
        }
        
        # returning the final output
        return (outputValue)
}