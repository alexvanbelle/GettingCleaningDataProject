# this script enables to read the data from UCI HAR Dataset and to create a tidy data set
# please read the README.md file to get more details 

# we expect to have the full data set in a folder of working folder
# url of the data set: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
library(plyr)
setwd("J:/cours/gettingandcleaningdata/GettingCleaningDataProject")

# this function does step 1 to 4:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
LoadData <- function(){
    # subfolder can be either "train" or "test"
    LoadSubData <- function(subfolder = "train"){
        setPath <- paste(folderData, subfolder, paste("X_", subfolder, ".txt", sep=""), sep=pathSeparator)
        labelPath <- paste(folderData, subfolder, paste("Y_", subfolder, ".txt", sep=""), sep=pathSeparator)
        subjectPath <- paste(folderData, subfolder, paste("subject_", subfolder, ".txt", sep=""), sep=pathSeparator)
        
        # read the set and assign it directly the right colnames based on features.txt
        set <- read.table(setPath, col.names = features$value)
        label <- read.table(labelPath, col.names = c("activityID"))
        subject <- read.table(subjectPath, col.names = c("subject"))
        # we bind before any merge since merge can reorder the data
        set <- cbind(subject, set, label)
        set
    }
    
    folderData <- "UCI HAR Dataset"
    pathSeparator <- "/"
    
    activityLabelPath <- paste(folderData, "activity_labels.txt", sep=pathSeparator)
    activityLabels <- read.table(activityLabelPath, col.names = c("activityID", "activity"))
    
    featurePath <- paste(folderData, "features.txt", sep="/")
    features <- read.table(featurePath, col.names = c("featureID", "value"))
    
    fullSet <- rbind(LoadSubData("train"), LoadSubData("test"))
    
    #determine columns to keep
    names <- colnames(fullSet)
    indexImportant <- names == "subject" | names == "activityID" | grepl(".*[.](mean|std)[.].*", names)
    # keep the right columns
    fullSet <- fullSet[,indexImportant]
    
    # subject should be considered as factor because it's a way to clearly identify a subject
    fullSet$subject <- as.factor(fullSet$subject)
        
    # merge and remove the activityID since we already have the activity
    fullSet <- merge(activityLabels, fullSet, by = "activityID")
    fullSet <- fullSet[,colnames(fullSet) != "activityID"]
    fullSet
}

# this function creates the tidy data as it is described on step 5. 
# It expects the data to be in the format described in previous steps
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
createFinalTidy <- function(data){
    RenameIfRelevant <- function(columnName){
        if (columnName == "subject" | columnName == "activity")
            return(columnName)
        return(paste("meanOf.", columnName, sep=""))
    }
    
    # compute the mean of each column for any combination of subject and activity
    tidy <- ddply(data, .(subject,activity), colwise(mean))
    
    #rename all columns except the subject and the activity to reflect the transformation in the name
    names <- colnames(tidy)
    colnames(tidy) <- sapply(names, RenameIfRelevant, USE.NAMES= FALSE)
    tidy
}

data <- LoadData()
tidy <- createFinalTidy(data)
write.table(tidy, file = "step5_tidy.txt", row.names = FALSE)

