XTrain <- XTest <- NULL

runAnalysis <- function() {
  # Getting and extracting SmartLab Human Activity recognition data in UCI HAR format

  filePath <- function(...) { paste(..., sep = "/") }

  downloadData <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    downloadDir <- "C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles"

    zipFile <- filePath(downloadDir, "dataset.zip")
    if(!file.exists(zipFile)) { download.file(url, zipFile, method = "curl") }

    dataDir <- "UCI HAR Dataset"
    if(!file.exists(dataDir)) { unzip(zipFile, exdir = ".") }

    dataDir
  }

  dataDir <- downloadData()

  # Merge the training and the test sets to create one data set.

  readData <- function(path) {
    read.table(filePath(dataDir, path))
  }

  # Read and cache XTrain and XTest data
 
  if(is.null(XTrain)) { XTrain <<- read.table("C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles/UCI HAR Dataset/train/X_train.txt") }
  if(is.null(XTest))  { XTest  <<- read.table("C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles/UCI HAR Dataset/test/X_test.txt") }
  merged <- rbind(XTrain, XTest)

  featureNames <- read.table("C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles/UCI HAR Dataset/features.txt")[, 2]
  names(merged) <- featureNames

  # Extract only the measurements on the mean and standard deviation for each measurement.
  # Limit to columns with feature names matching mean() or std():
  matches <- grep("(mean\\(\\)|std)\\(\\)", names(merged))
  ltddata <- merged[, matches]

  # Use descriptive activity names to name the activities in the data set.
  # Get the activity data and map to descriptive names:
  yTrain <- read.table("C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles/UCI HAR Dataset/train/y_train.txt")
  yTest  <- read.table("C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles/UCI HAR Dataset/test/y_test.txt")
  yMerged <- rbind(yTrain, yTest)[, 1]

  activityNames <-
    c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
  activities <- activityNames[yMerged]

  # Appropriately label the data set with descriptive variable names.
  # Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
  # Remove extra dashes and BodyBody naming error from original feature names
  names(ltddata) <- gsub("^t", "Time", names(ltddata))
  names(ltddata) <- gsub("^f", "Frequency", names(ltddata))
  names(ltddata) <- gsub("-mean\\(\\)", "Mean", names(ltddata))
  names(ltddata) <- gsub("-std\\(\\)", "StdDev", names(ltddata))
  names(ltddata) <- gsub("-", "", names(ltddata))
  names(ltddata) <- gsub("BodyBody", "Body", names(ltddata))

  # Include activities and subject with readable names
  subjectTrain <- read.table("C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles/UCI HAR Dataset/train/subject_train.txt")
  subjectTest  <- read.table("C:/Users/hemanthg/Google Drive/CU/Coursera/DownloadFiles/UCI HAR Dataset/test/subject_test.txt")
  subjects <- rbind(subjectTrain, subjectTest)[, 1]

  tidy <- cbind(Subject = subjects, Activity = activities, ltddata)

  # Create a second, independent tidy data set with the average of each variable for each activity and each subject.
  library(plyr)
  # Column means for all but the subject and activity columns
  limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
  tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
  names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])

  # Write file
  write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE)

  # Also return data
  tidyMeans
}

# Use to check that the tidyMeans.txt is properly readable
checkData <- function() {
  read.table("tidyMeans.txt", header = TRUE)
}
