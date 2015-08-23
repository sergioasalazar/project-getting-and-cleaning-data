library(plyr)
library(data.table)
library(dplyr)

fname <- "gd_dataset.zip"

# Download and unzip dataset
if(!file.exists(fname)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, fname)
}
if(!file.exists("UCI HAR Dataset")){
  unzip(fname)
}

YTest <- read.table("UCI HAR Dataset/test/y_test.txt")
XTest <- read.table("UCI HAR Dataset/test/X_test.txt")
SubjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
YTrain <- read.table("UCI HAR Dataset/train/y_train.txt")
XTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
SubjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
Features <- read.table("UCI HAR Dataset/features.txt")

# column names
colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])
# common ID
XTrain$activities <- YTrain[, 1]
XTrain$participants <- SubjectTrain[, 1]
XTest$activities <- YTest[, 1]
XTest$participants <- SubjectTest[, 1]

# merge datasets
Master <- rbind(XTrain, XTest)
duplicated(colnames(Master))
Master <- Master[, !duplicated(colnames(Master))]

# Extracts only the measurements on the mean and standard deviation for each measurement
Mean <- grep("mean()", names(Master), value = FALSE, fixed = TRUE)
Mean <- append(Mean, 471:477)
InstrumentMeanMatrix <- Master[Mean]
# STD
STD <- grep("std()", names(Master), value = FALSE)
InstrumentSTDMatrix <- Master[STD]

# Uses descriptive activity names to name the activities in the data set
Master$activities <- as.character(Master$activities)
Master$activities[Master$activities == 1] <- "Walking"
Master$activities[Master$activities == 2] <- "Walking Upstairs"
Master$activities[Master$activities == 3] <- "Walking Downstairs"
Master$activities[Master$activities == 4] <- "Sitting"
Master$activities[Master$activities == 5] <- "Standing"
Master$activities[Master$activities == 6] <- "Laying"
Master$activities <- as.factor(Master$activities)

# Appropriately labels the data set with descriptive variable names
names(Master) <- gsub("Acc", "Accelerator", names(Master))
names(Master) <- gsub("Mag", "Magnitude", names(Master))
names(Master) <- gsub("Gyro", "Gyroscope", names(Master))
names(Master) <- gsub("^t", "time", names(Master))
names(Master) <- gsub("^f", "frequency", names(Master))

# Create tidy dataset
Master.dt <- data.table(Master)
TidyData <- Master.dt[, lapply(.SD, mean), by = 'participants,activities']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)
