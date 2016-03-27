# Coursera: getting and cleaning data, course project, 24th March 2016

# Set working directory
setwd("/home/cristian/Documents/PhD/Lectures/Coursera/getAndCleanData/courseProject/UCI HAR Dataset/")

# Import activity labels
activityLabels <- read.table("activity_labels.txt")

# Import the 561 features
features <- read.table("features.txt")
featuresNames <- features[, 2]
length(featuresNames)
unique(featuresNames)
which(duplicated(featuresNames))
featuresNames[which(duplicated(featuresNames))]
featuresNames[303] == featuresNames[317]
head(trainSet[305] == trainSet[319])

## Import data for the training dataset
# Import train subjects (volunteer number)
trainSubjects <- read.table("train/subject_train.txt")
colnames(trainSubjects) <- "subjectNumber"
str(trainSubjects)
summary(trainSubjects)
table(trainSubjects)
# Import training labels
trainLabels <- read.table("train/y_train.txt")
colnames(trainLabels) <- "activity"
str(trainLabels)
summary(trainLabels)
# Import training set
trainSet <- read.table("train/X_train.txt")
str(trainSet)
colnames(trainSet)
# Name the columns of the training set
colnames(trainSet) <- featuresNames
# Bind the training data together, including subject number, activity and features
trainAll <- cbind(trainSubjects, trainLabels, trainSet)

## Import data for the testing dataset
# Import test subjects
testSubjects <- read.table("test/subject_test.txt")
colnames(testSubjects) <- "subjectNumber"
str(testSubjects)
summary(testSubjects)
table(testSubjects)
# Import testing labels
testLabels <- read.table("test/y_test.txt")
colnames(testLabels) <- "activity"
str(testLabels)
summary(testLabels)
# Import testing set
testSet <- read.table("test/X_test.txt")
str(testSet)
# Name the columns of the testing set
colnames(testSet) <- featuresNames
# Bind the testing data together, including subject number, activity and features
testAll <- cbind(testSubjects, testLabels, testSet)

# Merge the training and testing sets
# The training set and testing set contain different records so that I will append
# the testing set at the end (bottom) of the training set using rbind()
all(colnames(trainAll) == colnames(testAll))
setTrainTest <- rbind(trainAll, testAll)
# There are columns with the same name. I remove one of the homonymous columns
dupCols <- which(duplicated(colnames(setTrainTest)))
setUnique <- setTrainTest[, -dupCols]

# Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
meanStdCols <- c(grep("mean", colnames(setUnique)), grep("std", colnames(setUnique)))
meanStdMeasurements <- setUnique[, meanStdCols]

# Uses descriptive activity names to name the activities in the data set
for (i in 1:6) {
    setUnique$activity[setUnique$activity == activityLabels[i, 1]] <-
        as.character(activityLabels[i, 2])
}

# Appropriately labels the data set with descriptive variable names
# Remove the "()" and "," from the column names as they pose problems
colnames(setUnique) <- 
    gsub(",", "", gsub("\\)", "", gsub("\\(", "", colnames(setUnique))))

# From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
library(dplyr)
setUnique <- tbl_df(setUnique)

setUnique[, lapply(.SD, mean, by = subjectNumber)]

setSubjectActivity <- group_by(setUnique, subjectNumber, activity)
setSummary <- summarize_each(setSubjectActivity, funs(mean))
head(setSummary)
str(setSummary)

# Export this dataset in a text file
# Open the file in a spreadsheet with space as a field separator
write.table(setSummary, "summaryDataSet.text", row.names = FALSE)
