###############################################################################################################################################################################
# written by Arundhati Kulkarni
# 2014-08-22
# run_analysis 
############# project  assignment getting and cleaning data #############################
#
# This script does the following
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable 
#     for each activity and each subject. 

# 1. Merge two data set training and test to create one data set

wd <- getwd();

if (!is.null(wd)) { 
  setwd(wd)
} else {
  stop("Working directory path is not set.")
}

# Read the data from file .
featuresData     <- read.table('./Dataset/features.txt',header=FALSE)
activityTypeData <- read.table('./Dataset/activity_labels.txt',header=FALSE) #imports activity_labels.txt
subjectTrainData <- read.table('./Dataset/train/subject_train.txt',header=FALSE) #imports subject_train.txt
xTrainData       <- read.table('./Dataset/train/x_train.txt',header=FALSE) #imports x_train.txt
yTrainData       <- read.table('./Dataset/train/y_train.txt',header=FALSE) #imports y_train.txt
#Reading Test data
subjectTest <- read.table('./Dataset/test/subject_test.txt',header=FALSE) #imports subject_test.txt
xTest       <- read.table('./Dataset/test/x_test.txt',header=FALSE) #imports x_test.txt
yTest       <- read.table('./Dataset/test/y_test.txt',header=FALSE) #imports y_test.txt

# Assigning the column name to we imported
colnames(activityTypeData)  <- c('activityId','activityType')
colnames(subjectTrainData)  <- "subjectId"
colnames(xTrainData)        <- featuresData[,2] 
colnames(yTrainData)        <- "activityId"

# create a final training set by cbind
trainingData <- cbind(yTrainData,subjectTrainData,xTrainData)

# assigning column names to test data 
colnames(subjectTest) <- "subjectId"
colnames(xTest)       <-  featuresData[,2]
colnames(yTest)       <-  "activityId"

# now combinging all testData together
testData <- cbind(yTest,subjectTest,xTest)

# combine Training  and Test Data together
finalData <- rbind(trainingData,testData)

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  <- colnames(finalData) 

# 2. Extract the only measurements of mean and standard deviation.

# create a vector which contains true values for Id mean stddev() columns and false for othe
LVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset finalData table based on the lVector to keep only desired columns
finalData <- finalData[LVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData <- merge(finalData,activityTypeData,by='activityId',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
colNames  <- colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  <- finalData[,names(finalData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
cleanAndTidyData    <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
cleanAndTidyData    <- merge(cleanAndTidyData,activityTypeData,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(cleanAndTidyData, 'CleanAndTidyData.txt',row.names=TRUE,sep='\t')

