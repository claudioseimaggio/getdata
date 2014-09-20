## HOUSEKEEPING
##
setwd("d:/Project-getdata")
if (file.exists(".RData")) load(".RData")
library(dplyr)
library(tidyr)
# remove n=100, nrow=100

##
## STEP 1: Merges the training and the test sets to create one data set.
##

## I've red the label of the columns (features.txt)
colNames<-read.table("UCI HAR Dataset/features.txt", sep=" ", header=FALSE)
colWidths=rep(16,length(colNames[,2]))
#
## I've red the measurements file from test (X_test.txt)
testx <- read.fwf("UCI HAR Dataset/test/X_test.txt", colWidths, header=FALSE, n=100)
#
## I've red the measurements file from train (X_train.txt)
trainx <- read.fwf("UCI HAR Dataset/train/X_train.txt", colWidths, header=FALSE, n=100)
#
## training and test datasets are merged
mergedXdata <- rbind(trainx, testx)
#
## assigning the column names to the merged dataset.
names(mergedXdata)<-colNames[,2]

## I've red the activity ID file from test (y_test.txt)
testAct<-read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE, col.name="ActId", nrows=100)
#
## I've red the activity ID file from train (X_train.txt)
trainAct<-read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE, col.name="ActId", nrows=100)
##
#
## training and test datasets are merged
mergedActdata <- rbind(trainAct, testAct)


## I've red the subjects ID file from test (subject_test.txt)
testSub<-read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE, col.name="SubId", nrows=100)
#
## I've red the subjects ID file from train (X_train.txt)
trainSub<-read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE, col.name="SubId", nrows=100)
##
#
## training and test datasets are merged
mergedSubdata <- rbind(trainSub, testSub)

## merged subject, activity, measurements
mergedData <- cbind(mergedSubdata, mergedActdata, mergedXdata)



## STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement.
##

## selecting the columns that contains - accordingly with the "features.info" dataset -  mean() or std()
mergedDataSel<-select(mergedData,contains("mean\\()|std\\()"))
mergedDataSel1<-cbind(mergedSubdata, mergedActdata,mergedDataSel)

##
## STEP 3: Uses descriptive activity names to name the activities in the data set.
##

## I've red the activity labels file
activityLabels<-read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE, sep= "", col.name=c("ActId", "ActLabel"))
#
## I've joint the description of activities to the file
mergedDataSelAct <- merge(activityLabels,mergedDataSel1,by="ActId")

##
## STEP 4: Appropriately labels the data set with descriptive variable names. 
##
#
## I've modified the names of the columns
l<-names(mergedDataSelAct)
l <- gsub("-","_",l); l <- gsub("\\()","",l)
names(mergedDataSelAct) <- l
##
## STEP 5: From the data set in step 4, creates a second, independent tidy data set
##         with the average of each variable for each activity and each subject.
##
result <- summarise (group_by(mergedDataSelAct,ActId,SubId), mean(tBodyAcc_mean_Y))

z<-summarise (group_by(mergedDataSelAct,ActId,SubId), mean( tBodyAcc_mean_X )
              , mean( tBodyAcc_mean_Y ), mean( tBodyAcc_mean_Z ), mean( tBodyAcc_std_X )
              , mean( tBodyAcc_std_Y ), mean( tBodyAcc_std_Z ), mean( tGravityAcc_mean_X )
              , mean( tGravityAcc_mean_Y ), mean( tGravityAcc_mean_Z )
              , mean( tGravityAcc_std_X ), mean( tGravityAcc_std_Y )
              , mean( tGravityAcc_std_Z ), mean( tBodyAccJerk_mean_X )
              , mean( tBodyAccJerk_mean_Y ), mean( tBodyAccJerk_mean_Z )
              , mean( tBodyAccJerk_std_X ), mean( tBodyAccJerk_std_Y )
              , mean( tBodyAccJerk_std_Z ), mean( tBodyGyro_mean_X )
              , mean( tBodyGyro_mean_Y ), mean( tBodyGyro_mean_Z )
              , mean( tBodyGyro_std_X ), mean( tBodyGyro_std_Y )
              , mean( tBodyGyro_std_Z ), mean( tBodyGyroJerk_mean_X )
              , mean( tBodyGyroJerk_mean_Y ), mean( tBodyGyroJerk_mean_Z )
              , mean( tBodyGyroJerk_std_X ), mean( tBodyGyroJerk_std_Y )
              , mean( tBodyGyroJerk_std_Z ), mean( tBodyAccMag_mean )
              , mean( tBodyAccMag_std ), mean( tGravityAccMag_mean )
              , mean( tGravityAccMag_std ), mean( tBodyAccJerkMag_mean )
              , mean( tBodyAccJerkMag_std ), mean( tBodyGyroMag_mean )
              , mean( tBodyGyroMag_std ), mean( tBodyGyroJerkMag_mean )
              , mean( tBodyGyroJerkMag_std ), mean( fBodyAcc_mean_X )
              , mean( fBodyAcc_mean_Y ), mean( fBodyAcc_mean_Z )
              , mean( fBodyAcc_std_X ), mean( fBodyAcc_std_Y )
              , mean( fBodyAcc_std_Z ), mean( fBodyAccJerk_mean_X )
              , mean( fBodyAccJerk_mean_Y ), mean( fBodyAccJerk_mean_Z )
              , mean( fBodyAccJerk_std_X ), mean( fBodyAccJerk_std_Y )
              , mean( fBodyAccJerk_std_Z ), mean( fBodyGyro_mean_X )
              , mean( fBodyGyro_mean_Y ), mean( fBodyGyro_mean_Z )
              , mean( fBodyGyro_std_X ), mean( fBodyGyro_std_Y )
              , mean( fBodyGyro_std_Z ), mean( fBodyAccMag_mean )
              , mean( fBodyAccMag_std ), mean( fBodyBodyAccJerkMag_mean )
              , mean( fBodyBodyAccJerkMag_std ), mean( fBodyBodyGyroMag_mean )
              , mean( fBodyBodyGyroMag_std ), mean( fBodyBodyGyroJerkMag_mean )
              , mean( fBodyBodyGyroJerkMag_std ))

write.table(z,"result.txt",row.name=FALSE,sep=";")