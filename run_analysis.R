#load libraries
library(dplyr)

##############  DOWNLOADING THE FILE  #########################
#Checking the existence of the folder. 
#If don't exist, create the folder and download the file
if(!file.exists("dataProj")){
    print("here")
    dir.create("dataProj")
}
 
#down
temp=tempfile()
fileUrl="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile=temp, mode="wb")
#temp="dataProj/dataset.zip"


##############  LOADING AND SHAPPING THE DATA FOR TEST  ##############
#test data:
X_test <- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"))
subject_test <- read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"))
features <- read.table(unz(temp, "UCI HAR Dataset/features.txt"))

##Shapping the test data into 1 data set
test<-tbl_df(X_test)
rm(X_test)
names(test)<-features$V2
test$subject <- as.character(subject_test$V1)
test$y<-as.numeric(y_test$V1)
test$group=rep("test", length(test$y))

##Selecting only required columns:
reqCols <- c(names(test)[sort(c(grep("*mean*", names(test)), grep("*std*", names(test))))], "subject", "y", "group")
test <- test[,reqCols]

##cleaning problematic characters:
names(test)<-gsub("-", "_", names(test))
names(test)<-gsub("[()]", "", names(test))


##############  LOADING AND SHAPPING THE DATA FOR TRAIN  ##############
X_train <- read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"))
subject_train <- read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"))

##Shapping the test data into 1 data set
train <- tbl_df(X_train)
rm(X_train)
names(train)<-features$V2
train$subject <- as.character(subject_train$V1)
train$y<-as.character(y_train$V1)
train$group=rep("train", length(train$y))

##Selecting only required columns:
reqCols <- c(names(train)[sort(c(grep("*mean*", names(train)), grep("*std*", names(train))))], "subject", "y", "group")
train <- train[,reqCols]

##cleaning problematic characters:
names(train)<-gsub("-", "_", names(train))
names(train)<-gsub("[()]", "", names(train))

##############  MERGE TEST AND TRAIN  ##############
data<-rbind(test,train)

##Delete the train and test sets to save memory
rm(test)
rm(train)

##############  ADD ACTIVITY LABELS  ##############
##add descriptitions for activities
activity_labels <- read.table(unz(temp, "UCI HAR Dataset/activity_labels.txt"))
names(activity_labels)<-c("cod", "activity")
data<-merge(data, activity_labels, by.x="y", by.y="cod", all.x=TRUE)


##############  CALCULATE THE MEANS AND SDV  ##############
data<-tbl_df(data)
tidyDataSet<- data %>%
    group_by(activity, subject) %>%
    summarize(mean_tBodyAcc_mean_X=mean(tBodyAcc_mean_X)
              ,mean_tBodyAcc_mean_Y=mean(tBodyAcc_mean_Y)
              ,mean_tBodyAcc_mean_Z=mean(tBodyAcc_mean_Z)
              ,mean_tBodyAcc_std_X=mean(tBodyAcc_std_X)
              ,mean_tBodyAcc_std_Y=mean(tBodyAcc_std_Y)
              ,mean_tBodyAcc_std_Z=mean(tBodyAcc_std_Z)
              ,mean_tGravityAcc_mean_X=mean(tGravityAcc_mean_X)
              ,mean_tGravityAcc_mean_Y=mean(tGravityAcc_mean_Y)
              ,mean_tGravityAcc_mean_Z=mean(tGravityAcc_mean_Z)
              ,mean_tGravityAcc_std_X=mean(tGravityAcc_std_X)
              ,mean_tGravityAcc_std_Y=mean(tGravityAcc_std_Y)
              ,mean_tGravityAcc_std_Z=mean(tGravityAcc_std_Z)
              ,mean_tBodyAccJerk_mean_X=mean(tBodyAccJerk_mean_X)
              ,mean_tBodyAccJerk_mean_Y=mean(tBodyAccJerk_mean_Y)
              ,mean_tBodyAccJerk_mean_Z=mean(tBodyAccJerk_mean_Z)
              ,mean_tBodyAccJerk_std_X=mean(tBodyAccJerk_std_X)
              ,mean_tBodyAccJerk_std_Y=mean(tBodyAccJerk_std_Y)
              ,mean_tBodyAccJerk_std_Z=mean(tBodyAccJerk_std_Z)
              ,mean_tBodyGyro_mean_X=mean(tBodyGyro_mean_X)
              ,mean_tBodyGyro_mean_Y=mean(tBodyGyro_mean_Y)
              ,mean_tBodyGyro_mean_Z=mean(tBodyGyro_mean_Z)
              ,mean_tBodyGyro_std_X=mean(tBodyGyro_std_X)
              ,mean_tBodyGyro_std_Y=mean(tBodyGyro_std_Y)
              ,mean_tBodyGyro_std_Z=mean(tBodyGyro_std_Z)
              ,mean_tBodyGyroJerk_mean_X=mean(tBodyGyroJerk_mean_X)
              ,mean_tBodyGyroJerk_mean_Y=mean(tBodyGyroJerk_mean_Y)
              ,mean_tBodyGyroJerk_mean_Z=mean(tBodyGyroJerk_mean_Z)
              ,mean_tBodyGyroJerk_std_X=mean(tBodyGyroJerk_std_X)
              ,mean_tBodyGyroJerk_std_Y=mean(tBodyGyroJerk_std_Y)
              ,mean_tBodyGyroJerk_std_Z=mean(tBodyGyroJerk_std_Z)
              ,mean_tBodyAccMag_mean=mean(tBodyAccMag_mean)
              ,mean_tBodyAccMag_std=mean(tBodyAccMag_std)
              ,mean_tGravityAccMag_mean=mean(tGravityAccMag_mean)
              ,mean_tGravityAccMag_std=mean(tGravityAccMag_std)
              ,mean_tBodyAccJerkMag_mean=mean(tBodyAccJerkMag_mean)
              ,mean_tBodyAccJerkMag_std=mean(tBodyAccJerkMag_std)
              ,mean_tBodyGyroMag_mean=mean(tBodyGyroMag_mean)
              ,mean_tBodyGyroMag_std=mean(tBodyGyroMag_std)
              ,mean_tBodyGyroJerkMag_mean=mean(tBodyGyroJerkMag_mean)
              ,mean_tBodyGyroJerkMag_std=mean(tBodyGyroJerkMag_std)
              ,mean_fBodyAcc_mean_X=mean(fBodyAcc_mean_X)
              ,mean_fBodyAcc_mean_Y=mean(fBodyAcc_mean_Y)
              ,mean_fBodyAcc_mean_Z=mean(fBodyAcc_mean_Z)
              ,mean_fBodyAcc_std_X=mean(fBodyAcc_std_X)
              ,mean_fBodyAcc_std_Y=mean(fBodyAcc_std_Y)
              ,mean_fBodyAcc_std_Z=mean(fBodyAcc_std_Z)
              ,mean_fBodyAcc_meanFreq_X=mean(fBodyAcc_meanFreq_X)
              ,mean_fBodyAcc_meanFreq_Y=mean(fBodyAcc_meanFreq_Y)
              ,mean_fBodyAcc_meanFreq_Z=mean(fBodyAcc_meanFreq_Z)
              ,mean_fBodyAccJerk_mean_X=mean(fBodyAccJerk_mean_X)
              ,mean_fBodyAccJerk_mean_Y=mean(fBodyAccJerk_mean_Y)
              ,mean_fBodyAccJerk_mean_Z=mean(fBodyAccJerk_mean_Z)
              ,mean_fBodyAccJerk_std_X=mean(fBodyAccJerk_std_X)
              ,mean_fBodyAccJerk_std_Y=mean(fBodyAccJerk_std_Y)
              ,mean_fBodyAccJerk_std_Z=mean(fBodyAccJerk_std_Z)
              ,mean_fBodyAccJerk_meanFreq_X=mean(fBodyAccJerk_meanFreq_X)
              ,mean_fBodyAccJerk_meanFreq_Y=mean(fBodyAccJerk_meanFreq_Y)
              ,mean_fBodyAccJerk_meanFreq_Z=mean(fBodyAccJerk_meanFreq_Z)
              ,mean_fBodyGyro_mean_X=mean(fBodyGyro_mean_X)
              ,mean_fBodyGyro_mean_Y=mean(fBodyGyro_mean_Y)
              ,mean_fBodyGyro_mean_Z=mean(fBodyGyro_mean_Z)
              ,mean_fBodyGyro_std_X=mean(fBodyGyro_std_X)
              ,mean_fBodyGyro_std_Y=mean(fBodyGyro_std_Y)
              ,mean_fBodyGyro_std_Z=mean(fBodyGyro_std_Z)
              ,mean_fBodyGyro_meanFreq_X=mean(fBodyGyro_meanFreq_X)
              ,mean_fBodyGyro_meanFreq_Y=mean(fBodyGyro_meanFreq_Y)
              ,mean_fBodyGyro_meanFreq_Z=mean(fBodyGyro_meanFreq_Z)
              ,mean_fBodyAccMag_mean=mean(fBodyAccMag_mean)
              ,mean_fBodyAccMag_std=mean(fBodyAccMag_std)
              ,mean_fBodyAccMag_meanFreq=mean(fBodyAccMag_meanFreq)
              ,mean_fBodyBodyAccJerkMag_mean=mean(fBodyBodyAccJerkMag_mean)
              ,mean_fBodyBodyAccJerkMag_std=mean(fBodyBodyAccJerkMag_std)
              ,mean_fBodyBodyAccJerkMag_meanFreq=mean(fBodyBodyAccJerkMag_meanFreq)
              ,mean_fBodyBodyGyroMag_mean=mean(fBodyBodyGyroMag_mean)
              ,mean_fBodyBodyGyroMag_std=mean(fBodyBodyGyroMag_std)
              ,mean_fBodyBodyGyroMag_meanFreq=mean(fBodyBodyGyroMag_meanFreq)
              ,mean_fBodyBodyGyroJerkMag_mean=mean(fBodyBodyGyroJerkMag_mean)
              ,mean_fBodyBodyGyroJerkMag_std=mean(fBodyBodyGyroJerkMag_std)
              ,mean_fBodyBodyGyroJerkMag_meanFreq=mean(fBodyBodyGyroJerkMag_meanFreq)
    )

rm(data)

##############  CREATE THE OUTPUT FILE  ##############
write.table(tidyDataSet, "tidyDataSet.dat", row.name=F)

##############  CLEANUP THE ENVIRONMENT ##############
rm(list= c("activity_labels", "fileUrl", "subject_train", "subject_test"))
rm(list= c("features", "temp", "y_test", "y_train", "reqCols", "tidyDataSet"))

