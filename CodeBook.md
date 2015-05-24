---
title: "Getting and Cleaning Data course project"
author: "SAJJAD"
date: "May 23, 2015"
output: html_document
---
Getting and Cleaning Data course project
==========================

```{r setoptions , echo=FALSE}
opts_chunk$set(echo=TRUE,results="hide", eval=FALSE)
```

This is a codebook for the project defined in the course "Getting and Cleaning Data" in the Data Science specialication provided by Coursera.  

The project contains 5 steps as:  
1. Merges the training and the test sets to create one data set.  
2. Extracts only the measurements on the mean and standard deviation for each measurement.  
3. Uses descriptive activity names to name the activities in the data set.  
4. Appropriately labels the data set with descriptive variable names.  
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

STEP 1  
Merging the train and the test sets to create one data set.
First all the datasets is imported to Rstudio using read.table() function.

```{r}
features<-read.table("Dataset/features.txt")
activity_labels<-read.table("Dataset/activity_labels.txt")
X_train<-read.table("Dataset/train/X_train.txt")
y_train<-read.table("Dataset/train/y_train.txt")
subject_train<-read.table("Dataset/train/subject_train.txt")
X_test<-read.table("Dataset/test/X_test.txt")
y_test<-read.table("Dataset/test/y_test.txt")
subject_test<-read.table("Dataset/test/subject_test.txt")
```
The test and train datasets is combined using rbind function for all X, y and subject data sets.   
```{r}
X_all<-rbind(X_train,X_test)
y_all<-rbind(y_train,y_test)
subject_all<-rbind(subject_train,subject_test)
```
By the end of step 1 all test and train datasets have been  combind. We have three files as X\_all, y\_all and subject\_all.

STEP 2  
Extracts only the measurements on the mean and standard deviation for each measurement.   

First we should find the id for measurments containing mean() or std() terms.

```{r}
mean_columns_id<-grep("mean()",features[,2],fixed=TRUE)
std_columns_id<-grep("std()",features[,2],fixed=TRUE)
```
Next we choose the desired columns of the dataset X\_all and save in to X\_mean\_std. 
```{r}
X_mean_std<-X_all[,c(mean_columns_id,std_columns_id)]
```
STEP 3   
Activity name and subject is added to the dataset of X\_mean\_std as two columns to the right of X\_mean\_std.
```{r}
activity<-factor(as.numeric(y_all[,1]),labels=levels(activity_labels[,2])) 
X_mean_std$activity<-activity
X_mean_std$subject<-subject_all
```

STEP 4   
Appropriately labels the data set with descriptive variable names.

```{r}
mean_funcs<-features[mean_columns_id,2]
std_funcs<-features[std_columns_id,2]
funcs<-c(as.character(mean_funcs[1:length(mean_funcs)]),as.character(std_funcs[1:length(std_funcs)]),"activity","subject")
names(X_mean_std)<-funcs
```

At this point X_mean_std contains all the observations (test and train) for mean() and std() measurments. First 33 columns are mean() measurments and the second 33 columns are std() measurments. Dataset contains 10299 observation. In X\_mean\_std column 67 contains activity tags and column 68 contains subject numbers. All the columns have their coresponding names.  

STEP 5   
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

```{r}
temp0<-length(levels(X_mean_std[,"activity"]))
tidy_data<-data.frame(matrix(ncol = c(length(funcs)-2), nrow = 0))
tidy_data_subject<-data.frame(matrix(ncol = 1, nrow = 0))
tidy_data_activity<-data.frame(matrix(ncol = 1, nrow = 0))
for (i in 1:max(unique(X_mean_std[,"subject"]))){
     temp1<-which(X_mean_std[,"subject"]==i)
     for (j in 1:length(levels(X_mean_std[,"activity"]))){
          temp2<-which(X_mean_std[,"activity"]==activity_labels[j,2])
          temp3<-intersect(temp1,temp2)
          temp4<-colMeans(X_mean_std[temp3,funcs[1:c(length(funcs)-2)]])
          tidy_data<-rbind(tidy_data,temp4)
          tidy_data_subject<-rbind(tidy_data_subject,i)
          tidy_data_activity<-rbind(tidy_data_activity,j)
     }
}
tidy_activity<-factor(as.numeric(tidy_data_activity[,1]),labels=levels(activity_labels[,2])) 
tidy_subject<-factor(as.numeric(tidy_data_subject[,1]),labels=c(1:max(unique(X_mean_std[,"subject"]))))
tidy_data$activity<-tidy_activity
tidy_data$subject<-tidy_subject
names(tidy_data)<-funcs
write.table(tidy_data,"tidy_data.txt",row.name=FALSE)
```

Finally the "tidy\_data.txt" dataset contains 68 columns (33 for mean() functions, 33 for std() functions, 1 for activity tags and 1 for subject numbers). Also in has 180 rows which comes from 6 activty for each of  the 30 subjects.

In order to make it more clear here are the 68 columns' names of "tidy_data.txt" dataset where the last two are "activity" and "subject".    
[1] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
 [3] "tBodyAcc-mean()-Z"           "tGravityAcc-mean()-X"       
 [5] "tGravityAcc-mean()-Y"        "tGravityAcc-mean()-Z"       
 [7] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
 [9] "tBodyAccJerk-mean()-Z"       "tBodyGyro-mean()-X"         
[11] "tBodyGyro-mean()-Y"          "tBodyGyro-mean()-Z"         
[13] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
[15] "tBodyGyroJerk-mean()-Z"      "tBodyAccMag-mean()"         
[17] "tGravityAccMag-mean()"       "tBodyAccJerkMag-mean()"     
[19] "tBodyGyroMag-mean()"         "tBodyGyroJerkMag-mean()"    
[21] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
[23] "fBodyAcc-mean()-Z"           "fBodyAccJerk-mean()-X"      
[25] "fBodyAccJerk-mean()-Y"       "fBodyAccJerk-mean()-Z"      
[27] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
[29] "fBodyGyro-mean()-Z"          "fBodyAccMag-mean()"         
[31] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyGyroMag-mean()"    
[33] "fBodyBodyGyroJerkMag-mean()" "tBodyAcc-std()-X"           
[35] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
[37] "tGravityAcc-std()-X"         "tGravityAcc-std()-Y"        
[39] "tGravityAcc-std()-Z"         "tBodyAccJerk-std()-X"       
[41] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
[43] "tBodyGyro-std()-X"           "tBodyGyro-std()-Y"          
[45] "tBodyGyro-std()-Z"           "tBodyGyroJerk-std()-X"      
[47] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
[49] "tBodyAccMag-std()"           "tGravityAccMag-std()"       
[51] "tBodyAccJerkMag-std()"       "tBodyGyroMag-std()"         
[53] "tBodyGyroJerkMag-std()"      "fBodyAcc-std()-X"           
[55] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
[57] "fBodyAccJerk-std()-X"        "fBodyAccJerk-std()-Y"       
[59] "fBodyAccJerk-std()-Z"        "fBodyGyro-std()-X"          
[61] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
[63] "fBodyAccMag-std()"           "fBodyBodyAccJerkMag-std()"  
[65] "fBodyBodyGyroMag-std()"      "fBodyBodyGyroJerkMag-std()"   
[67] "activity"                    "subject"


