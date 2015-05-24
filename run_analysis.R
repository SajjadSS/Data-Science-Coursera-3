#### START OF STEP 1 OUT OF 5 ####
rm(list=ls(all=TRUE))
features<-read.table("Dataset/features.txt")
activity_labels<-read.table("Dataset/activity_labels.txt")
X_train<-read.table("Dataset/train/X_train.txt")
y_train<-read.table("Dataset/train/y_train.txt")
subject_train<-read.table("Dataset/train/subject_train.txt")
X_test<-read.table("Dataset/test/X_test.txt")
y_test<-read.table("Dataset/test/y_test.txt")
subject_test<-read.table("Dataset/test/subject_test.txt")
X_all<-rbind(X_train,X_test) #Combining X_test and X_train by common columns, X_test is added to the end of X_train
y_all<-rbind(y_train,y_test) #Combining y_test and y_train by common columns, y_test is added to the end of y_train
subject_all<-rbind(subject_train,subject_test) #Combining subject_test and subject_train by common columns, subject_test is added to the end of subject_train
#### END OF STEP 1 OUT OF 5 ####

#### START OF STEP 2 OUT OF 5 ####
mean_columns_id<-grep("mean()",features[,2],fixed=TRUE) #finding the number (id) of columns where are mean of the data
std_columns_id<-grep("std()",features[,2],fixed=TRUE) #finding the number (id) of columns where are standard deviation of data
X_mean_std<-X_all[,c(mean_columns_id,std_columns_id)] #extracting mean and std columns, first are all columns of means, then all columns of stds.
##### END OF STEP 2 OUT OF 5 ####

#### START OF STEP 3 OUT OF 5 ####
activity<-factor(as.numeric(y_all[,1]),labels=levels(activity_labels[,2])) 
X_mean_std$activity<-activity
X_mean_std$subject<-subject_all
#### END OF STEP 3 OUT OF 5 ####

#### START OF STEP 4 OUT OF 5 ####
mean_funcs<-features[mean_columns_id,2]
std_funcs<-features[std_columns_id,2]
funcs<-c(as.character(mean_funcs[1:length(mean_funcs)]),as.character(std_funcs[1:length(std_funcs)]),"activity","subject")
names(X_mean_std)<-funcs
#### END OF STEP 4 OUT OF 5 ####

#### START OF STEP 5 OUT OF 5 ####
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
#### END OF STEP 5V OUT OF 5 ####

