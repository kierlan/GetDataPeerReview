## The dataset should be unzipped into a folder named 'UCI HAR Dataset' in your working directory
## This is the condition which must be met for the script to work
## If not, you will be warned about it, and the script will not run.
condition<-(  (file.exists("./UCI HAR Dataset/test/X_test.txt") 
             & file.exists("./UCI HAR Dataset/test/y_test.txt") 
             & file.exists("./UCI HAR Dataset/train/X_train.txt") 
             & file.exists("./UCI HAR Dataset/train/y_train.txt") 
             & file.exists("./UCI HAR Dataset/activity_labels.txt") 
             & file.exists("./UCI HAR Dataset/features.txt") 
             & file.exists("./UCI HAR Dataset/test/subject_test.txt")
             & file.exists("./UCI HAR Dataset/train/subject_test.txt")
             )

if(!condition) {
  print("Please make sure the entire dataset is in a folder named 'UCI HAR Dataset' in your working folder and try again. For more information, see the README.md in the repo.")
}
else {
# Assuming the dataset is present, we may begin:
# First we load the 'test' and 'train' data, as well as the features, subject and activity IDs and labels into data tables
test.data<-read.table("./UCI HAR Dataset/test/X_test.txt")
test.activityIDs<-read.table("./UCI HAR Dataset/test/y_test.txt")
test.subjectIDs<-read.table("./UCI HAR Dataset/test/subject_test.txt")
train.data<-read.table("./UCI HAR Dataset/train/X_train.txt")
train.activityIDs<-read.table("./UCI HAR Dataset/train/y_train.txt")
train.subjectIDs<-read.table("./UCI HAR Dataset/train/subject_train.txt")
activity.labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
features.labels<-read.table("./UCI HAR Dataset/features.txt")

#Begin to construct cleaner data set - first the subjects, and activityIDs
TestSet<-cbind(test.subjectIDs,test.activityIDs)
TrainSet<-cbind(train.subjectIDs,train.activityIDs)

#Name the two columns properly
names(TestSet)<- c("Subject","Activity ID")
names(TrainSet)<-c("Subject","Activity ID")

#Then we add the activity labels, naming the column 'Activity'
#We use sapply to go over the Activity ID column and create a vector of appropriate activity labels according to IDs
TestSet$Activity<-sapply(TestSet[,2],function(x) activity.labels[x,2])
TrainSet$Activity<-sapply(TrainSet[,2],function(x) activity.labels[x,2])

#The test and train data are a 561 variable vector - the 561 features - lets name them appropriately so we can select them
names(test.data)<-features.labels[,2]
names(train.data)<-features.labels[,2]

#We need only the measures of mean and standard deviation from the test and train data
#First a vector which chooses the columns to keep, sorted in order of appearance
test.keep<-c(grep("mean()",names(test.data),fixed=TRUE),grep("std()",names(test.data),fixed=TRUE))
test.keep<-sort(test.keep)
train.keep<-c(grep("mean()",names(train.data),fixed=TRUE),grep("std()",names(train.data),fixed=TRUE))
train.keep<-sort(train.keep)

#Then we keep only the mean and standard deviation measurement data
test.data<-test.data[,test.keep]
train.data<-train.data[,train.keep]

#We add the data to our sets
TestSet<-cbind(TestSet,test.data)
TrainSet<-cbind(TrainSet,train.data)

#And we put the sets together
TotalSet<-rbind(TestSet,TrainSet)

#Now we need to find the average results on all the measures for each activity by each subject
#First we factor the Subjects and Activity to get commonalities
f<-interaction(TotalSet$Subject,TotalSet$Activity)
#Then we sapply the colMeans on the calculated factors
tidyData<-sapply(split(TotalSet[,4:69],f),colMeans)

#we transpose the data frame since the sapply confused the columns and the rows (instead of x,y it is now y,x)
tidyData<-data.frame(t(tidyData)) #this transposes the data frame
#we get the row.names - which contain the subject IDS and activities in the correct order
rowNames<-row.names(tidyData)
row.names(tidyData)<-NULL #we also clean up the row names so they aren't ugly
#get the order of the subjects and activities from the row names and save as a data frame
SubAct<-as.data.frame(do.call(rbind,strsplit(rowNames,".",fixed=TRUE)))
#name the columns
names(SubAct)<-c("Subject","Activity")
#finally put the data together
tidyData<-cbind(SubAct,tidyData)
#and write it out
write.table(tidyData,file="./tidyData.txt")

#as well as the totalData
write.table(TotalSet,file="./totalData.txt")

}