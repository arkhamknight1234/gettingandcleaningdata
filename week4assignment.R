
library(plyr)
library(dplyr)

#############Merges the training and the test sets to create one data set.#####################

#read in the train data set#
X_train<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/train/X_train.txt")
subject_train<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/train/subject_train.txt")
y_train<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/train/y_train.txt")

train<- cbind(subject_train, y_train, X_train)

#read in the test data set#
X_test<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/test/X_test.txt")
subject_test<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/test/subject_test.txt")
y_test<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/test/y_test.txt")

test<- cbind(subject_test, y_test, X_test)

merged<-rbind(train,test)
#merged dataset has no column headings, the features dataset could have the headings
features<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/features.txt") # to be used for both X_train & X_test data
activity_labels<- read.table("C:/Users/Sheetal/gettingandcleaningdata/UCI HAR Dataset/activity_labels.txt")

varNames<-c("subject","activity_num",as.character(features$V2))
names(merged)<-varNames

table(merged$activity_num) #checking activity occurrencies

lookup<- activity_labels
names(lookup)<- c("activity_num","activity_type")
labeledDF<- join(merged,lookup,by='activity_num')


################Extracts only the measurements on the mean and standard deviation for each measurement.########
sum(duplicated(features$V2)) 
features$V2[duplicated(features$V2)] 
# Remove the duplicated columns:
to.remove<-as.character(features$V2[duplicated(features$V2)])
dropDF<-labeledDF
dropDF<-dropDF[, !(colnames(dropDF) %in% to.remove)]

#Now subset with only measurements on the mean and standard deviation:
subDF<-select(dropDF,contains("subject"), contains("activity_type"), contains("mean"), contains("std"))
dim(subDF) # final dataset has 88 columns

################USE DESCRIPTIVE ACTIVITY NAMES for THE ACTIVITIES IN THE DATA SET########
features<- read.table("features.txt") # to be used for both X_train & X_test data
activity_labels<- read.table("activity_labels.txt")

varNames<-c("subject","activity_num",as.character(features$V2))
names(merged)<-varNames

#has already been covered in step 1

#####STEP 4: LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES############# 
#has already been covered in step 1

#####STEP 5: CREATES A SECOND, INDEPENDENT TIDY DATA SET WITH THE AVG. OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT.#########

df2<- aggregate(subDF, by=list(subDF$subject, subDF$activity_type),  FUN=mean)
dim(df2) # final dataset has 180 rows which makes sense. But 2 columns have been added.

#Fix columns created with the aggregate function:
df2$subject<-NULL
df2$activity_type<-NULL
colnames(df2)[1:2]<-c("subject","activity_type")
str(df2) #is a data frame, subject is integer, activity_type is factor and the rest of columns are all numeric class.

#Write final dataset in a .txt file:
write.table(df2, "tidy_dataset_with_avg.txt", row.name=FALSE)
 


