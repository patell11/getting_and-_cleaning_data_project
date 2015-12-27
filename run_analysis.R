
##read the data  form the files
features<-read.table("./features.txt",header = FALSE)
activitytype<-read.table("./activity_labels.txt",header = FALSE)

##read the train data

subjecttrain<-read.table("./train/subject_train.txt",header = FALSE)
xtrain<-read.table("./train/X_train.txt",header = FALSE)
ytrain<-read.table("./train/y_train.txt",header = FALSE)

##assign the column names
colnames(activitytype)<-c("activityid","activitytype")
colnames(subjecttrain)<-"subjectid"
colnames(xtrain)<-features[,2]
colnames(ytrain)<-"activityid"

##creating the combined training data
trainingdata<-cbind(ytrain,subjecttrain,xtrain)

##reading the test data
 subjecttest<-read.table("subject_test.txt",header = FALSE)
 xtest<-read.table("X_test.txt",header=FALSE)
 ytest<-read.table("y_test.txt",header = FALSE)
 
 ##assigning the column names to the test data

  colnames(subjecttest)<-"subjectid"
 colnames(xtest)<-features[,2]
colnames(ytest)<-"activityid"

##creating the combined test data

testdata<-cbind(ytest,subjecttest,xtest)

##finally combine the test data and the test data

finaldata<-rbind(trainingdata,testdata)

##reading the columns name of the final data into a vector
columnnames<-colnames(finaldata)

##creating a logical vector for the stored columns name to choose only the column
##with the mean and standard deviation data
 
logicalVector = (grepl("activity..",columnnames) | grepl("subject..",columnnames) | grepl("-mean..",columnnames) & !grepl("-meanFreq..",columnnames) & !grepl("mean..-",columnnames) | grepl("-std..",columnnames) & !grepl("-std()..-",columnnames));

##taking only the columns having the mean and std data

finaldata <- finaldata[logicalVector==TRUE];

##changing the name of the column with the descriptive names


for (i in 1:length(colnames)) 
{
  colnames[i] = gsub("\\()","",colnames[i])
  colnames[i] = gsub("-std$","StdDev",colnames[i])
  colnames[i] = gsub("-mean","Mean",colnames[i])
  colnames[i] = gsub("^(t)","time",colnames[i])
  colnames[i] = gsub("^(f)","freq",colnames[i])
  colnames[i] = gsub("([Gg]ravity)","Gravity",colnames[i])
  colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames[i])
  colnames[i] = gsub("[Gg]yro","Gyro",colnames[i])
  colnames[i] = gsub("AccMag","AccMagnitude",colnames[i])
  colnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnames[i])
  colnames[i] = gsub("JerkMag","JerkMagnitude",colnames[i])
  colnames[i] = gsub("GyroMag","GyroMagnitude",colnames[i])
};

##grouping the data by activityid,subjectid and the activitytype and
##taking the mean of each variables

library(plyr)
tidydata<-ddply(finaldata,.(activityid,subjectid,activitytype),colwise(mean))

##expoting the tidy data

write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');




