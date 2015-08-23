## run_analysis.R

library(plyr)

## Read in test tables
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
xTest <- read.table("UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("UCI HAR Dataset/test/y_test.txt")

## Read in train tables
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("UCI HAR Dataset/train/y_train.txt")

## Merge subject_test and subject_train tables together and name the column
subjects <- rbind(subjectTest,subjectTrain)
colnames(subjects) <- "Subject"

## Merge x tables together and name the column using features.txt
## Use str(features) to see factors
x <- rbind(xTest,xTrain)
features <- read.table("UCI HAR Dataset/features.txt")
colnames(x) <- features$V2 

## Merge y tables together and name the column
y <- rbind(yTest,yTrain)
colnames(y) <- "Activity"

## Merge all three to make complete data set
data <- cbind(subjects,x,y)

## Get the columns with mean and std in them only
cols <- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
colNames <-c(as.character(cols), "Subject", "Activity" )
meanstdData <- subset(data,select=colNames)

## Re-label Activities in meanstdData set
meanstdData$Activity <- as.character(meanstdData$Activity)
meanstdData$Activity[meanstdData$Activity == 1] <- "Walking"
meanstdData$Activity[meanstdData$Activity == 2] <- "Walking Upstairs"
meanstdData$Activity[meanstdData$Activity == 3] <- "Walking Downstairs"
meanstdData$Activity[meanstdData$Activity == 4] <- "Sitting"
meanstdData$Activity[meanstdData$Activity == 5] <- "Standing"
meanstdData$Activity[meanstdData$Activity == 6] <- "Laying"
meanstdData$Activity <- as.factor(meanstdData$Activity)

## Appropriately label the data set with descriptive variable names
## Using gsub to subsitute bad names for good, descriptive ones in meanstdData
names(meanstdData) <- gsub("^t", "Time", names(meanstdData))
names(meanstdData)<-gsub("^f", "Frequency", names(meanstdData))
names(meanstdData)<-gsub("Acc", "Accelerometer", names(meanstdData))
names(meanstdData)<-gsub("Gyro", "Gyroscope", names(meanstdData))
names(meanstdData)<-gsub("Mag", "Magnitude", names(meanstdData))
names(meanstdData)<-gsub("BodyBody", "Body", names(meanstdData))
names(meanstdData)<-gsub("tBody", "TimeBody", names(meanstdData))

## Create a second independent tidy data
tidy <- aggregate(. ~Subject + Activity, meanstdData, mean)
tidy <- tidy[order(tidy$Subject,tidy$Activity),]
write.table(tidy, file = "TidyData.txt", row.names=FALSE)
