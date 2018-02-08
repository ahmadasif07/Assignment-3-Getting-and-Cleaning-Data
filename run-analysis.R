library(reshape2)

getwd()
setwd("/Data Sciences/John Hopkins/WS/tidydatacourse")

# Load Labels for activities and features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

#Load Data Training
subjectTrain = read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE); #imports subject_train.txt
xTrain       = read.table("UCI HAR Dataset/train/x_train.txt",header=FALSE); #imports x_train.txt
yTrain       = read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE); #imports y_train.txt

# Load Data Testing
subjectTest = read.table('UCI HAR Dataset/test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('UCI HAR Dataset/test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('UCI HAR Dataset/test/y_test.txt',header=FALSE); #imports y_test.txt

# Assigin column names to the data imported above
colnames(activityLabels)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# cCreate the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(subjectTrain,yTrain,xTrain);

# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(subjectTest,yTest,xTest);

# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData);


# Extract only the data on mean and standard deviation
featuresWanted.index <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted.names <- features[featuresWanted.index,2]
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)

finalData = finalData[,c(1,2,featuresWanted.index+2)]
# turn activities & subjects into factors
finalData$activityId <- factor(finalData$activityId, levels = activityLabels[,1], labels = activityLabels[,2])
finalData$subjectId <- as.factor(finalData$subjectId)

finalData.melted <- melt(finalData, id = c("subjectId", "activityId"))
finalData.mean <- dcast(finalData.melted, subjectId + activityId ~ variable, mean)

write.table(finalData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

