## This script merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each 
## measurement. 
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names. 
## creates a second, independent tidy data set with the average of each 
## variable for each activity and each subject.

## First, load the train data set along with the labels
trainData = read.table("./train/X_train.txt")
trainLabels = read.table("./train/y_train.txt")
## Then, laod the test data set along with the labels
testData = read.table("./test/X_test.txt")
testLabels = read.table("y_test.txt")
## Now, load the subjects for train and test data sets
trainSubject = read.table("./train/subject_train.txt")
testSubject = read.table("./test/subject_test.txt")
## Create a dataframe with variable names
variable_names = read.table("features.txt")
## Subset so we only have the variable names left
names = variable_names[,2]
## Now, assing the names to train and test data sets
colnames(trainData) <- names
colnames(testData) <- names
activity_labels = factor(c("WALKING", "WALKING_UPSTAIRS", 
                           "WALKING_DOWNSTAIRS", "SITTING", 
                           "STANDING", "LAYING"))
## Replace activities (1 through 6) with descriptive names
trainLabels[(which(trainLabels[,1]==1)),] = "WALKING"
trainLabels[(which(trainLabels[,1]==3)),] = "WALKING_DOWNSTAIRS"
trainLabels[(which(trainLabels[,1]==4)),] = "SITTING"
trainLabels[(which(trainLabels[,1]==5)),] = "STANDING"
trainLabels[(which(trainLabels[,1]==6)),] = "LAYING"
testLabels[(which(testLabels[,1]==1)),] = "WALKING"
testLabels[(which(testLabels[,1]==2)),] = "WALKING_UPSTAIRS"
testLabels[(which(testLabels[,1]==3)),] = "WALKING_DOWNSTAIRS"
testLabels[(which(testLabels[,1]==4)),] = "SITTING"
testLabels[(which(testLabels[,1]==5)),] = "STANDING"
testLabels[(which(testLabels[,1]==6)),] = "LAYING"
names(trainLabels) = "ACTIVITY"
names(testLabels) = "ACTIVITY"
## We will use matchcols from gdata to pick out variables with mean() and std()
install.packages("gdata")
library(gdata)
## Pick out the variables with mean() in their names
cols_with_mean = matchcols(trainData, with=c("mean()"))
## Pick out the variables with std() in their names
cols_with_std=matchcols(trainData, with=c("std()"))
## Now, subset the train and test data sets with mean() and std() varaialbes
## As an example, the two statements below would subset with mean() only
## trainData_with_means = trainData[, cols_with_mean]
## testData_with_means = testData[, cols_with_mean]
## But, we need both mean() and std() variables
trainData_with_means_std = trainData[, c(cols_with_mean, cols_with_std)]
testData_with_means_std = testData[, c(cols_with_mean, cols_with_std)]
names(trainSubject)="SUBJECT"
names(testSubject)="SUBJECT"
## Now, add the ACTIVITY and SUBJECT columns
trainData_with_activity_subject = cbind(trainData_with_means_std, trainLabels, 
                                        trainSubject)
testData_with_activity_subject = cbind(testData_with_means_std, testLabels, 
                                       testSubject)
## Join the train and test data sets using join from the plyr package
library(plyr)
FinalDataSet = join(trainData_with_activity_subject, 
                    testData_with_activity_subject, type="full")
## The following is not the most elegant, but it gets the job done
## It is the best hack I could come up with at this time
## First, create a vector to hold 79 elements
x <- c(1:79)
## Create a data frame with 6 rows for the ACTIVITIES
df_activity <- data.frame(nrow=6)
## Programmatically, use tapply on each variable grouped by ACTIVITY
for (i in x) {
  x <- tapply(FinalDataSet[,i], FinalDataSet[,80], mean)
  df_activity <- cbind(df_activity, x)
}
## Since we created a data frame with 6 rows at the beginning, let us remove them
df_activity = df_activity[,2:80]
## Assign names to the dataframe
names(df_activity) = names(FinalDataSet[1:79])
## Now, do the same with SUBJECTs
y <- c(1:79)
df_subject <- data.frame(nrow=30)
for (i in y) {
  y <- tapply(FinalDataSet[,i], FinalDataSet[,81], mean)
  df_subject <- cbind(df_subject, y)
}
df_subject = df_subject[,2:80]
names(df_subject) = names(FinalDataSet[1:79])
## Combine both SUBJECT and ACTIVITY dataframes using rbind
df_subject_activity = rbind(df_subject, df_activity)
## Write the .text file
write.table(df_subject_activity, file="run_analysis.txt", row.names=FALSE)

