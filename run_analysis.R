# 1. Merges the training and the test sets to create one data set.

trainx <- read.table("train/X_train.txt")
testx <- read.table("test/X_test.txt")
X <- rbind(trainx, testx)

trains <- read.table("train/subject_train.txt")
tests <- read.table("test/subject_test.txt")
S <- rbind(trains, tests)

trainy <- read.table("train/y_train.txt")
testy <- read.table("test/y_test.txt")
Y <- rbind(trainy, testy)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
good <- grep("mean\\(\\)|std\\(\\)", features[, 2])
X <- X[, good]
names(X) <- features[good, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- gsub("-", "", names(X)) # remove "-" in column names

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] <- gsub("_", "", tolower(as.character(activities[, 2])))
Y[, 1] <- activities_Y
activities_Y <- activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(X,S,Y)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

Subjects <- length(S[,1])
nActivities <- length(activities[,1])
nColumns = dim(cleaned)[2]
result = cleaned[1:(Subjects*nActivities), ]

row = 1
for (s in 1:Subjects) {
    for (a in 1:nActivities) {
        result[row, 1] = S[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:nColumns] <- colMeans(tmp[, 3:nColumns])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages.txt")








