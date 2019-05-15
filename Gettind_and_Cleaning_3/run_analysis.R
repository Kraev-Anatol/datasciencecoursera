# Downloading and unzipping of basic data.
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- getwd()
download.file(fileURL, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")


# Extraction activity labels and features
activities <- data.table::fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("class", "activity"))

features <- data.table::fread(file.path(path, "UCI HAR Dataset/features.txt")
                        , col.names = c("index", "Names"))
# Search and replacement of coincidence (mean and deviation)
featuresWanted <- grep("(mean|std)\\(\\)", features[, Names])
measurements <- features[featuresWanted, Names]
measurements <- gsub('[()]', '', measurements)

# Extraction train databases
X_train <- data.table::fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(X_train, colnames(X_train), measurements)

trainActivities <- data.table::fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- data.table::fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
X_train <- cbind(trainSubjects, trainActivities, X_train)


# Extraction test datasets
X_test <- data.table::fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(X_test, colnames(X_test), measurements)
testActivities <- data.table::fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- data.table::fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
X_test <- cbind(testSubjects, testActivities, X_test)


# merge datasets
UnionTrainTest <- rbind(X_train, X_test)


# Creation of an independent set of tidy data with the mean value of each variable for each occupation and each examinee.
UnionTrainTest[["Activity"]] <- factor(UnionTrainTest[, Activity]
                                 , levels = activities[["class"]]
                                 , labels = activities[["activity"]])
UnionTrainTest <- reshape2::melt(data = UnionTrainTest, id = c("SubjectNum", "Activity"))

UnionTrainTest <- reshape2::dcast(data = UnionTrainTest, SubjectNum + Activity ~ variable, fun.aggregate = mean)

write.table(UnionTrainTest, "FinalData.txt", row.name=FALSE)

