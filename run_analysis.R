library(data.table)
library(reshape2)

## Read all data files from the working directory
## Unzip and put all data files in the ".data/UCI_HAR_Dataset" folder in the working directory

dtSubjectTrain <- fread("./data/UCI_HAR_Dataset/train/subject_train.txt")
dtSubjectTest <- fread("./data/UCI_HAR_Dataset/test/subject_test.txt")
dtActivityTrain <- fread("./data/UCI_HAR_Dataset/train/Y_train.txt")
dtActivityTest <- fread("./data/UCI_HAR_Dataset/test/Y_test.txt")
dtTrain <- data.table(read.table("./data/UCI_HAR_Dataset/train/X_train.txt"))
dtTest <- data.table(read.table("./data/UCI_HAR_Dataset/test/X_test.txt"))


## Row combine the data
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

## Column combine the data
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

## Set key
setkey(dt, subject, activityNum)


## Read Features.txt
## Extract only mean() and std()
dtFeatures <- data.table(read.table("./data/UCI_HAR_Dataset/Features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

## Create new column to indicate featureCode
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]

## Subset the master data with the featureCode
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]


## Use descriptive activity names
dtActivityNames <- fread("./data/UCI_HAR_Dataset/activity_labels.txt")
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

## Label with descriptive activity names
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dt, subject, activityNum, activityName)

## Melt the data to reshape into tall and narrow format
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

## Merge activity name
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

## Create new variables "activity" (equivalent to activityName) and "feature" (equivalent to featureName)
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)


## Use grepl to extract the values from 'feature' column
## 'feature' can be broken down into:
## 1. featDomain: Time, Freq
## 2. featInstrument: Accelerometer, Gyroscope
## 3. featAcceleration: NA, Body, Gravity
## 4. featVariable: Mean, SD
## 5. featJerk: NA, Jerk
## 6. featMagnitude: NA, Magnitude
## 7, featAxis: NA, X, Y, Z

## Features with 2 categories
y <- matrix(seq(1, 2), nrow=2)

x <- matrix(c(grepl("^t", dt$feature), grepl("^f", dt$feature)), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))

x <- matrix(c(grepl("Acc", dt$feature), grepl("Gyro", dt$feature)), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))

x <- matrix(c(grepl("BodyAcc", dt$feature), grepl("GravityAcc", dt$feature)), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))

x <- matrix(c(grepl("mean()", dt$feature), grepl("std()", dt$feature)), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

## Features with 1 category
dt$featJerk <- factor(grepl("Jerk", dt$feature), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepl("Mag", dt$feature), labels=c(NA, "Magnitude"))

## Features with 3 categories
y <- matrix(seq(1, 3), nrow=3)
x <- matrix(c(grepl("-X", dt$feature), grepl("-Y", dt$feature), grepl("-Z", dt$feature)), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))


## Create new tidy dataset called "dtTidy"
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

## Output tidyData.txt into working directory
write.table(dtTidy, "tidyData.txt", quote=FALSE, sep="\t", row.names=FALSE)