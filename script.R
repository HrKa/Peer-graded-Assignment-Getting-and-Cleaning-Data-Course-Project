library(dplyr)

# 1. Merges the training and the test sets to create one data set.
 	# Load activity labels
	activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

	# Load features
	features <- read.table("UCI HAR Dataset/features.txt")

	# Load datasets
	X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
	Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
	subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

	X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
	Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
	subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

	# Merge
	X_all <- rbind(X_train, X_test)
	Y_all <- rbind(Y_train, Y_test)
	subject_train_all <- rbind(subject_train, subject_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

	select_features <- features[grep("mean\\(\\)|std\\(\\)",features[,2]),]
	X_all <- X_all[,select_features[,1]]

# 3. Uses descriptive activity names to name the activities in the data set.

	colnames(Y_all) <- "activity"
	Y_all$activity_label <- factor(Y_all$activity, labels = as.character(activity_labels[,2]))
	activity_label <- Y_all[,-1]

# 4. Appropriately labels the data set with descriptive variable names.
	colnames(X_all) <- features[select_features[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	colnames(subject_train_all) <- "subject"
	all <- cbind(X_all, activity_label, subject_train_all)
	all_mean <- all %>% group_by(activity_label, subject) %>% summarize_each(funs(mean))
	write.table(all_mean, file = "./UCI HAR Dataset/tidy.txt", row.names = FALSE, col.names = TRUE)