# # you want a run_analysis R script, 
# a ReadMe markdown document, 
# a Codebook markdown document, 
# and a tidy data text file (this last goes on Coursera).

# 'features_info.txt': Shows information about the variables used on the feature vector.
# 'features.txt': List of all features.
# 'activity_labels.txt': Links the class labels with their activity name.
# 'train/X_train.txt': Training set.
# 'train/y_train.txt': Training labels.
# 'test/X_test.txt': Test set.
# 'test/y_test.txt': Test labels.
# 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.

# Clears workspace
rm(list=ls())

# Reads Training and Test set and combines them
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X <- rbind(X_train, X_test)

# Reads Training and Test labels and combines them
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
Y <- rbind(Y_train, Y_test)

# Reads Training and Test subject labels and combines them
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject <- rbind(subject_train, subject_test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# Reads features
features <- read.table("UCI HAR Dataset/features.txt")

# Extracts index of features relevant to mean or standard deviation
mean_sd_index <- grep("-mean\\(\\)|-std\\(\\)", features$V2)

# Creates data frame of mean and standard deviation for each measurement
X_mean_sd <- X[, mean_sd_index]

# Modifies feature names and assigns them to data set (X)
names(X) <- features[mean_sd_index, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

## 3. Uses descriptive activity names to name the activities in the data set

# Extracts activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Modifies activity labels and assigns them to data set (Y)
activity_labels[, 2] = as.character( activity_labels[, 2])
activity_labels[, 2] = tolower(activity_labels[, 2])
activity_labels[, 2] = gsub("_", "", activity_labels[, 2])
names(Y) <- "activity"

## 4. Appropriately labels the data set with descriptive variable names. 

# Assigns heading to subject column
names(subject) <- "subject"

# Combines subject, Y and X data sets
cleaned <- cbind(subject, Y, X)

# Writes combined data set to text file
write.table(cleaned, "cleaned_data.txt")

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Counts the number of unique subjects in data set
unique_subjects = unique(subject)[,1]
number_unique_subjects = length(unique(subject)[,1])

# Counts the number of activities in the data set
number_activities = length(activity_labels[,1])
number_columns = dim(cleaned)[2]
result = cleaned[1:(number_unique_subjects*number_activities), ]

# For loops across unique subjects and number of activities to create average of each variable
row = 1
for (s in 1:number_unique_subjects) {
        for (a in 1:number_activities) {
                result[row, 1] = unique_subjects[s]
                result[row, 2] = activity_labels[a, 2]
                output <- cleaned[cleaned$subject==subject & cleaned$activity_labels==activity_labels[a, 2], ]
                result[row, 3:number_columns] <- column_means(output[, 3:number_columns])
                row = row+1
        }
}

# Writes new data set to text file
write.table(result, "new_data_set.txt")