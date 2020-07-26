library(dplyr)

# First off read data into R

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt", col.names="activity")
subject_train <- read.table(file.path("train","subject_train.txt"), col.names="subject")

x_test  <- read.table("test/X_test.txt")
y_test  <- read.table("test/y_test.txt", col.names="activity")
subject_test  <- read.table(file.path("test","subject_test.txt"),col.names="subject")

# Merge data

train_data <- cbind(x_train,subject_train,y_train)
test_data <- cbind(x_test,subject_test,y_test)
final_data <- rbind(train_data, test_data)

# read features
features <- read.table("features.txt", col.names=c("id","feature"))

# find indexes of only mean and std
feature_index <- grep("(mean\\(\\)|std\\(\\))", features$feature)
feature_index <- append(feature_index,c(ncol(final_data)-1, ncol(final_data)))

# apply indexes to dataset
indexed_data <- final_data[, feature_index]

# replace activity numbers with activity names
activities <- read.table("activity_labels.txt", col.names=c("id", "feature"))
for (i in 1:nrow(activities)) {
  indexed_data$activity[indexed_data$activity == activities[i, "id"]] <- as.character(activities[i, "feature"])
}

# give each column a name from the features lits
feature_index <- grep("(mean\\(\\)|std\\(\\))", features$feature)
feature_names <- features$feature[feature_index]
feature_names <- append(feature_names,c("subject","activity"))
names(indexed_data) <- feature_names

# create tidy data set
tidy_data <- mutate(indexed_data, subject = as.factor(subject)) %>% 
  group_by(activity, subject) %>% 
  summarize_each(funs(mean)) %>% 
  ungroup

# write tidy data set into separate file
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)