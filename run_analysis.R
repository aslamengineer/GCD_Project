run_analysis <- function(directory)
{  
  setwd(directory)

  features <- read.table("../UCI HAR Dataset/features.txt", quote="\"")
  subject_train <- read.table("../UCI HAR Dataset/train/subject_train.txt", quote="\"")
  X_train <- read.table("../UCI HAR Dataset/train/X_train.txt", quote="\"")
  y_train <- read.table("../UCI HAR Dataset/train/y_train.txt", quote="\"")
  subject_test <- read.table("../UCI HAR Dataset/test/subject_test.txt", quote="\"")
  X_test <- read.table("../UCI HAR Dataset/test/X_test.txt", quote="\"")
  y_test <- read.table("../UCI HAR Dataset/test/y_test.txt", quote="\"")
  
  labels <- make.names(features$V2, unique = TRUE)
  
  colnames(X_train) <- labels
  colnames(X_test) <- labels
  colnames(y_train) <- "Activity"
  colnames(y_test) <- "Activity"
  colnames(subject_train) <- "Subject"
  colnames(subject_test) <- "Subject"
  
  for(i in 1:7352)
  {
    if(y_train$Activity[i] == 1)
    {
      y_train$Activity[i] = "WALKING"
    }
    else if(y_train$Activity[i] == 2)
    {
      y_train$Activity[i] = "WALKING_UPSTAIRS"
    }
    else if(y_train$Activity[i] == 3)
    {
      y_train$Activity[i] = "WALKING_DOWNSTAIRS"
    }
    else if(y_train$Activity[i] == 4)
    {
      y_train$Activity[i] = "SITTING"
    }
    else if(y_train$Activity[i] == 5)
    {
      y_train$Activity[i] = "STANDING"
    }
    else if(y_train$Activity[i] == 6)
    {
      y_train$Activity[i] = "LAYING"
    }
  }
  
  for(i in 1:2947)
  {
    if(y_test$Activity[i] == 1)
    {
      y_test$Activity[i] = "WALKING"
    }
    else if(y_test$Activity[i] == 2)
    {
      y_test$Activity[i] = "WALKING_UPSTAIRS"
    }
    else if(y_test$Activity[i] == 3)
    {
      y_test$Activity[i] = "WALKING_DOWNSTAIRS"
    }
    else if(y_test$Activity[i] == 4)
    {
      y_test$Activity[i] = "SITTING"
    }
    else if(y_test$Activity[i] == 5)
    {
      y_test$Activity[i] = "STANDING"
    }
    else if(y_test$Activity[i] == 6)
    {
      y_test$Activity[i] = "LAYING"
    }
  }
  
  library(dplyr)
  
  train_mean <- select(X_train, contains("mean"))
  
  train_sd <- select(X_train, contains("std"))
  
  training<- cbind(subject_train,y_train,train_mean,train_sd)
  
  test_mean <- select(X_test, contains("mean"))
  
  test_sd <- select(X_test, contains("std"))
  
  testing<- cbind(subject_test,y_test,test_mean,test_sd)
  
  one_data <- rbind(training,testing)
  
  inGroup <- group_by(one_data,Activity,Subject)
  
  solution <- summarise_each(inGroup, funs(mean))
  
  write.table(solution, file="tidy.txt",row.names = FALSE)

  
}