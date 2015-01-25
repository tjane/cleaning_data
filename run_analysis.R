path = "C:/Users/a/Desktop/R/cleaning data/UCI HAR Dataset"
setwd(path)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
feature <- read.table("features.txt")
col <- grep("mean()|std()",as.character(feature[,2]))

#Merges the training and the test sets to create one data set.
dir_train <- paste(path,'/train',sep = "")
dir_test <- paste(path,'/test',sep = "")
dir_merge <- paste(path,'/merge',sep = "")
all_files <- list.files(dir_train)
all_dirs <- sapply(list.dirs(dir_train),basename)
for(i in seq_along(all_files)){
  if(!(all_files[i] %in% all_dirs)){
    file_name = unlist(strsplit(all_files[i],"[_]"))
    test_name = paste(c(file_name[1:length(file_name) - 1],'test.txt'),collapse = "_")
    merge_name = paste(c(file_name[1:length(file_name) - 1],'merge.txt'),collapse = "_")
    table_train = read.table(paste(c(dir_train,all_files[i]),collapse = "/"))
    table_test = read.table(paste(c(dir_test,test_name),collapse = "/"))
    table_merge = rbind(table_train,table_test)
    if(merge_name == "X_merge.txt"){
      #Appropriately labels the data set with descriptive variable names. 
      names(table_merge) <- as.character(feature[,2])
      write.table(table_merge[,col],paste(c(dir_merge,merge_name),collapse = "/"),row.names = FALSE,quote = FALSE)
    }
    else{
      #Uses descriptive activity names to name the activities in the data set
      if(merge_name == "y_merge.txt"){
        activity <- as.character(read.table("activity_labels.txt")[,2])
        table_merge <- sapply(table_merge[,1],function(x) activity[x])
      }
      write.table(table_merge,paste(c(dir_merge,merge_name),collapse = "/"),row.names = FALSE,col.names = FALSE)
    }
  } 
}
#From the data set in step 4, creates a second
#independent tidy data set with the average of each variable for each activity and each subject.
path = "C:/Users/a/Desktop/R/cleaning data/UCI HAR Dataset"
setwd(path)
X <- read.table(paste(c(path,"/merge/X_merge.txt"),collapse = ""),head = TRUE)
y <- read.table(paste(c(path,"/merge/y_merge.txt"),collapse = ""))
subject <- read.table(paste(c(path,"/merge/subject_merge.txt"),collapse = ""))
data <- data.frame(subject = subject[,1],activity = as.character(y[,1]),X)


sub <- unique(subject[,1])
activity <- unique(as.character(y[,1]))
result <- data.frame()
for(s in seq_along(sub)){
  for(a in seq_along(activity)){
    choose <- subset(data,subject == sub[s] & activity == activity[a],select = c(3:ncol(data)))
    t <- mapply(mean,choose)
    result <- rbind(result,cbind(sub[s],cbind(activity[a],t(t))))
  }
}
names(result) <- names(data)
write.table(result,"tidy_data.txt",row.names = FALSE)
