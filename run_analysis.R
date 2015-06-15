#Data Science Track
#Getting and Cleaning Data
#Jeff Forbeck
library (plyr)
library (dplyr)
library(stringr)
library(Hmisc)
library(sqldf)


setwd("C:/Users/forbecj/Dropbox/DataScience/DataCleaning/Week 3/Data/UCI HAR Dataset")

#Read X train and y train

train_x <- read.table("./train/X_train.txt")

train_y <- read.table("./train/y_train.txt")

#Read X test and y test

test_x <- read.table("./test/X_test.txt")

test_y <- read.table("./test/y_test.txt")

#Rename dependent variable - prepare for merge
test_y  <- plyr::rename(test_y,c("V1" = "y_label"))
train_y  <- plyr::rename(train_y,c("V1" = "y_label"))

#Read description/label data
activity_labels <- read.table("./activity_labels.txt")
subjects_train <- read.table("./train/subject_train.txt", col.names=c("subject"))
subjects_test <- read.table("./test/subject_test.txt", col.names=c("subject"))
features <- read.table("./features.txt")



#Merge activity names for more descriptive lables

merge_test_y <- sqldf("select V2 as activity, test_y.y_label from activity_labels 
                      inner join test_y  on activity_labels.V1=test_y.y_label")
merge_train_y <- sqldf("select V2 as activity, train_y.y_label from activity_labels 
                      inner join train_y  on activity_labels.V1=train_y.y_label")


#Merge x and y datasets
test <- cbind(test_x, merge_test_y)
train <- cbind(train_x, merge_train_y)


#Add subjects
train <- cbind(train, subjects_train)
test <- cbind(test, subjects_test)

#Concatanate data
uci <- rbind(test, train)

# Add descriptive variable names

rename_list <- ""
for (i in 1:length(features$V2)){
  y <- paste(as.character(features[i,"V2"]),str_trim(i),sep=" ") # new variable name
  x <- paste(str_trim("V"),str_trim(i),sep="") # Old variable name
  uci <- plyr::rename(uci,setNames(y,x)) # Rename old variable name to new 
  
}

#Select variables for analysis
uci_select_measures <- uci[c(grep("mean", colnames(uci),ignore.case=TRUE,value=TRUE),
                             grep("std", colnames(uci),ignore.case=TRUE,value=TRUE),
                             grep("activity", colnames(uci),ignore.case=TRUE,value=TRUE),
                             grep("subject", colnames(uci),ignore.case=TRUE,value=TRUE))]


#Summarize and write out data set
uci_avg <- uci_select_measures %>% group_by(activity, subject) %>% summarise_each(funs(mean))
write.table(uci_avg,'././uci_analysis.txt')

#Write out list of variables to be included in Codebook

write.table(data.frame(colnames(uci_select_measures)),'././uci_analysis_var_names.txt',row.name=FALSE )



# Code for creating dev data
# dev_test_x <- head(test_x, n=100)
# dev_test_y<- head(merge_test_y, n=100)
# 
# dev_train_x <- head(train_x, n=100)
# dev_train_y<- head(merge_train_y, n=100)
# 
# 
# test <- cbind(test_x, merge_test_y)
# train <- cbind(train_x, merge_train_y)