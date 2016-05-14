
# This script performs the required operations 
# for the Getting and Cleaning Data  Course Project
# Marc Fuller
# May 13, 2016
###############
# Before running this script insure that the working directory 
# is set to the same location where the data file described in the 
# read me is unzipped.
# The result will be a file named tidy.txt that contains the averages all feature
# variables over individual and activity that contain either mean or std in
# their name. See the read me file for further details.

run_analysis <- function(){
      library(dplyr)
#First grab the data from the training and test subdirectories
      setwd("train")
      x_train <- read.table("x_train.txt")
      y_train <- read.table("y_train.txt")
      subj_train <- read.table("subject_train.txt")
      setwd("../test")
      x_test <- read.table("x_test.txt")
      y_test <- read.table("y_test.txt")
      subj_test <- read.table("subject_test.txt")
# Now bind the feature variables with theit respective subject and activity columns 
      train <- cbind(y_train,x_train)
      train <- cbind(subj_train,train)
      test <- cbind(y_test,x_test)
      test <- cbind(subj_test,test)
# Return to the original directory and stack the training and 
# test data into one variable.
      setwd("..")
      all_dat <- rbind(train,test)
# Set names of the first two columns to
# subject and activity 
      colnames(all_dat)[1:2] <- c("subject","activity")
# Now sort the rows, first according to subject, then according to activity
      all_dat<- arrange(all_dat,subject,activity)
# get the names of all feature variables 
      col_labels <- read.table("features.txt",as.is=2)
      col_labels <- col_labels[,2]
# now massage the labels a little to make them more readable 
      col_labels <- gsub('[-()]', '', col_labels)
      col_labels <- gsub("^t","Time",col_labels)
      col_labels <- gsub("^f","Freq",col_labels)
      col_labels <- gsub("mean","Mean",col_labels)
      col_labels <- gsub("std","StdDev",col_labels)
# Make the column names the new improved 
# feature variable names
      colnames(all_dat)[3:(length(col_labels)+2)] <- col_labels
# now get the activity names and substitute them for the activity numbers
      activity_names <- read.table("activity_labels.txt")
# simple way to do this is to turn activity numerics into factors
# and then rename the levels
      all_dat$activity <- as.factor(all_dat$activity)
      levels(all_dat$activity) <- activity_names[,2]
# search for all feature variable that have StdDev or mean 
# (case insenitive) in their name
      col_sel <- grep("Mean|StdDev",colnames(all_dat),ignore.case=1 )
# add in the subject and activity columns      
      col_sel <- c(1,2,col_sel)
# now throw away everything else and insure order is correct     
      mean_std <- all_dat[,col_sel]
      mean_std <- arrange(mean_std,subject,activity)
# mean_std above is the data as aggregated over both training and test sets
# all that remains is the final step to average it over subject and activity
# which is done in the following line      
      final <- mean_std %>% group_by(subject,activity) %>% summarise_each(funs(mean))
# By  changing the width and line maximum options we can print out the data in 
# a format that is both tidy and visually easy to look at.
# note we have limited the signifcant digits to 3 since the accuracy and precision 
# of cmmercial devices is not even that accurate.  Further digits are noise.
# the result appears in file "tidy.txt"
      options(width=3000)
      options(max.print=30000)
      capture.output(print.data.frame(final,row.names=F,digits=3,print.gap=4),file="tidy.txt")

}