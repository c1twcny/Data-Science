###############################################################################
## run_analysis() performs the following functions:
## (1) Check out whether a data folder "UCI HAR Dataset" exists in the current
##     working directory.
## (2) Merges both training and test sets into one data set
## (3) Extracts the mean & standard deviation on attributes for each   
##     measurement.
## (4) Use descriptive activity names to name the activities in the data set 
## (4) Label the data set with descriptive variable names
## (5) Create a tidy data set with the average of each variable for each
##     activity and each subject
##
##
##
##
###############################################################################
##
## Created by: TWC
## Date:       Nov. 23, 2014	
## Version:    0.1.0
## License:    GNU GPL-3.0

##
## run_analysis()
##


run_analysis <- function(directory="/home/u863713/Coursera/Data-Science/R-programming") {

## import some libraries

    library(data.table)
    library(dplyr)
    library(tidyr)

## set up initial parameters

    dir_wd 	<- directory
    dir_data 	<- "UCI HAR Dataset"
    file_test	<- c("X_test.txt", "y_test.txt", "subject_test.txt")
    file_train	<- c("X_train.txt","y_train.txt","subject_train.txt")
    label	<- c("_data","_label","_subject")

## Verify the current working directory does contain "UCI HAR Dataset" folder
    
    if (!dir.create(file.path(getwd(),dir_data), showWarnings=FALSE)) {
        cat(dir_data, "exists in the current working directory.\n")
        dir_data <- paste(dir_wd,"/",dir_data,"/", sep="")
#        print(dir_data)
#        list.files(dir_data) 
    } else {
        stop(dir_data, " is missing")
    }


    label <- read.table(paste(dir_data,"/","features.txt",sep=""))
    label <- as.factor(label[, 2])
## read the following data files from /test and /train directory
## /test directory
##   |____ X_test.txt
##   |____ subject_test.txt
##   |____ y_test.txt
##
## /train directory
##   |____ X_train.txt
##   |____ subject_test.txt
##   |____ y_train.txt
##

    dir_test <- paste(dir_data,"test","/",sep="")
    dir_train<- paste(dir_data,"train","/",sep="")
    data_test<- sapply(file_test, function(x) {
                       tdir <- paste(dir_test,x,sep="")
                       read.table(tdir)})
    test_data	  <- data_test[[1]]
    test_activity <- data_test[[2]]
    test_subject  <- data_test[[3]]
    data_train<-sapply(file_train, function(x) {
                       tdir <- paste(dir_train,x,sep="")
                       read.table(tdir)})
    train_data	  <- data_train[[1]]
    train_activity<- data_train[[2]]
    train_subject <- data_train[[3]]

## replace numbers with descriptive labels on activity data set
    test_activity <- tbl_dt(sapply(test_activity[, 1], function(x) switch(x,
                            "Walking","Walking_upstairs","Walking_downstairs",
                            "Sittings","Standing","Laying")))
    train_activity<- tbl_dt(sapply(train_activity[, 1], function(x) switch(x,
                            "Walking","Walking_upstairs","Walking_downstairs",
                            "Sittings","Standing","Laying")))

## read all the lables from "features.txt" file
    label <- read.table(paste(dir_data,"features.txt",sep=""))
    
## merge train_data & test_data
    train_data <- cbind(train_subject, train_activity, train_data)
    test_data  <- cbind(test_subject, test_activity, test_data)
    tfull_data <- rbind(train_data, test_data)

## Assign descriptive lables to each column
    partial_data <- tfull_data[, 3:ncol(tfull_data)-2]
    colnames(partial_data) <- label[, 2]
    names(partial_data)
    full_data <- tbl_dt(cbind(tfull_data[,1:2], partial_data))
#    setnames(full_data, 1:2, c("Subject", "Activity"))   

## Extract index of attributes containins mean and standard deviation of
## measurements
    meanIDX <- grep("\\-mean[a-zA-Z]*\\(\\)", label[, 2]) #partial_data
    stdIDX  <- grep("\\-std[a-zA-Z]*\\(\\)", label[, 2])  #partial_data
    table(c(meanIDX, stdIDX))

## Extract columns contain attributes related to mean & S.D. of measurements
    subData <- partial_data[, c(meanIDX, stdIDX)]

## Create a new data set containing subject, activities, mean & S.D.
## measurements
    new_subData <- tbl_dt(cbind(tfull_data[, 1:2], subData))
    setnames(new_subData, 1:2, c("Subject", "Activity"))
    head(new_subData)
} # end of main function

 
