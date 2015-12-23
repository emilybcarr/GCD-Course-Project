#Here are the data for the project:
    #https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#A full description is available at the site where the data was obtained:
    #http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# You should create one R script called run_analysis.R that does the following. 
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## Step 1:
    ## Manually download the data & save to appropriate folder
    ## Set working directory to that folder
        setwd("~/Data Science Training/Getting and Cleaning Data")
    ## Save the data sets into variables (tests: 2947 obs. of 1 var; trains: 7352 obs. of 1 var)
        features <- read.csv("UCI HAR Dataset/features.txt", sep = " ",header=FALSE)
        activities <- read.csv("UCI HAR Dataset/activity_labels.txt", sep = " ",header=FALSE)
        testx <- read.csv("UCI HAR Dataset/test/X_test.txt",header=FALSE)
        trainx <- read.csv("UCI HAR Dataset/train/X_train.txt",header=FALSE)
        testy <- read.csv("UCI HAR Dataset/test/y_test.txt",header=FALSE)
        trainy <- read.csv("UCI HAR Dataset/train/y_train.txt",header=FALSE)
        subjecttest <- read.csv("UCI HAR Dataset/test/subject_test.txt",header=FALSE)
        subjecttrain <- read.csv("UCI HAR Dataset/train/subject_train.txt",header=FALSE)
    ## Create the test and train sets
        testset <- data.frame(subjecttest,testy,testx)
        trainset <- data.frame(subjecttrain,trainy,trainx)
        colnames(testset) <- c("subject", "y", "x")
        colnames(trainset) <- c("subject", "y", "x")
    ## Merge the training and test sets to create one data set.
        dataset <- rbind.data.frame(testset, trainset)
        
## step 2: Extract only the measurements on the mean and standard deviation for each measurement.
    ## Split up column x and name the columns
        featurenames <- as.vector(features$V2)
        library(stringr)
        dataset2 <- str_split_fixed(dataset$x, " ", n = 561) #not sure if this is right
        colnames(dataset2) <- featurenames
    ## Eliminate all feature columns except those containing "std()" and "mean()"
        stdcols <- grep("std", colnames(dataset2), value = TRUE)
        meancols <- grep("mean", colnames(dataset2), value = TRUE)
        stdmeancols <- c(stdcols, meancols)
        dataset2_stdmean <- dataset2[,(colnames(dataset2) %in% stdmeancols)]
    ## Combine with subject and y columns
        dataset_stdmean <- data.frame(dataset$subject, dataset$y, dataset2_stdmean)
    
## Step 3: Use descriptive activity names to name the activities in the data set.
    ## Confirm that the "y" column is the activity. 
        #levels(as.factor(dataset$y))
            ##answer: [1] "1" "2" "3" "4" "5" "6"
            ## there are 6 activities, so this seems to align.
    ## Change the "y" values to be the activity name instead of the number.
        #looked for a function to do this (e.g. gsub) but could not get it to work & had to use nested loop
        dataset_stdmean$dataset.y <- as.character(dataset_stdmean$dataset.y)
        for (i in 1:nrow(dataset_stdmean)) {
            for (j in 1:nrow(activities)) {
                if (dataset_stdmean$dataset.y[i] == as.character(j)) {
                    dataset_stdmean$dataset.y[i] <- as.character(activities[j,2])
                }
            }
        }

## Step 4: Appropriately label the data set with descriptive variable names. 
    # Already done for the feature names; need to update the first two columns
        colnames(dataset_stdmean)[1] <- "Subject"
        colnames(dataset_stdmean)[2] <- "Activity"

## Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each
## variable for each activity and each subject.
    ## Sort the table by Subject from lowest to highest & by Activity alphabetically
        dataset_stdmean_sorted <- dataset_stdmean[order(dataset_stdmean["Subject"], dataset_stdmean["Activity"]), ]
        activities_sorted <- activities[order(activities["V2"]), ]
    ## Determine how many times each subject performed each activity
        numsubjects <- max(dataset_stdmean_sorted$Subject)
        numactivities <- nrow(activities)
        numrowsneeded <- numsubjects*numactivities
        #num_to_avg <- integer(length = numrowsneeded)
        dataset_stdmean_averages <- as.data.frame(matrix(nrow = numrowsneeded, ncol = ncol(dataset_stdmean_sorted)))
        colnames(dataset_stdmean_averages) <- colnames(dataset_stdmean_sorted)
        
        k <- 0
        a <- 1
        for (i in 1:numsubjects) {
            for (j in 1:numactivities) {
                k <- k + 1
                num_to_avg <- sum(dataset_stdmean_sorted$Subject == i & dataset_stdmean_sorted$Activity == activities_sorted[j,2])
                z <- a + num_to_avg - 1
                dataset_stdmean_averages[k,1] <- i
                dataset_stdmean_averages[k,2] <- as.character(activities_sorted[j,2])
                for (m in 3:ncol(dataset_stdmean_sorted)) {
                    dataset_stdmean_averages[k,m] <- mean(as.numeric(as.character(dataset_stdmean_sorted[a:z,m])),na.rm = TRUE)
                }
                a <- z + 1
            }
        }
        
    ## print the resulting data frame to a txt file (commented out to avoid recreating file)
        #write.table(dataset_stdmean_averages, file = "UCIHAR_stdmean_averages.txt", row.names = FALSE)
        