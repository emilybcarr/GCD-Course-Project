# GCD-Course-Project
Repository for the Coursera Getting &amp; Cleaning Data course project  
  
README for run_analysis.R - 12/23/15  
***
__VERSION:__ Created using RStudio Version 0.99.484  
  
  
__DATA:__  
* The data used for this script is available [here] (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
* A full description is available at the site where the data was obtained [here]   (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
  
  
__SCRIPT DETAILS:__  
* The script assumes the data is stored in the working directory.
* The script does the following main tasks:  
  + Merges the training and the test sets to create one data set.
  + Extracts only the measurements on the mean and standard deviation for each measurement. 
  + Uses descriptive activity names to name the activities in the data set
  + Appropriately labels the data set with descriptive variable names. 
  + From the data set in the step above, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
* Output is a .txt file containing a tidy data set with the average of each variable for each activity and each subject.
