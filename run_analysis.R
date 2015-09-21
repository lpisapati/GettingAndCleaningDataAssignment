runScript <- function() {
  # refer https://class.coursera.org/getdata-032/human_grading/view/courses/975116/assessments/3/submissions
  
  # download the zip file from the given URL
  zipFileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  downloadedZipFile = "data.zip"
  downloadFolder = "tmp"
  pTmp <- function(fn) {
    return (paste(downloadFolder, fn, sep = "/"))
  }
  rootPath = pTmp("UCI HAR Dataset")
  testPath = pTmp("UCI HAR Dataset/test")
  trainPath = pTmp("UCI HAR Dataset/train")
  # do only if tmp folder DOES NOT exists
  if (!file.exists(downloadFolder)) {
    
    print("creating tmp folder and downloading the zip file.")
    # create a tmp folder
    dir.create(downloadFolder)
    # download the file file into tmp folder
    download.file(zipFileUrl, pTmp(downloadedZipFile), method = "curl")
    # unzip the file
    unzip(pTmp(downloadedZipFile), exdir=downloadFolder)
  }
  
  
  ##step-1: Merges the training and the test sets to create one data set.
  # create tables sets for test data
  testSubjectTable = read.table(paste(testPath, "subject_test.txt", sep = "/"))
  testXTable = read.table(paste(testPath, "X_test.txt", sep = "/"))
  testYTable = read.table(paste(testPath, "Y_test.txt", sep = "/"))
  # create tables sets for train data
  trainSubjectTable = read.table(paste(trainPath, "subject_train.txt", sep = "/"))
  trainXTable = read.table(paste(trainPath, "X_train.txt", sep = "/"))
  trainYTable = read.table(paste(trainPath, "Y_train.txt", sep = "/"))
  # merge train and test - subject data
  subjectTable <- rbind(testSubjectTable, trainSubjectTable)
  # merge train and test - subject data
  xTable <- rbind(testXTable, trainXTable)
  # merge train and test - subject data
  yTable <- rbind(testYTable, trainYTable)
  
  dataSet = cbind(subjectTable, yTable, xTable)
  
  ##step-2: Extracts only the measurements on the mean and standard deviation for each measurement. 
  # read features file into a table
  featuresTable = read.table(paste(rootPath, "features.txt", sep = "/"))
  # read mean and sd from the featuresTable
  featureNameList = featuresTable$V2[grep("(mean|std)\\(\\)", featuresTable[,2])]
  #print(featureNameList)
  # name the columns
  names(dataSet) = c("Subject", "Activity", as.character(featureNameList))
  #print(str(dataSet))
  
  ##step-3: Uses descriptive activity names to name the activities in the data set
  activityLabelTable = read.table(paste(rootPath, "activity_labels.txt", sep = "/"))
  #print(activityLabelTable)
  dataSet$Activity = activityLabelTable[match(dataSet$Activity, activityLabelTable$V1), 2]
  #print(head(dataSet))
  
  ##step-4: Appropriately labels the data set with descriptive variable names. 
  # rename t for time and f for frequency
  names(dataSet) = gsub("^t","Time",gsub("^f","Frequency",names(dataSet))) 
  # rename Acc for Accelerometer and Gyro for Gyroscope
  names(dataSet) = gsub("Acc","Accelerometer",gsub("Gyro","Gyroscope",names(dataSet)))
  # rename mean() for MEAN and std() for SD
  names(dataSet) = gsub("mean\\(\\)","MEAN",gsub("std\\(\\)","SD",names(dataSet)))
  # rename mag for Magnitude and bodybody for Body
  names(dataSet) = gsub("Mag","Magnitude",gsub("BodyBody","Body",names(dataSet)))
  #print(names(dataSet))
  
  ##step-5: From the data set in step 4, creates a second, independent tidy data 
  ##set with the average of each variable for each activity and each subject.
  dataSet2 = colMeans(dataSet[,-(1:2)])
  # remove unnamed columns
  dataSet2 = dataSet[,1:68]
  print(head(dataSet2))
  
  ## Make cookbook
  # load knitr package
  install.packages("knitr")
  library(knitr)
  # generate codebook.md and html file
  knit2html("codebook.Rmd")
  # open the html file in default browser
  browseURL(paste0('file://', file.path(getwd(), "codebook.html")))
  
  # delete tmp folder
  if (file.exists(downloadFolder)) {
    print("deleting tmp folder")
    unlink(downloadFolder, recursive = TRUE)
  }
}