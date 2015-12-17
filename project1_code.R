# Author: Barry Obondo
# Data Science Specialization: Johns Hopkins University on Coursera
# Reproducible Research
# Project 1
###############################################################

# Set WD

# setwd("C:/Users/jabondo/desktop/Dropbox/Coursera_DSS/5_ReproRsch/project1")
# setwd("C:/Users/barry_000/Desktop/Dropbox/Coursera_DSS/5_ReproRsch/project1")
 setwd("C:/data/Dropbox/Coursera_DSS/5_ReproRsch/project1")

# load required packages
library(dplyr)
library(lubridate)
library(ggplot2)

filezip <- "repdata%2Fdata%2Factivity.zip"

# download dataset file if does not exist
if(!file.exists(filezip)){
    datasetURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    # for the URL, 'https' does not work when running the code in knitr
    download.file(datasetURL, destfile="activity.zip")}

# unzip dataset if not yet unzipped
if(!file.exists("activity.csv")){
    unzip("activity.zip")}

# Load data
rawdata <-read.csv("activity.csv")
rawdata <- tbl_df(rawdata)

# convert date to POSIXct (date) format
rawdata$date <- ymd(rawdata$date)

str(rawdata) # check variable format conversions


# remove missing values
rawdata_noNA <- na.omit(rawdata)


# Histogram for total number of steps taken per day

# aggregate steps by day
steps_per_day <- aggregate(steps ~ date, rawdata_noNA, sum)

# histogram
hist(steps_per_day$steps, main="Steps Taken per Day",  
     xlab="Number of steps", ylab="Frequency", col="Grey")


# Mean and median of total number of steps taken per day (removed NA)
mean_stepsperday <- mean(steps_per_day$steps)
median_stepsperday <- median(steps_per_day$steps)

mean_stepsperday
median_stepsperday 

# Time series plot of time interval (x) and average number of steps taken across all days (y)
steps_per_interval <- aggregate(steps ~ interval, rawdata, mean)

timeintervalplot <- ggplot(steps_per_interval, aes(interval, steps, color="steps")) + 
    geom_line() +
    ggtitle(expression("Time-Series of the Average Number of Steps for a Given Time Interval")) +
    xlab("Time Interval for Aggregating Step Counts (minutes)") +
    ylab("Average Number of Steps")
print(timeintervalplot)

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

#find position of max value
max_index <- which.max(steps_per_interval[,"steps"])
max_index
# look up max interval value in the index position
max_interval <- steps_per_interval[max_index, 1]  
max_interval

# Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
sumNAvalues <- length(which(is.na(rawdata$steps)))
sumNAvalues


# Devise a strategy for filling in all of the missing values in the dataset. 

# Strategy: replace NAs with the mean value for each day of the week

# Initialize dataframe
imputedata <-tbl_df(rawdata)

# create new variable to assign days of the week
imputedata$day <- weekdays(as.Date(imputedata$date))

# create data frame with mean for each day
meanDay <- as.data.frame(aggregate(steps ~ day, imputedata, mean))

# index missing values
# NA_index <- which(is.na(imputedata$steps))
# NA_index

# create merge dataset with merged mean day
imputedata2 <- merge(imputedata, meanDay, by=c("day"))
# rename(imputedata2, steps=steps.x, dayAverage=steps.y)
names(imputedata2)[2:5] <- c("steps", "date", "interval", "dayAverage")
# sort by date and interval
imputedata2 <- imputedata2[order(imputedata2$date, imputedata2$interval),]

# create new variable that will be populated with 'steps' 
# unless it is NA for which it will be populated by the 'Daily Average' value

imputedata2$imputedSteps <- ifelse(is.na(imputedata2$steps), 
                                   imputedata2$dayAverage, imputedata2$steps)
# New dataset with imputed values (drop 'steps' dataset)
imputedFinal <- select(imputedata2, date, day, imputedSteps, interval)

# Make a histogram of the total number of steps taken each day 
# aggregate steps by day for the imputed data set
imputedSteps_per_day <- aggregate(imputedSteps ~ date, imputedFinal, sum)

# histogram of steps by day for the imputed data set
hist(imputedSteps_per_day$imputedSteps, main="Steps Taken per Day (Imputed Dataset)",  
     xlab="Number of steps", ylab="Frequency", col="Grey")

# Mean and median of steps taken per day
mean_ImputedStepsperday <- mean(imputedSteps_per_day$imputedSteps)
median_Imputedstepsperday <- median(imputedSteps_per_day$imputedSteps)
mean_ImputedStepsperday
median_Imputedstepsperday

# Q: Differ from previous data??

# table for comparison of mean and median for data with removed missing values, and that with imputed values
imputed_vs_noNAs <- matrix(c(mean_stepsperday, median_stepsperday, mean_ImputedStepsperday, median_Imputedstepsperday ),ncol=2,byrow=TRUE)
colnames(imputed_vs_noNAs) <- c("Mean","Median")
rownames(imputed_vs_noNAs) <- c("Missing Values removed","Imputed Missing Values")
imputed_vs_noNAs  <- as.table(imputed_vs_noNAs)
imputed_vs_noNAs

# Q: What is the impact of imputing missing values on the estimates of the total daily number of steps???

############################################################
## For this part the weekdays() function may be of some help here.
## Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
# indicating whether a given date is a weekday or weekend day.

imputedFinal$wkdwknd <- ifelse(imputedFinal$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")


# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

# calculate mean of steps for each interval
imputedStepsInterval <- aggregate(imputedSteps ~ interval, imputedFinal, mean)

# group by weekday and weekend
groupByday <- group_by(imputedFinal, interval, wkdwknd)
averageStepsDay <- summarize(groupByday, averageImputedSteps = mean(imputedSteps))

# line plots for average weekend and weekday steps by interval
wkdwkndAverage <- ggplot(averageStepsDay, aes(x=interval, y=averageImputedSteps)) + 
  geom_line(color="blue") + 
  facet_wrap(~ wkdwknd, nrow=2, ncol=1) +
  ggtitle(expression("Average Number of Steps (from Imputed data) by Type of Day and Time Interval")) +
  xlab("Time Interval for Aggregating Step Counts (minutes)") +
  ylab("Average Number of Steps")

print(wkdwkndAverage)





