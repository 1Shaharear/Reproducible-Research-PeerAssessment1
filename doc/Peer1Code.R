#Loading required libraries for cleaning and managing the data
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(gridExtra)
library(chron)

#Downloading the data. First we check if we have the file -
#-already in our working directory. If not we download it.

if(!file.exists("Factivity")){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, "Factivity")
  unzip("Factivity") 
  activity <- read.csv("activity.csv")
  activity <- data.frame(activity)
}

if(file.exists("Factivity")){
  activity <- read.csv("activity.csv")
  activity <- data.frame(activity)
}

#Getting an idea of the data. With the summary we realize that we have 2304 NA´s
head(activity)
summary(activity)
str(activity)

#Processing the data
activity[,"interval"] <- as.factor(activity[,"interval"])
activity[,"date"] <- ymd(activity[,"date"])

##Question 1: What is mean total number of steps taken per day?
#For this part of the assignment, you can ignore the missing values in the dataset.
#1. Calculate the total number of steps taken per day
#2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
#3. Calculate and report the mean and median of the total number of steps taken per day

stepsDay <- aggregate(steps ~ date, activity, sum)
stepsDay %>%
  ggplot(aes(x = steps, fill = "Steps")) +
  geom_histogram(size = 2, bins=5) +
  labs(title = "Total steps per day", subtitle = "From 02 of Oct to 29 Nov") +
  ylab("Steps") +
  xlab("Count") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  guides(fill=FALSE)

mean(stepsDay$steps)
median(stepsDay$steps)

##Question 2 What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

intervalSteps <- aggregate(steps ~ interval, activity, FUN=sum)
plot(intervalSteps$interval, intervalSteps$steps, 
     type = "l", lwd = 1,
     xlab = "Interval", 
     ylab = "Steps",
     color="blue",
     main = "Steps / 5-Minute Interval")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervalSteps[which.max(intervalSteps$steps),]

##Imputing missing values
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
summary(activity)
table(is.na(activity))

#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
activityNA <- activity
DateMeanSteps <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
activityNA$CompleteSteps <- ifelse(is.na(activityNA$steps), 
                                 round(DateMeanSteps$steps[match(activityNA$interval, DateMeanSteps$interval)],0),
                                 activityNA$steps)

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityNA <- data.frame(steps=activityNA$CompleteSteps, interval=activityNA$interval, date=activityNA$date)
summary(activityNA)

#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
stepsDay <- aggregate(steps ~ date, activity, sum)
stepsDayNA <- aggregate(steps ~ date, activityNA, sum)

require(gridExtra)
plot1 <- stepsDay %>%
  ggplot(aes(x = steps, fill = "Steps")) +
  geom_histogram(size = 2, bins=5) +
  labs(title = "Total steps per day", subtitle = "Raw Database") +
  ylab("Steps") +
  xlab("Count") +
  ylim(c(0,40)) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  guides(fill=FALSE)
plot2 <- stepsDayNA %>%
  ggplot(aes(x = steps, fill = "Steps")) +
  geom_histogram(size = 2, bins=5) +
  labs(title = "Total steps per day", subtitle = "New Database NA = Mean(interval)") +
  ylab("Steps") +
  xlab("Count") +
  ylim(c(0,40)) +
  theme_classic() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill=FALSE)
grid.arrange(plot1, plot2, ncol=2)

mean(stepsDay$steps)
mean(stepsDayNA$steps)

median(stepsDay$steps)
mean(stepsDayNA$steps)
median(stepsDayNA$steps)

##Are there differences in activity patterns between weekdays and weekends?
#1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activityNA$TypeDay <- ifelse(is.weekend(activityNA$date), "weekend", "weekday")
table(activityNA$TypeDay)
activityNA$TypeDay <- as.factor(activityNA$TypeDay)
summary(activityNA)

#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
meanintervalNA<- aggregate(steps ~ interval + TypeDay, activityNA, FUN=mean)
meanintervalNA$interval <- as.integer(meanintervalNA$interval)
head(meanintervalNA)
ggplot(meanintervalNA, aes(x=interval, y=steps)) + 
  geom_line(color="green", size=1) + 
  facet_wrap(~TypeDay, nrow=2) +
  labs(x="\nInterval", y="\nSteps") +
  theme_classic()
