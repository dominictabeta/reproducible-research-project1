library(dplyr)
library(lattice)

# Loading the data into a dataframe 'activity'
activity<-read.csv("activity.csv")

activity<-mutate(activity,date=as.Date(date,"%Y-%m-%d"))
groupbydate<-group_by(activity,date)

# Calculate the total steps taken per day
stepsperday<-summarize(groupbydate,sumsteps=sum(steps))
stepsperday

# Plot the steps taken per day
with(stepsperday,plot(date,sumsteps,type="h"))

# Calculate the mean number of steps taken per day
mean(stepsperday$sumsteps,na.rm=TRUE)

# Calculate the median number of steps taken per day
median(stepsperday$sumsteps,na.rm=TRUE)

# Calculate the average daily activity pattern
groupbyinterval<-group_by(activity,interval)

avgdailyactivity<-summarize(groupbyinterval,avgstep=mean(steps))
avgdailyactivity<-summarize(groupbyinterval,avgstep=mean(steps,na.rm=TRUE))

# Line plot of the average daily activity pattern
with(avgdailyactivity,plot(interval,avgstep,type="l"))

# Find the interval with the highest average activity
avgdailyactivity[which.max(avgdailyactivity$avgstep),]

# Calculate and report the total number of missing values in the dataset

totalna<-sum(is.na(activity$steps))

# The strategy I choose to impute the missing values is to replace the NAs with the mean for that 5-minute interval

imputedactivity<-activity
for (i in 1:nrow(imputedactivity)) {
        if (is.na(imputedactivity[i,"steps"])) {
                imputedactivity[i,"steps"]<-avgdailyactivity[avgdailyactivity$interval==imputedactivity[i,"interval"],"avgstep"]
                
        } 
}

groupbydateimputed<-group_by(imputedactivity,date)

# Calculate the total steps taken per day
imputedstepsperday<-summarize(groupbydateimputed,sumsteps=sum(steps))
imputedstepsperday

# Plot the steps taken per day
with(imputedstepsperday,plot(date,sumsteps,type="h"))

# Calculate the mean number of steps taken per day
mean(imputedstepsperday$sumsteps,na.rm=TRUE)

# Calculate the median number of steps taken per day
median(imputedstepsperday$sumsteps,na.rm=TRUE)

# The effect of imputing the missing values has resulted in the mean steps per day
# remaining the same and the median steps per day only increasing slightly from 10765
# to 10766.19
# The mean and median steps per day for the dataset set with imputed values are both the same
# 

# Applying the weekday/weekend labels to the dataset

daylabeled<-mutate(imputedactivity,daylabel=weekdays(date))
for (i in 1:nrow(daylabeled)) {
        if (daylabeled[i,"daylabel"] %in% c("Saturday","Sunday")) {
                daylabeled[i,"daylabel"]<-"weekend"
        } else {
                daylabeled[i,"daylabel"]<-"weekday"
        }
}

groupbydaylabel<-group_by(daylabeled,daylabel,interval)
weekdaycompare<-summarize(groupbydaylabel,avgstep=mean(steps))

# Plotting the weekend vs weekday comparison of avg steps for each interval
xyplot(avgstep~interval | factor(daylabel),data=weekdaycompare,groups=daylabel,type="l",xlab = "Interval",ylab="Number of steps",layout=c(1,2),col=c("blue","blue"),scales=list())

