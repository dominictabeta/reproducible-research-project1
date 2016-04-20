library(dplyr)

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






