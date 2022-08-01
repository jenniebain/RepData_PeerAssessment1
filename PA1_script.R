library(dplyr)
library(tidyr)
library(ggplot2)

## The data will be in the forked repository, if it's not unzipped yet, unzip it

if(!file.exists("activity.csv")){unzip("./activity.zip")}

## Read the data into a dataframe
activities <- read.csv("activity.csv")

## Convert the interval to a time and make a separate date/time field
activities$time <- sprintf("%d:%02d",activities$interval %/% 100, activities$interval %% 100)
activities$datetime <- as.POSIXct(paste(activities$date,activities$time,sep=" "), "%Y-%m-%d %H:%M")

### What is the mean total number of steps taken per day?

## Calculate the mean number of steps taken per day
steps_day <- tapply(activities$steps,activities$date,sum,na.rm=TRUE)

## Plot a histogram of the total number of steps taken each day
hist(steps_day,main="Total Steps per Day from 10/1/2012 to 11/30/2012",xlab="Number of Steps per Day",col="light blue")

## Calculate the mean and median of the total number of steps taken per day
mean(steps_day)
median(steps_day)

### What is the average daily activity pattern?

steps_int <- aggregate(activities$steps,list(activities$interval),mean,na.rm=TRUE)
plot(steps_int$Group.1,steps_int$x,type="l",xaxp = c(0,2400,24),main="Mean Steps per Day by Interval",xlab="5 Minute Interval",ylab="Mean Steps per Day")
steps_int[which.max(steps_int$x),1]


### Imputing missing values
## Calculate the number of missing values in the dataset
nrow(subset(activities,is.na(activities$steps)))

## Replace NA's with the mean for that time interval over all days

act_new <- activities

act_new$steps <- as.numeric(act_new$steps)

act_new %>%
  group_by(interval) %>%
  mutate_at(vars(steps), ~replace_na(.,mean(.,na.rm=TRUE))) -> act_new

steps_day_new <- tapply(act_new$steps,act_new$date,sum,na.rm=TRUE)
hist(steps_day_new,main="Total Steps per Day from 10/1/2012 to 11/30/2012\nNAs replaced with mean per interval",xlab="Number of Steps per Day",col="light blue")

mean(steps_day_new)
median(steps_day_new)

### Are there differences in activity patterns between weekdays and weekends?
act_new$daytype <- weekdays(act_new$datetime)
act_new$daytype <- replace(act_new$daytype,act_new$daytype == 'Sunday' | act_new$daytype == 'Saturday',"Weekend")
act_new$daytype <- replace(act_new$daytype,act_new$daytype != 'Weekend',"Weekday")

act_new %>%
  group_by(interval,daytype) %>%
  summarize(mean=mean(steps)) %>%
  ggplot(aes(x=interval,y=mean,color=daytype)) + 
  geom_line(size=1,show.legend = FALSE) +
  facet_wrap(~daytype,ncol=1) +
  ggtitle("Mean Steps per Day by Interval") +
  xlab("5 Minute Interval") +
  ylab("Mean Steps per Day") 
