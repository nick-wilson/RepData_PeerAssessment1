# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The following code was used to load and preprocess the data:


```r
zipfile<-"activity.zip"
csvfile<-"activity.csv"
if (!file.exists(csvfile)) {
    unzip(zipfile)    
    }
activity_data<-read.csv(csvfile)
```

## What is mean total number of steps taken per day?

The following code was used to plot a histogram of the total
steps per day and then calculate the mean and median of that data:

```r
library(dplyr)
# add up the total steps on each date
activity_total_steps<-activity_data%>%group_by(date)%>%
    summarize(total_steps=sum(steps,na.rm=TRUE))
# plot a histogram of total steps
hist(activity_total_steps$total_steps,breaks=10,
     xlab="Total steps per day",main="Histogram of total steps per day")
```

![](PA1_template_files/figure-html/mean_steps_per_day-1.png) 

```r
# Calculate the mean
mean_steps_per_day<-mean(activity_total_steps$total_steps)
cat("The mean total number of steps per day is",mean_steps_per_day)
```

```
## The mean total number of steps per day is 9354.23
```

```r
# Calculate the median
median_steps_per_day<-median(activity_total_steps$total_steps)
cat("The median total number of steps per day is",median_steps_per_day)
```

```
## The median total number of steps per day is 10395
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
