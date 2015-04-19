# Personal Activity Monitoring
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Load and preprocess the data

```r
setwd("~/Documents/coursera/DataScienceSeries/ReproducibleResearch/RepData_PeerAssessment1")
activity <- read.csv(unz("activity.zip", "activity.csv"))
```
### Calculate the total number of steps taken per day


```r
activity <- activity[!(is.na(activity$steps)),]
df <- aggregate(activity$step, by=list(activity$date), FUN=sum)
names(df) <- c("date", "totalSteps")
```

### Make a histogram of the total number of steps taken each day

```r
hist(df$totalSteps, breaks=20, xlab="Total Number of Steps", main="Total Number of Steps Taken Each Day", col="red")
```

![](PA1_template_files/figure-html/hist-1.png) 

### Calculate and report the mean and median

```r
meanSteps <- mean(df$totalSteps)
medianSteps <- median(df$totalSteps)
```

The mean of the total number of steps taken per day is 10766.1886792453
The median of the total number of steps taken per day is 10765

### Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
stepsByIntervalAndDate <- aggregate(activity$steps, by=list(activity$interval, activity$date), FUN=mean)
names(stepsByIntervalAndDate) <- c("interval", "date", "steps")
datetime <- as.POSIXct(stepsByIntervalAndDate$interval, origin=stepsByIntervalAndDate$date)
plot(datetime, stepsByIntervalAndDate$steps, type="l", xlab="5-minute Intervals", ylab="Average Number Of Steps", col="red", main="average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

### Which 5-minute interval, averaged across all the days in the dataset, contains the maximum number of steps?

```r
maximumNumberOfSteps <- stepsByIntervalAndDate[which(grepl(max(stepsByIntervalAndDate$steps), stepsByIntervalAndDate$steps)),]
maxDate <- as.POSIXct(maximumNumberOfSteps$interval, origin= maximumNumberOfSteps$date)
```
The 5-minute interval containing the maximum number of steps is 2012-11-27 00:10:15

### Calculate and report the total number of missing values in the dataset

```r
activityWithNAs <- read.csv("activity.csv")
numberOfMissingValues <- nrow(activityWithNAs[is.na(activityWithNAs$steps),])
```
The number of missing values is 2304

### New dataset equivalent to the original but with the missing data filled in

```r
activityWithImputedValues <- replace(activityWithNAs, is.na(activityWithNAs), mean(activityWithNAs$steps, na.rm=TRUE))
```

### Total number of steps taken each day with the missing data filled in

```r
numberOfStepsEachDay<- aggregate(activityWithImputedValues$step, by=list(activityWithImputedValues$date), FUN=sum)
names(numberOfStepsEachDay) <- c("date", "totalSteps")
hist(numberOfStepsEachDay$totalSteps, breaks=20, xlab="Total Number of Steps", main="Total Number of Steps Taken Each Day", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
### The mean and median total number of steps taken per day with missing values filled in

```r
meanStepsWithImputedValues <- mean(numberOfStepsEachDay$totalSteps)
medianStepsWithImputedValues <- median(numberOfStepsEachDay$totalSteps)
```

### Difference in values from the estimates from the first part of the assignment
The mean of the total number of steps taken per day is 10766.1886792453

The mean total number of steps taken per day with missing values filled in is 10766.1886792453

The median of the total number of steps taken per day is 10765

The median total number of steps taken per day with missing values filled in is 10766.1886792453

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
As can be seen from the figures above, only the median value is affected by filling in the missing data.

### New factor variable with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
days <- weekdays(as.Date(activityWithImputedValues$date)) %in% c('Saturday','Sunday')
activityWithImputedValues$day <- factor(days, labels = c("weekday", "weekend"))
```
### Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
meanStepsWithImputedValues <- aggregate(data=activityWithImputedValues, steps ~ interval + day, FUN="mean")
datetime <- as.POSIXct(stepsByIntervalAndDate$interval, origin=stepsByIntervalAndDate$date)
library(lattice)
xyplot(data=meanStepsWithImputedValues, steps ~ interval | day, type="l", grid=TRUE, layout=c(1,2), ylab="Average Number Of Steps", xlab="5-minute intervals", main="Activity Patterns Comparing Weekdays and Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

