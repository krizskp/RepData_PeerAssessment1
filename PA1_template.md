---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

#### 1. Load the data (i.e. read.csv())


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```

#### 2. Process/transform the data (if necessary) into a format suitable for your 


```r
activityData$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activityData$interval), format='%H:%M')
```

## What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day


```r
stepsPerDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

#### 2. Make a histogram of the total number of steps taken each day


```r
qplot(stepsPerDay, xlab='Total steps per day', ylab='Frequency', binwidth=1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanStepsPerDay <- mean(stepsPerDay)
medianStepsPerDay <- median(stepsPerDay)

sprintf("Mean: %.2f", meanStepsPerDay)
```

```
## [1] "Mean: 9354.23"
```

```r
sprintf("Median: %.2f", medianStepsPerDay)
```

```
## [1] "Median: 10395.00"
```


## What is the average daily activity pattern?

#### 1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgPerTimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)
ggplot(data=avgPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line(color='red') +
    xlab("Interval (5 min.") +
    ylab("Avg. number of steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- which.max(avgPerTimeBlock$meanSteps)
timeForMaxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgPerTimeBlock[maxSteps,'interval'])
```

**Most Steps at: 8:35**

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numMissing <- length(which(is.na(activityData$steps)))
```

**Number of missing values: 2304**

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Use the mean number of steps to fill in missing values.

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)
qplot(stepsPerDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency', binwidth=1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
meanStepsPerDayImputed <- mean(stepsPerDayImputed)
medianStepsPerDayImputed <- median(stepsPerDayImputed)

sprintf("Mean: %.2f", meanStepsPerDayImputed)
```

```
## [1] "Mean: 10766.19"
```

```r
sprintf("Median: %.2f", medianStepsPerDayImputed)
```

```
## [1] "Median: 10766.19"
```

After imputing the missing values, the mean and median values have increased. Imputing missing data resulted in increase of total daily number of steps (instead of each NAs we have average that is always >=0)

## Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```


#### 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(avgActivityDataImputed, aes(interval, steps)) + 
    geom_line(color="red") + 
    facet_grid(dateType ~ .) +
    xlab("Interval (5 mins.)") + 
    ylab("Avg. number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
