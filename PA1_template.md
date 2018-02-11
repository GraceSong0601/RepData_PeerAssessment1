---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
setwd("/Users/songyj0417/Desktop/R/05.Reproducible Research/Week 2")
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

```r
library(plyr)
```

1.Calculate the total number of steps taken per day

```r
activity_tot <- ddply(activity, .(date), summarize, total = sum(steps))
head(activity_tot)
```

```
##         date total
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

2.Make a histogram of the total number of steps taken each day

```r
hist(activity_tot$total)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day

```r
summary(activity_tot$total)[3:4]
```

```
##   Median     Mean 
## 10765.00 10766.19
```

## What is the average daily activity pattern?

1.Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activity_avg <- ddply(activity, .(interval), summarize, avg = mean(steps, na.rm = TRUE))
head(activity_avg)
```

```
##   interval       avg
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(avg ~ interval, activity_avg, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
activity_avg[activity_avg$avg == max(activity_avg$avg),]
```

```
##     interval      avg
## 104      835 206.1698
```

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

Answer: Use the mean for that 5-minute interval

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(simputation)
activity_impt <- impute_proxy(activity, steps ~ mean(steps,na.rm=TRUE) | interval)
head(activity_impt)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
activity_impt_tot <- ddply(activity_impt, .(date), summarize, total = sum(steps))
head(activity_impt_tot)
```

```
##         date    total
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(activity_impt_tot$total)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
summary(activity_impt_tot$total)[3:4]
```

```
##   Median     Mean 
## 10766.19 10766.19
```
Answer:    The estimate for Median differs from the estimate from the first part of the assignment while the Mean estimate is the same as the estimate from the first part.   When we impute missing data, the estimates of the total daily number of steps will be greater than the estimates from the original data.

## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_impt_w <- mutate(activity_impt, daytype = ifelse(weekdays(as.Date(date)) %in% c("星期六", "星期日"), "weekend", "weekday"))
head(activity_impt_w)
```

```
##       steps       date interval daytype
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
activity_impt_w_avg <- ddply(activity_impt_w, .(daytype, interval), summarize, avg = mean(steps, na.rm = TRUE))
head(activity_impt_w_avg)
```

```
##   daytype interval        avg
## 1 weekday        0 2.25115304
## 2 weekday        5 0.44528302
## 3 weekday       10 0.17316562
## 4 weekday       15 0.19790356
## 5 weekday       20 0.09895178
## 6 weekday       25 1.59035639
```

```r
library(lattice)
xyplot(avg ~ interval | daytype, data = activity_impt_w_avg, layout = c(1, 2), type = "l", 
       ylab = "Number of Steps", xlab = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
