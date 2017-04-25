# Reproducible Research Project 1

##Loading and preprocessing the data

Install Packages

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

Load the data

```r
activity <- read.csv("activity.csv")
```
Convert date column to datetime

```r
activity$date <- as.Date(activity$date)
```

##What is mean total number of steps taken per day?

Plot a histogram of the total number of steps taken each day

```r
ggplot(na.omit(activity), aes(x=date, y=steps)) + geom_bar(stat="identity") + 
  labs(main="Total Steps Per Day", x="Date", y="Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate the total steps per day

```r
totalSteps <- activity %>% group_by(date) %>% filter(!is.na(steps)) %>%
                summarize(totalStepsPerDay = sum(steps, na.rm=TRUE))
totalSteps
```

```
## # A tibble: 53 × 2
##          date totalStepsPerDay
##        <date>            <int>
## 1  2012-10-02              126
## 2  2012-10-03            11352
## 3  2012-10-04            12116
## 4  2012-10-05            13294
## 5  2012-10-06            15420
## 6  2012-10-07            11015
## 7  2012-10-09            12811
## 8  2012-10-10             9900
## 9  2012-10-11            10304
## 10 2012-10-12            17382
## # ... with 43 more rows
```

Calculate the mean steps per day

```r
meanSteps<- mean(totalSteps$totalStepsPerDay, na.rm=TRUE)
meanSteps
```

```
## [1] 10766.19
```

Calculate the median steps per day

```r
medianSteps<- median(totalSteps$totalStepsPerDay, na.rm=TRUE)
medianSteps
```

```
## [1] 10765
```

##What is the average daily activity pattern?

Average total steps taken over 5 minute intervals

```r
totalIntSteps <- activity %>% group_by(interval) %>% filter(!is.na(steps)) %>%
                summarize(totalStepsPerInt = mean(steps, na.rm=TRUE))
totalIntSteps
```

```
## # A tibble: 288 × 2
##    interval totalStepsPerInt
##       <int>            <dbl>
## 1         0        1.7169811
## 2         5        0.3396226
## 3        10        0.1320755
## 4        15        0.1509434
## 5        20        0.0754717
## 6        25        2.0943396
## 7        30        0.5283019
## 8        35        0.8679245
## 9        40        0.0000000
## 10       45        1.4716981
## # ... with 278 more rows
```

Plot average total steps taken over 5-minute intervals

```r
ggplot(na.omit(totalIntSteps), aes(x=interval, y=totalStepsPerInt)) + geom_line() + 
  labs(main="Average Total Steps Over 5 Minute Intervals", x="Intervals", y="Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
totalIntSteps[which.max(totalIntSteps$totalStepsPerInt),]
```

```
## # A tibble: 1 × 2
##   interval totalStepsPerInt
##      <int>            <dbl>
## 1      835         206.1698
```

##Imputing missing values

Total number of missing values in the dataset

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Fill in the missing values in the dataset with the mean for that 5-minute interval

```r
activity2 <- activity

nas <- is.na(activity2$steps)
intervalMean <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify=TRUE)
activity2$steps[nas] <- intervalMean[as.character(activity2$interval[nas])]
```

Double-check there are no missing values in the dataset

```r
sum(is.na(activity2$steps))
```

```
## [1] 0
```

Calculate number of steps taken per day

```r
stepsFilledNAs <- activity2 %>% group_by(date) %>% filter(!is.na(steps)) %>% summarize(steps = sum(steps)) %>% print
```

```
## # A tibble: 61 × 2
##          date    steps
##        <date>    <dbl>
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## # ... with 51 more rows
```

Plot a histogram of the total number of steps taken each day with NAs filled

```r
ggplot(na.omit(stepsFilledNAs), aes(x=date, y=steps)) + geom_bar(stat="identity") + 
  labs(main="Total Steps Per Day", x="Date", y="Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

Calculate the mean steps per day

```r
meanSteps2 <- mean(stepsFilledNAs$steps, na.rm=TRUE)
meanSteps2
```

```
## [1] 10766.19
```

Calculate the median steps per day

```r
medianSteps2 <- median(stepsFilledNAs$steps, na.rm=TRUE)
medianSteps2
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activity2 <- mutate(activity2,weekType = ifelse(weekdays(activity2$date) == "Saturday" | weekdays(activity2$date) == "Sunday", "weekend", "weekday"))

activity2$weekType <- as.factor(activity2$weekType)
head(activity2)
```

```
##       steps       date interval weekType
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
intervalWeekType <- activity2 %>% group_by(interval, weekType) %>% summarize(steps = mean(steps))

weekTypePlot <- ggplot(intervalWeekType, aes(x=interval, y=steps, color = weekType)) + geom_line() + 
        facet_wrap(~weekType, ncol = 1, nrow=2)
print(weekTypePlot)
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
