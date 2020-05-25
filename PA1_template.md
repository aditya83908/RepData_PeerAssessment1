---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity<- read.csv('activity.csv',sep = ",",header = TRUE, na.strings ="NA",
                   colClasses = c('integer','Date','factor'))
library(ggplot2)
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     diamonds
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","10","100",..: 1 226 2 73 136 195 198 209 212 223 ...
```

```r
activity<- na.omit(activity)
```


## What is mean total number of steps taken per day?

```r
total.steps <- tapply(activity$steps, activity$date, FUN=sum)
plot1<- ggplot(activity,aes(date, steps))+ geom_histogram(stat="identity", binwidth = 0.5)+ labs(title = "Histogram of Total Number of Steps Taken Each Day",x = "Date", y = "Total Number of Steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
print(plot1)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
Mean And Median of steps per day

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
averages <- aggregate(activity$steps, list(interval = as.numeric(as.character(activity$interval))), FUN = "mean")
names(averages)[2] <- "Avg.Steps"

plot2 <- ggplot(averages, aes(interval, Avg.Steps)) + geom_line(color = "green", size = 0.7) + labs(title = "Time Series Plot of the 5-minute Intervals", x = "5-minute intervals", y = "Average Number of Steps Taken")
print(plot2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all days, contains the most steps?

```r
averages[averages$Avg.Steps == max(averages$Avg.Steps),]
```

```
##     interval Avg.Steps
## 104      835  206.1698
```

Imputing missing values by using mean for that 5-minute interval.


## Imputing missing values

```r
impData <- activity 
for (i in 1:nrow(impData)) {
    if (is.na(impData$steps[i])) {
        impData$steps[i] <- averages[which(impData$interval[i] == averages$interval), ]$Avg.Steps
    }
}
sum(!complete.cases(impData))
```

```
## [1] 0
```

```r
plot3 <- ggplot(impData, aes(date, steps)) + geom_histogram(stat = "identity",binwidth = .5) +
        labs(title = "Histogram of Total Number of Steps Taken Each Day (Imputed Data)",x = "Date", y = "Total Number of Steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
print(plot3)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
total.steps.impute <- tapply(impData$steps, impData$date, FUN = sum)
mean(total.steps.impute)
```

```
## [1] 10766.19
```

```r
median(total.steps.impute)
```

```
## [1] 10765
```


## Are there differences in activity patterns between weekdays and weekends?

```r
impData$weekdays <- factor(format(impData$date, "%A"))
levels(impData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"   "Wednesday"
```

```r
levels(impData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(impData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(impData$weekdays)
```

```
## 
## weekday weekend 
##   11232    4032
```

```r
new.averages <- aggregate(impData$steps, 
                      list(interval = as.numeric(as.character(impData$interval)), 
                           weekdays = impData$weekdays),
                      FUN = "mean")
names(new.averages)[3] <- "meanOfSteps"
library(lattice)
plot4 <- xyplot(new.averages$meanOfSteps ~ new.averages$interval | new.averages$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
print(plot4)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
