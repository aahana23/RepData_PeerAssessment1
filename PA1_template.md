# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(lattice)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.3
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

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}

data <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?

```r
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = "Total Steps Each Day", col="blue", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
paste("mean and median of the total number of steps taken per day",rmean,rmedian,sep=" ")
```

```
## [1] "mean and median of the total number of steps taken per day 10766.1886792453 10765"
```


## What is the average daily activity pattern?

```r
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#type is l for line when u do aggregate y ~ x then in dataset x first y second
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
paste(" 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps   ",max_interval)
```

```
## [1] " 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps    835"
```


## Imputing missing values


```r
data_full <- data
nas <- is.na(data_full$steps)
paste("the number of rows with NAs  are   ",sum(nas),sep=" ")
```

```
## [1] "the number of rows with NAs  are    2304"
```

```r
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]


steps_by_day2 <- aggregate(steps ~ date, data_full, sum)
hist(steps_by_day2$steps, main = "Total Steps Each Day", col="blue", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
rmeann <- mean(steps_by_day2$steps)
rmediann <- median(steps_by_day2$steps)
paste("mean and median of the total number of steps taken per day",rmeann,rmediann,sep=" ")
```

```
## [1] "mean and median of the total number of steps taken per day 10766.1886792453 10766.1886792453"
```

```r
paste("Difference between total no of steps has come by the impact of imputing missing data is  " ,   sum(steps_by_day2$steps)-sum(steps_by_day$steps))
```

```
## [1] "Difference between total no of steps has come by the impact of imputing missing data is   86129.5094339623"
```

## Are there differences in activity patterns between weekdays and weekends?

```r
data_full <- mutate(data_full, weektype =ifelse(weekdays(as.Date(data_full$date)) == "Saturday"| weekdays(as.Date(data_full$date)) == "Sunday" ,"weekend", "weekday"))
data_full$date <- as.Date(data_full$date)
steps_by_interval_i <- aggregate(steps ~ interval + weektype, data_full, mean)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$weektype, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
