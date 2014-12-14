---
title: 'Reproducible Research: Peer Assessment 1'
author: "Dwayne Pindling"
date: "Sunday, December 14th, 2014"
output: html_document
---

```r
## The Code below is designed to Load and preprocess data
## The 0 values and NA values will be removed from the dataset.
url<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "activity.zip", mode="wb") 
unzip("activity.zip")
data <- read.csv("activity.csv")
data$interval <- sprintf("%04d", data$interval)

## What is mean total number of steps taken per day?
## A Histogram is also created with the total number of steps taken each day.
## The mean and median of dataset is also calculated
data$steps[data$steps==0] <- NA
omitdata <- na.omit(data)
omitdata$steps <-as.numeric(omitdata$steps)
omitdata$date <- as.Date(omitdata$date, format = "%Y-%m-%d")
sumdata <- aggregate(omitdata$steps, by=list(omitdata$date), FUN=sum)
hist(sumdata$x, main="Total number of steps per day",xlab="Steps per day", ylab="Frequency")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r
mean(sumdata$x)
```

```
## [1] 10766
```

```r
median(sumdata$x)
```

```
## [1] 10765
```


```r
## What is the average daily activity pattern??
## A time series plot is created of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
## The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is displayed?
omitdata$interval <- format(omitdata$interval, format="%H%M")
averagesteps <- aggregate(omitdata$steps, list(interval = omitdata$interval),mean)
plot(averagesteps$interval,averagesteps$x,type="l", main="Average number of steps per interval", xlab="5 min intervals(hhmm)", ylab="Average number of steps" )
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
max(averagesteps$x)
```

```
## [1] 352.5
```


```r
## Imputing missing values
data <- read.csv("activity.csv")
## Total numbers of missing values in the dataset
sum(is.na(data$steps))
data$interval <- sprintf("%04d", data$interval)
## The strategy used to fill in missing data is to update each interval with the
## mean for that 5 minute interval.
data$steps[is.na(data$steps)] <- averagesteps[match(data$interval, averagesteps$interval),2]
```

```
## Warning: number of items to replace is not a multiple of replacement
## length
```

```r
data$steps[data$steps==0] <- NA
na.omit(data$steps)
## Updated histogram containing substituted values
hist(data$steps,main="Total number of steps per day",xlab="Steps per day", ylab="Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


```r
## Are there differences in activity patterns between weekdays and weekends?
data <- read.csv("activity.csv")
data$steps[is.na(data$steps)] <- 0
averagesteps <- aggregate(data$steps, list(interval = data$interval),mean)
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
week <-weekdays(data$date)
week <-as.factor(ifelse(week %in% c("Saturday","Sunday"),"Weekend","Weekday"))
updatedata <- cbind(data,week)
weekplotdata <- subset(updatedata, week=="Weekday")
weekendplotdata <- subset(updatedata, week=="Weekend")
averageweekplotdata <- aggregate(weekplotdata$steps, list(interval= weekplotdata$interval),mean)
averageweekendplotdata <- aggregate(weekendplotdata$steps, list(interval = weekendplotdata$interval),mean)
par(mfrow = c(2,1), mar = c(4,4,2,1), oma = c(0,0,2,0))
WeekdayPlot <- plot(averageweekplotdata$interval,averageweekplotdata$x,type="l", main="Average Number of steps per week", xlab="Frequency", ylab="Interval")
WeekendPlot <- plot(averageweekendplotdata$interval, averageweekendplotdata$x, type="l",main= "Average Number of steps per weekend", xlab="Frequency",ylab="Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
