# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip","activity.csv")
activity <- read.csv("activity.csv")

#Cleanup our file
file.remove("activity.csv")
```

```
## [1] TRUE
```

```r
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


## What is mean total number of steps taken per day?

```r
steps.per.day <- aggregate(activity$steps,list(activity$date),sum)
colnames(steps.per.day) <-c("Date","Steps")

mean.steps.daily1 <- mean(steps.per.day$Steps,na.rm=TRUE)
median.steps.daily1 <- median(steps.per.day$Steps,na.rm=TRUE)

hist(steps.per.day$Steps,
     xlab="Sum of Steps",
     main="Total Steps Taken Per Day",
     col=c("cyan"))
```

![](PA1_template_files/figure-html/sumHistogram-1.png) 


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
