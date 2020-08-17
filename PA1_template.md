---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Reading the dataset:


```r
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

## What is mean total number of steps taken per day?

#### 1- Calculate the total number of steps taken per day


```r
steps <- activity[ ,"steps"]
date <- activity[ ,"date"]

steps_per_day <- tapply(steps, date, sum, na.rm=TRUE)

steps_per_day <- data.frame(date = as.Date(unique(date), "%Y-%m-%d"),
                            steps = steps_per_day)
                         
head(steps_per_day)
```

```
##                  date steps
## 2012-10-01 2012-10-01     0
## 2012-10-02 2012-10-02   126
## 2012-10-03 2012-10-03 11352
## 2012-10-04 2012-10-04 12116
## 2012-10-05 2012-10-05 13294
## 2012-10-06 2012-10-06 15420
```

#### 2- Histogram of the total number of steps taken each day


```r
hist(steps_per_day$steps, col="blue", main = "Histogram steps per day",
     xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### 3- Calculate and report the mean and median of the total number of steps taken per day


```r
summary(steps_per_day)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :    0  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9354  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

Answer: the mean of steps is 9354 and the median 10395.

## What is the average daily activity pattern?

#### 1 - Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(dplyr)
```


```r
steps_per_interval <- activity %>%
                group_by(interval) %>%
                summarize(steps = mean(steps, na.rm=TRUE)) %>%
        print
```

```
## # A tibble: 288 x 2
##    interval  steps
##       <int>  <dbl>
##  1        0 1.72  
##  2        5 0.340 
##  3       10 0.132 
##  4       15 0.151 
##  5       20 0.0755
##  6       25 2.09  
##  7       30 0.528 
##  8       35 0.868 
##  9       40 0     
## 10       45 1.47  
## # ... with 278 more rows
```

```r
with(steps_per_interval, plot(interval, steps, type = "l", col = "blue",
                              main = "Average of steps per 5-min interval",
                              xlab="5-min interval",
                              ylab="average number of steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### 2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
df_in_order <- arrange(steps_per_interval, desc(steps))
head(df_in_order, 1)
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```

Answer: The interval 835 contains, on average, the maximum number of steps (206 steps).

## Imputing missing values

### 1 - Calculate and report the total number of missing values in the dataset


```r
sum(is.na(activity))
```

```
## [1] 2304
```

### 2 - Devise a strategy for filling in all of the missing values in the dataset. 

First I merged the original table(activity) with the table steps_per_interval, wich contained the mean of steps per interval.


```r
join <- merge(activity, steps_per_interval, by="interval")
```

Then I created a new column, if the steps are "NA" the result is the mean of steps for that interval.


```r
join$steps_no_NA <- ifelse(is.na(join$steps.x), join$steps.y, join$steps.x)
```


### 3 - Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
final_table <- join %>%
        select(date, interval, steps_no_NA) %>%
        rename(steps=steps_no_NA) %>%
        arrange(date)
head(final_table)
```

```
##         date interval     steps
## 1 2012-10-01        0 1.7169811
## 2 2012-10-01        5 0.3396226
## 3 2012-10-01       10 0.1320755
## 4 2012-10-01       15 0.1509434
## 5 2012-10-01       20 0.0754717
## 6 2012-10-01       25 2.0943396
```

### 4- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
final_steps_per_day <- final_table %>%
        group_by(date) %>%
        summarize(steps=sum(steps)) %>%
        print
```

```
## # A tibble: 61 x 2
##    date        steps
##    <fct>       <dbl>
##  1 2012-10-01 10766.
##  2 2012-10-02   126 
##  3 2012-10-03 11352 
##  4 2012-10-04 12116 
##  5 2012-10-05 13294 
##  6 2012-10-06 15420 
##  7 2012-10-07 11015 
##  8 2012-10-08 10766.
##  9 2012-10-09 12811 
## 10 2012-10-10  9900 
## # ... with 51 more rows
```

```r
hist(final_steps_per_day$steps, col = "blue", main = "Histogram Steps Per Day",
     xlab="Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Mean of steps: 10,766  
Median of steps: 10,766

## Are there differences in activity patterns between weekdays and weekends?

### 1- Create a new factor variable in the dataset with two levels – “weekday” and “weekend”

First I created a new column with the name of the day


```r
final_table$date <- as.Date(final_table$date, "%Y-%m-%d")

final_table$weekday <- weekdays(final_table$date)
```

Then I created a factor column, with two levels "weekday" and "weekend". 


```r
final_table$day <- as.factor(ifelse(final_table$weekday %in% c("Saturday","Sunday"), 
                          "weekend", "weekday"))

table_with_days <- final_table %>%
        select(date, interval, steps, day)

head(table_with_days)
```

```
##         date interval     steps     day
## 1 2012-10-01        0 1.7169811 weekday
## 2 2012-10-01        5 0.3396226 weekday
## 3 2012-10-01       10 0.1320755 weekday
## 4 2012-10-01       15 0.1509434 weekday
## 5 2012-10-01       20 0.0754717 weekday
## 6 2012-10-01       25 2.0943396 weekday
```

### 2 - Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
average_steps <- table_with_days %>%
        group_by(interval, day) %>%
        summarize(steps=mean(steps))
library(ggplot2)
qplot(interval, steps, data=average_steps, geom="path", facets=day~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
