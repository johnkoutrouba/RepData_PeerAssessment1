# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
This assumes all files from the repository are synched and present in the
working directory.  

First, unzip the data files.

```r
unzip("activity.zip")
```

Next, read in the activity data file and view some summary information.

```r
activity <- read.csv("activity.csv", header = TRUE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

The date field was read in as a factor.  Convert it to a date.

```r
library(lubridate)
activity$date <- ymd(activity$date)
```

The data set is now ready for analysis.  


## What is mean total number of steps taken per day?
The ddply function from the plyr package can sum the steps per day so that the 
mean and median are easy to calculate.  

First, a quick visualization of the data in the form of a histogram of total
steps by day.


```r
library(plyr)
library(ggplot2)
```

```r
qplot(ddply(activity, .(date), summarise, totsteps = sum(steps))[,2], 
      na.rm = TRUE, geom = "histogram", xlab = "Number of Steps Per Day",
      ylab = "Number of Days", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
mean(ddply(activity, .(date), summarise, totsteps = sum(steps))[,2], na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(ddply(activity, .(date), summarise, totsteps = sum(steps))[,2], na.rm = TRUE)
```

```
## [1] 10765
```


The mean steps per day is 1.0766189\times 10^{4} and the median steps per day is 10765, or a little under 11,000 for both values.  This corresponds to the tallest bar in the chart.  

## What is the average daily activity pattern?
A plot of the data showing the steps per interval will illustrate the daily 
activity pattern.


```r
library(ggplot2)
steps <- ddply(activity, .(interval), summarise, totsteps = mean(steps, na.rm = TRUE))
qplot(interval, totsteps, data = steps, geom = "line") +
        theme(legend.position = "none") +
        xlab("Time of Day") +
        ylab("Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The plot shows an average day starting between 05:30 and 06:00 with a flurry 
of activity that tapers off around 10:00.  The highest average number of steps 
occurs between 08:35 and 08:40. The day maintains a relative steady-state 
of activity, slightly increasing through the afternoon.  The activity 
level declines dramatically into the evening, and by 21:00 is very low for the 
rest of the night.

## Imputing missing values
The data set is missing 2304 values. The missing values will be substituted with 
imputed values using a median function for each value's interval in the sample.


```r
sum(is.na(activity$steps)) # Calculate number of NA values
```

```
## [1] 2304
```

```r
mediansteps <- ddply(activity, .(interval), summarise, mediansteps = median(steps, na.rm = TRUE))
for (i in 1:dim(activity)[1]) {
        if (is.na(activity[i, "steps"])) {
                activity[i, "steps"] <- mediansteps[mediansteps$interval == activity[i, "interval"], "mediansteps"]
        }
}
```

With the imputed values, the mean and median are now 9503.8688525 and 10395.  


```r
qplot(ddply(activity, .(date), summarise, totsteps = sum(steps))[,2], 
      na.rm = TRUE, geom = "histogram", xlab = "Number of Steps Per Day",
      ylab = "Number of Days", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(ddply(activity, .(date), summarise, totsteps = sum(steps))[,2], na.rm = TRUE)
```

```
## [1] 9503.869
```

```r
median(ddply(activity, .(date), summarise, totsteps = sum(steps))[,2], na.rm = TRUE)
```

```
## [1] 10395
```

The new values are lower than the values calculated excluding the imputed
data.

## Are there differences in activity patterns between weekdays and weekends?
To understand the difference between weekdays and weekends, a new variable to 
indicate weekday or weekend is added to the data set.  Then a plot comparing the
two can be constructed. 


```r
activity[weekdays(activity$date) 
         %in% c("Monday", "Tuesday", "Wednesday",
                "Thursday", "Friday"), "day_end"] <- "weekday"
activity[weekdays(activity$date) 
         %in% c("Saturday", "Sunday"), "day_end"] <- "weekend"
steps <- ddply(activity, .(interval, day_end), summarise, totsteps = mean(steps, na.rm = TRUE))
qplot(interval, totsteps, data = steps, geom = "line", color = day_end) +
        xlab("Time of Day") +
        ylab("Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

When the two series are plotted together, it is easy to see that weekend days
are more active, and start and end later than week days.
