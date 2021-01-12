---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
# Load data from source file "activity.csv" as a dataframe.
activitydf <- read.csv("activity.csv")

# Set date column to Date class type.
activitydf[,2] <- as.Date(activitydf[,2])
```


## What is mean total number of steps taken per day?

```r
# Sum steps per date and remove NAs.
steps <- aggregate(steps ~ date, activitydf, sum)

# Display a histogram of steps per day.
hist(steps[,2], main="Total Steps per Day")
```

![](PA1_report_files/figure-html/activitydf-1.png)<!-- -->

```r
# Compute the mean and median steps per day.
stepsmean <- mean(steps[,2])
stepsmedian <- median(steps[,2])
```


The mean steps per day is 1.0766189\times 10^{4}. The median steps per day is 10765.


## What is the average daily activity pattern?

```r
# Compute the average number of steps per interval across all days.
intsteps <- aggregate(steps ~ interval, activitydf, mean)

# Plot the average number of steps per interval as computed in the last step.
plot(intsteps$interval, intsteps$steps, type="l", col="blue", ylab="Steps", xlab="Interval", main="Average Steps per Interval (across all days)")
```

![](PA1_report_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Find and report the interval that corresponds to the maximum number of average steps per day as calculated across all days.
maxavgsteps <- max(intsteps[,2])
index <- intsteps[intsteps$steps == max(intsteps$steps),]
```

The 5-minute interval 835, 206.1698113 corresponds to the maximum number of average steps per day 206.1698113, as indicated by the highest peak in the graph of average steps per interval.

## Imputing missing values

```r
# Compute the total number of NAs within the dataset.
numNA <- sum(is.na(activitydf[,1]))

# Create a new dataset and set NAs to 0 per the strategy described in the text.
activitydf2 <- activitydf
NAs <- is.na(activitydf2)
activitydf2[NAs] <- 0
steps2 <- aggregate(steps ~ date, activitydf2, sum)
```

The total number of NAs within the steps column of the activity dataset is 2304. Each NA will be replaced by setting its value to 0 to reduce bias resulting from missing values. 


```r
# Plot the histogram of total steps per day using the dataset with NAs filled in.
hist(steps2[,2], main="Total Steps per Day (NAs set to 0)")
```

![](PA1_report_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Calculate and report the mean and median steps per day.
stepsmean2 <- mean(steps2[,2])
stepsmedian2 <- median(steps2[,2])
```

The mean steps per day as computed based on the dataset with NAs set to 0 is 9354.2295082 and the median steps per day is 1.0395\times 10^{4}. The mean and median values calculated on the revised dataset are *less than* from the original mean and median values, respectively, calculated above because the missing data was replaced with 0. Additionally, the histogram appropriately redistributes to include more steps in the bin of [0,5000].

## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a list that assigns the name of the day to each date within the filled in dataset. Bind activitydays with activitydf2 to form new data frame.
activitydf3 <- cbind(activitydf2, weekdays(activitydf2[,2]))
names(activitydf3) <- c("steps", "date", "interval", "day")

# Splice actividydf3 into two datasets: one with weekdays and one with weekends.
weekdayz <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekendz <- c("Saturday", "Sunday")
wkdaysactivity <- activitydf3[activitydf3$day == weekdayz,]
```

```
## Warning in activitydf3$day == weekdayz: longer object length is not a multiple
## of shorter object length
```

```r
wkendsactivity <- activitydf3[activitydf3$day == weekendz,]

# Produce two timeseries for the average number of steps taken across weekdays and another averaged across weekends. 
wkdaysteps <- aggregate(steps ~ interval, wkdaysactivity, mean)
wkendsteps <- aggregate(steps ~ interval, wkendsactivity, mean)

# Plot the two time series in two windows of the same plot.
par(mfrow=c(2,1), mar = numeric(4), oma=c(4,4,3,0.5), mgp=c(2,0.6,0))
#layout(matrix(c(1,2),2, byrow=TRUE))
plot(wkdaysteps$interval, wkdaysteps$steps, type="l", col="blue", axes=FALSE)#, main="Average Steps per Interval (top: across weekdays; bottom: across weekends")
axis(2L)
box()
plot(wkendsteps$interval, wkendsteps$steps, type="l", col="blue")
mtext("Interval", side=1, outer=TRUE, line=2.2)
mtext("Steps", side=2, outer=TRUE, line=2.2)
mtext("Average Steps per Interval (top: across weekdays; bottom: across weekends)", outer=TRUE)
```

![](PA1_report_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# Compute the mean and median of each time series.
wkdaymean <- mean(wkdaysteps$steps)
wkdaymedian <- median(wkdaysteps$steps)
wkendmean <- mean(wkendsteps$steps)
wkendmedian <- median(wkendsteps$steps)
totalwkday <- sum(wkdaysteps$steps)
totalwkend <- sum(wkendsteps$steps)
```

The average steps taken on weekdays is shown on the top of the last graph and the average steps taken on weekends is show on the bottom. The mean and the median of average steps taken on weekdays are 30.1342593 and 10.5, respectively. The mean and median of average steps taken on weekends are 37.5842014 and 16.625, respectively. The average steps taken on the weekends is greater than the average steps taken on weekdays.

