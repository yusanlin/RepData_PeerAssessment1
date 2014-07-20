# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
```
data = read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
### 1. Make a histogram of the total number of steps taken each day
```
hist(data$steps)
```
<img class=center src=fig/hist_steps.png>

### 2. Calculate and report the mean and median total number of steps taken per day
```
mean_steps = mean(data$steps, na.rm = TRUE)
median_steps = median(data$steps, na.rm = TRUE)
```

## What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```
library(doBy)
mean2 <- function(x){
  mean(x, na.rm = TRUE)
}
summarized_data = summaryBy(steps ~ interval, data = data, FUN = c(mean2))
plot(x=summarized_data$interval, y=summarized_data$steps.mean2,type="l")
```
<img class=center src=fig/time_series_1.png>

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```
summarized_data$interval[which.max(summarized_data$steps.mean2)]
```

## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```
sum(!complete.cases(data))
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- Create a new dataset that is equal to the original dataset but with the missing data filled in.
```
data_filled <- data
for( i in 1:nrow(data)){
  if( is.na(data$steps[i]) )
    data_filled$steps[i] <- median_steps
}
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```
hist(data_filled$steps)
mean_steps_filled = mean(data_filled$steps, na.rm = TRUE)
median_steps_filled = median(data_filled$steps, na.rm = TRUE)
```
<img class=center src=fig/hist_steps_nona.png>

## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```
isWeekday <- function(date){
  if (weekdays(as.Date(date)) == "Saturday" || weekdays(as.Date(date)) == "Saturday")
    x <- "weekend"
  else
    x <- "weekday"
  return(x)
}
```

```
weekdayWeekend <- c()
for (i in 1:nrow(data)){
  weekdayWeekend <- c(weekdayWeekend, isWeekday(as.Date(data$date[i])))
}
data_filled <- cbind(data_filled, weekdayWeekend)
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```
summarized_data_filled = summaryBy(steps ~ interval + weekdayWeekend, data = data_filled, FUN = c(mean2))
summarized_data_filled_weekday <- subset(summarized_data_filled, weekdayWeekend == "weekday")
summarized_data_filled_weekend <- subset(summarized_data_filled, weekdayWeekend == "weekend")
```

```
par(mfrow=c(2,1))
plot(summarized_data_filled_weekday$interval, summarized_data_filled_weekday$steps.mean2,type="l", xlab="interval", ylab="steps", main = "weekday")
plot(summarized_data_filled_weekend$interval, summarized_data_filled_weekend$steps.mean2,type="l", xlab="interval", ylab="steps", main = "weekend")
```
<img class=center src=fig/weekday_weekend_compare.png>