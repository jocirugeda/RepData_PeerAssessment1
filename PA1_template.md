# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r

# Donwload File from internet
# fileUrl<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
# download.file(fileUrl,destfile='activity.zip',method='curl')

unzip("activity.zip")

activity <- read.csv("activity.csv")

# create new factor for time interval for latter use
factor_interval <- factor(activity$interval)
activity$interval <- factor_interval

# convert values to integer for sorting purposes
activity$interval <- as.integer(activity$interval)

```


## What is mean total number of steps taken per day?

```r

sumarize_by_day <- function(ds_input) {
    # sum grouping by date removing Na values
    sum_by_day <- tapply(ds_input$steps, ds_input$date, sum, na.rm = TRUE)
    data_by_day <- data.frame(names(sum_by_day), sum_by_day)
    # change columns names
    names(data_by_day) <- c("date", "sum_steps")
    data_by_day
}

ds_by_day <- sumarize_by_day(activity)
# create histogram
hist(ds_by_day$sum_steps, xlab = "total number of steps taken each day", main = " Histogram number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r


# compute mean
daily_average <- mean(ds_by_day$sum_steps, na.rm = TRUE)
# compute median
daily_median <- median(ds_by_day$sum_steps, na.rm = TRUE)

```

- `9354.23 ` Is The mean total number of steps per day
- `10395 ` Is The median for the total number of steps per day 


## What is the average daily activity pattern?

1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r


sumarize_by_interval <- function(ds_input) {
    # compute mean by same interval
    avg_by_time <- tapply(ds_input$steps, ds_input$interval, mean, na.rm = TRUE)
    # create data frame from vector
    data_by_time <- data.frame(names(avg_by_time), avg_by_time)
    # change column names
    names(data_by_time) <- c("interval", "avg_steps")
    # convert values to integer for sorting them
    data_by_time$interval <- as.integer(data_by_time$interval)
    # sort by interval
    sort_data <- data_by_time[order(data_by_time$interval), ]
    # change column names
    names(sort_data) <- c("interval", "avg_time_steps")
    
    sort_data
    
}

ds_by_interval <- sumarize_by_interval(activity)

plot(ds_by_interval$interval, ds_by_interval$avg_time_steps, type = "l", xlab = "Time interval", 
    ylab = "Average steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


2- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r

max_interval <- ds_by_interval[ds_by_interval$avg_time_steps == max(ds_by_interval$avg_time_steps), 
    ]

max_time <- max_interval$interval
```


The interval that the maximum number of steps is the  `7 ` 



## Imputing missing values
1- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r

# extract NA rows
row_na <- activity[is.na(activity$steps), ]

size_na <- nrow(row_na)

```


The number of row with NA data is `2304`


2- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# using the interval mean of steps , we can fill the NA values with the mean
# for that interval


# merge by column interval to generate a dataset with each interval mean
# asigned

# change type to be the same
row_na$interval <- as.integer(row_na$interval)

mix_df <- merge(row_na, ds_by_interval, by = "interval")
```



3- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# select some of the columns
new_na <- mix_df[, c("avg_time_steps", "date", "interval")]
# change of column names
names(new_na) <- c("steps", "date", "interval")

# rows without NA values
row_ok <- activity[!is.na(activity$steps), ]

new_ds <- rbind(new_na, row_ok)

new_ds$interval <- as.integer(new_ds$interval)

sort_new_ds <- new_ds[order(new_ds$date, new_ds$interval), ]

```


4- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r

ds_by_day <- sumarize_by_day(sort_new_ds)

hist(ds_by_day$sum_steps, xlab = "total number of steps taken each day", main = " Histogram number of steps ( NA filled with interval mean")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r

daily_average <- mean(ds_by_day$sum_steps)

daily_median <- median(ds_by_day$sum_steps)

```


Here the summary for the new dataset with the NA replaced by mean of interval
- `10766.19 ` Is The mean total number of steps per day
- `10766.19 ` Is The median for the total number of steps per day 


## Are there differences in activity patterns between weekdays and weekends?


```r

# interval_factor<-factor(sort_new_ds$interval)

# sort_new_ds$interval<-interval_factor

sort_new_ds$DayNumber <- as.POSIXlt(as.Date(sort_new_ds$date, "%Y-%m-%d"))$wday

# split of Data set
ds_weekdays <- sort_new_ds[sort_new_ds$DayNumber != 0 & sort_new_ds$DayNumber != 
    6, ]

ds_weekends <- sort_new_ds[sort_new_ds$DayNumber == 0 | sort_new_ds$DayNumber == 
    6, ]


# sumarize each dataset by interval
weekdays_by_interval <- sumarize_by_interval(ds_weekdays)

weekends_by_interval <- sumarize_by_interval(ds_weekends)


# generate panel with plots comparing datasets
par(mfrow = c(2, 1))

plot(weekdays_by_interval$interval, weekdays_by_interval$avg_time_steps, type = "l", 
    xlab = "Time interval", ylab = "Average steps", main = " Weekdays")


plot(weekends_by_interval$interval, weekends_by_interval$avg_time_steps, type = "l", 
    xlab = "Time interval", ylab = "Average steps", main = " Weekends")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


