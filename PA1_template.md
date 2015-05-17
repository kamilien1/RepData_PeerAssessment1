# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
We want to do a few things to clean this data:

- unzip
- read as a csv
- process the date column from character to Date-type data
 
The data source can be downloaded [here.](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) The pre-processing code is shown below.

 

```r
unzip("activity.zip",exdir="./data")
out_file <- "./data/activity.csv"
play_data <- read.csv(out_file, header = TRUE, sep = ",", na.strings="NA",stringsAsFactors=F)
play_data$date <- as.Date(play_data$date, format = "%Y-%m-%d")
```

Now the data is *cleaned up* and ready for analysis. Recall the data is comprised of three columns:

- **steps:** raw number of steps walked in a 5-minute interval. NA values exist.
- **date:** the year/month/day of the activity
- **interval:** a 5 minute interval from 00:00 to 23:55 in a 24-hour period

## What is mean total number of steps taken per day?
We must sum up the total number of steps taken per day, omitting any `NA` values in the denominator. For example, if there are 10 data points and 2 are `NA`, we would sum up 8 values and divide by 8 to get the sum. The `agregate` function allows us to do this. We summary steps by date.

```r
steps_count <- aggregate(steps ~ date,sum,data=play_data)
steps_mean<- as.integer(mean(steps_count$steps))
steps_median<-median(steps_count$steps)
print(paste("Mean steps per day:",steps_mean,"Median steps per day:",steps_median))
```

```
## [1] "Mean steps per day: 10766 Median steps per day: 10765"
```
Now that we know the mean (10766) and median (10765) values for steps *per day*, let's plot this data, with `steps_mean` to show the average steps *per day* taken. 

```r
par(mar=c(5,4,3,1),las=1)
hist(steps_count$steps, breaks=20,col="red",xlab="Total Steps in a day",
     ylab="Frequency of Days", main="Frequency of Total Steps")
rug(steps_count$steps)
abline(v = steps_mean, col = "blue", lwd = 2)
text_x = steps_mean; text_y = 10
text(text_x, text_y, paste("Mean:",as.integer(steps_mean)), pos=2) 
```

<img src="PA1_template_files/figure-html/chunk3-plot1-1.png" title="" alt="" style="display: block; margin: auto;" />



## What is the average daily activity pattern?
Average daily activity pattern means how many steps does one walk based on the time of the day (from `00:00` to `23:55`). Here, we are interested in averaging values based on a common `interval` factor, rather than a `date` factor. We will continue off the variables in the previous problem.

```r
avgs_interval<- aggregate(steps ~ interval,mean, data=play_data)
max_average_steps_interval <- as.integer(max(avgs_interval$steps))
print(paste("Maximum average steps is:",max_average_steps_interval,"at interval",
            avgs_interval[which(avgs_interval$steps==max(avgs_interval$steps)),1]))
```

```
## [1] "Maximum average steps is: 206 at interval 835"
```
So, `interval` 835 has 206 `steps`. Looks like this is a morning person! Keep in mind we are **ignoring** `NA` values, which, later on, we'll see has an effect on this average!

```r
plot(x=avgs_interval$interval,y=avgs_interval$steps,type="l",
     xlab="5 Minute Interval 00:00 to 23:55",ylab="Average Step Count",
     main="Average Steps Taken by Interval Over All Days Observed")
abline(h=max_average_steps_interval,col="red")
```

<img src="PA1_template_files/figure-html/chunk5-plot2-1.png" title="" alt="" style="display: block; margin: auto;" />


## Imputing missing values
There are a number of `NA` values for steps. In fact, there are **2304** `NA` values. Let's make a new data set `clean_data` to play with. To clean the code, I chose a for-loop for ease of use.

```r
clean_data <- play_data
head(clean_data,6)
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

```r
# Get the mean of steps by interval
tapp_avgs <- tapply(play_data$steps,play_data$interval,mean,na.rm=TRUE)
# If NA exists, replace NA with mean value for current interval
for (i in 1:length(clean_data$steps)){
    if (is.na(clean_data[i,1])) {
        clean_data[i,1] <- tapp_avgs[as.character(clean_data[i,3])]    
    }    
}
head(clean_data,6)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
# how many NA's are left?
sum(is.na(clean_data))
```

```
## [1] 0
```
Now that we have cleaned the data and replaced `NA` values, let's plot again. For good measure, we will also show the differences.


```r
par(mfrow=c(1,2))

clean_steps_count <- aggregate(steps ~ date,sum,data=clean_data)
clean_steps_mean<-as.integer(mean(clean_steps_count$steps)); clean_steps_median<-as.integer(median(clean_steps_count$steps))

hist(clean_steps_count$steps, breaks=20,col="blue",xlab="Total Steps in a Day",
     ylab="Frequency of Days", main="Frequency of Total Steps\n with NA Replacement")
rug(clean_steps_count$steps)
abline(v=clean_steps_mean,col="red",lwd=2)


# show the old plot for a side-by-side comparison
hist(steps_count$steps, breaks=20,col="red",xlab="Total Steps in a Day",
     ylab="Frequency of Days", main="Frequency of Total Steps\n without NA Replacement")
rug(steps_count$steps)
abline(v = steps_mean, col = "blue", lwd = 2)
```

![](PA1_template_files/figure-html/chunk7-plot3-1.png) 

The new mean is 10766 and median is 10766. 



## Are there differences in activity patterns between weekdays and weekends?
Using the clean_data dataset with no `NA` values left, let's see if the user is more active during the weekdays (Monday-Friday) or weekends! 


```r
clean_data$weekdays <- weekdays(clean_data$date)
days <- unique(clean_data$weekdays)
weekday <- days[1:5]
weekend <- days[6:7]

wkday <- which(clean_data$weekdays %in% weekday)
wknd <- which(clean_data$weekdays %in% weekend)
weekday_count <- aggregate(steps ~ interval,mean,data=clean_data[wkday,])
weekday_count$wk <- "weekday"
weekend_count <- aggregate(steps ~ interval,mean,data=clean_data[wknd,])
weekend_count$wk <- "weekend"
nw_data <- rbind(weekday_count,weekend_count)

library(lattice)
xyplot( steps ~ interval | wk, data=nw_data,type="l",layout=c(2,1),
        at = seq(min(nw_data$interval),max(nw_data$interval),max(nw_data$interval)/20),
        main="Average Number of Steps Walked by Interval and Day Type")
```

![](PA1_template_files/figure-html/chunk8-plot4-1.png) 

So it looks like our person of interest is more active on the **average** weekend rather than the **average** weekday. By how much? Let's look!


```r
diff <- round(100*(sum(weekday_count$steps)-sum(weekend_count$steps))/sum(weekday_count$steps),
              digits=1)
print(paste("This person is",diff,"percent less active on weekdays than weekends."))
```

```
## [1] "This person is -19 percent less active on weekdays than weekends."
```
