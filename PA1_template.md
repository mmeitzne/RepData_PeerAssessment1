Activity Monitoring Project 1
================
Mike Meitzner
February 18, 2017

``` r
library(knitr)

opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
```

Read data
---------

Code for reading in the dataset and/or processing the data: Read the Activity Monitoring Data provided by the course. This data was downloaded from the course site and placed in the working directory.

``` r
file <- "activity.csv"

activity <- read.csv(file)
```

Steps per day
-------------

Sums the steps for each day and eliminates NA values.

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
totalstepsperday <- aggregate(steps~date, activity, sum, na.rm=TRUE)

head(totalstepsperday)
```

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

Mean and Median Steps per Day
-----------------------------

Mean and median number of steps taken each day

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
meansteps <- mean(totalstepsperday$steps)
meansteps

mediansteps <- median(totalstepsperday$steps)
mediansteps
```

    ## [1] 10766.19
    ## [1] 10765

Time series of steps taken
--------------------------

Time series plot of the average number of steps taken What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
library(ggplot2)

stepsperint <- aggregate(steps~interval, activity, mean, na.rm=TRUE)
ggplot(stepsperint, aes(interval, steps)) + geom_line() + 
    labs(x="Interval", y ="Avg Number of Steps") 
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

Interval with most steps
------------------------

The 5-minute interval that, on average, contains the maximum number of steps

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
stepsperint <- aggregate(steps~interval, activity, mean, na.rm=TRUE)
maxinterval <- stepsperint[which.max(stepsperint$steps),]$interval
maxinterval
```

    ## [1] 835

Histogram
---------

Histogram of the total number of steps taken each day

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
library(ggplot2)

ggplot(data=totalstepsperday, aes(x=steps)) + geom_histogram(binwidth=300) +
    scale_y_continuous(breaks=seq(0,15,1)) +  labs(x="Steps per Day", y ="Number of Days")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)

Imputing missing data
---------------------

Code to describe and show a strategy for imputing missing data

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
nas <- sum(is.na(activity$steps))
nas
```

    ## [1] 2304

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Solution: I will use the mean per interval for imputation

Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
updated <- function(x, y)
  {
    updatedsteps <- x$steps
    allnas <- which(is.na(updatedsteps))
    for (a in allnas)
        {
        intrvl <- x[a,]$interval
        avgstep <- y[y$interval==intrvl,]$steps  
        updatedsteps[a] <- avgstep
        }
    updatedsteps
  }

updatedsteps <- updated(activity, stepsperint)
activity_2 <- data.frame(steps = updatedsteps, date=activity$date, interval=activity$interval)
head(activity_2)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
library(ggplot2)

spdtotal_2 <- aggregate(steps~date, activity_2, sum, na.rm=TRUE)
meanstepsperday2 <- mean(spdtotal_2$steps)
medianstepsperday2 <- median(spdtotal_2$steps)
ggplot(data=spdtotal_2, aes(x=steps)) + geom_histogram(binwidth=300) + 
    scale_y_continuous(breaks=seq(0,15,1)) +
    labs(x="Average Total Number of Steps per Day", y ="Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
meanstepsperday2
medianstepsperday2
```

    ## [1] 10766.19
    ## [1] 10766.19

The mean and median are now the same (this makes sense as we forced it to through imputation). It was very close before, and imputation did not drastically change anything. The mode peak steps per day interval shifted to slightly later in the day and there are now 9 instances of that amount as opposed to 5.

Differences in activity patterns between weekdays and weekends
--------------------------------------------------------------

Using the imputed data set this part shows the different patterns between weekdays adn weekends

Note: I'm using the package lubridate to create a variable "dayofweek" where 1=Sunday through 7=Saturday. The variable "DayCategory" indicates weekend vs weekday

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
library(lubridate)
library(dplyr)

activity_2 <- mutate(activity_2, date = ymd(date), dayofweek = wday(date), 
                    DayCategory = ifelse(dayofweek != 1 & dayofweek != 7,"Weekday", 
                                      ifelse(dayofweek == 1 | dayofweek == 7, "Weekend", NA)))
activity_2 <- mutate(activity_2, DayCategory = as.factor(DayCategory))

head(activity_2)
```

    ##       steps       date interval dayofweek DayCategory
    ## 1 1.7169811 2012-10-01        0         2     Weekday
    ## 2 0.3396226 2012-10-01        5         2     Weekday
    ## 3 0.1320755 2012-10-01       10         2     Weekday
    ## 4 0.1509434 2012-10-01       15         2     Weekday
    ## 5 0.0754717 2012-10-01       20         2     Weekday
    ## 6 2.0943396 2012-10-01       25         2     Weekday

Find the averages for weekday and weekend intervals:

``` r
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)
stepsperintbydaytype <- aggregate(steps~DayCategory+interval, activity_2, mean, na.rm=TRUE)
ggplot(stepsperintbydaytype, aes(x=interval, y=steps)) + 
        geom_line(color=rgb(.1,.1,.1)) + 
        facet_wrap(~ DayCategory, nrow=2, ncol=1) +
        labs(x="Interval", y="Avg Steps") 
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)

It looks like there is quite a difference between weekday vs weekend. Weekdays have higher peaks and lower valleys throughout the day. For data sets like this, a signficiant factor like this should be considered. THis would impact the histograms created earlier in this assignment.
