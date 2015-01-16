# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(magrittr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(tidyr)
```

```
## 
## Attaching package: 'tidyr'
## 
## The following object is masked from 'package:magrittr':
## 
##     extract
```

```r
library(ggplot2)

activityDf <- read.csv('activity.csv', colClasses=c('integer', 'character', 'numeric'), stringsAsFactors=F) %>%
{
    .$date <- ymd(.$date)
    .
} 
```

## What is mean total number of steps taken per day?


To do so we are going to create a new column which is simply the day of the year for a given date. The code will then group by day of year and sum the steps for each day.


```r
stepsByDayDf <- activityDf %>%
    group_by(dayOfYear = yday(.$date)) %>%
    summarise(totalSteps = sum(steps)) %>% {
        meanStepsPerDay <<- mean(.$totalSteps, na.rm=T )
        medianStepsPerDay <<- median(.$totalSteps, na.rm=T)
        . #Return the dataset    
    }
```

Our mean and median steps per day is : 1.0766\times 10^{4} and 10765

We also can confirm this with the histogram. Noting that the highest frequency of days is for steps in the range 10,000 to 15,000 and the center of that range is 12500 which closely approximates our computed mean and median values.


```r
hist(stepsByDayDf$totalSteps, breaks = "Sturges",
     ylab='Frequency in Days', xlab='Step ranges',
     main = 'Occurrences of days for given step range',
     col='peachpuff', border='red')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 


## What is the average daily activity pattern?


```r
dailyIntervals <- activityDf %>% spread(interval, steps )
intervalSummary <- data.frame(meanStepsOverAllDays =
    sapply(dailyIntervals[,2:ncol(dailyIntervals)],
           function(x) mean(x, na.rm=T)),
    sequentialperoid = seq(0, 1435, by=5),
    clockperiod = names(dailyIntervals[,2:ncol(dailyIntervals)]))

#maxIntervalSteps <- which.max(intervalSummary$meanStepsOverAllDays)
```


```r
maxIntervalSteps <- which.max(intervalSummary$meanStepsOverAllDays)

p <- ggplot(intervalSummary, aes(x = sequentialperoid, y = meanStepsOverAllDays )) +
    geom_line(col='blue') +
    ggtitle("Mean Steps / Day") +
    scale_x_continuous(
        breaks=c(0, 250, 500, 750, 1000, 1250, 1500),
        labels=c('0','4:10', '8:20', '12:30', '16:40', '20:50', '25:00')) +
    labs(x='24Hr Time HH:MM', y='Mean steps per interval over all days.')
p
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
