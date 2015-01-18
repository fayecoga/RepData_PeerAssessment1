library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

activityDf <- read.csv('activity.csv', colClasses=c('integer', 'character', 'numeric'), stringsAsFactors=F) %>%
{
    .$date <- ymd(.$date)
    .
}

stepsByDayDf <- activityDf %>%
    group_by(dayOfYear = yday(.$date)) %>%
    summarise(totalSteps = sum(steps)) %>% {
        meanStepsPerDay <<- round(mean(.$totalSteps, na.rm=T ),1)
        medianStepsPerDay <<- median(.$totalSteps, na.rm=T)
        . #Return the dataset
    }


hist(stepsByDayDf$totalSteps, breaks = "Sturges",
     ylab='Frequency in Days', xlab='Step ranges',
     main = 'Occurrences of days for given step range',
     col='peachpuff', border='red')



dailyIntervals <- activityDf %>% spread(interval, steps )
intervalSummary <- data.frame(meanStepsOverAllDays =
                                  sapply(dailyIntervals[,2:ncol(dailyIntervals)],
                                         function(x) mean(x, na.rm=T)),
                              sequentialperoid = seq(0, 1435, by=5),
                              clockperiod = names(dailyIntervals[,2:ncol(dailyIntervals)]))

maxIntervalStepsIndex <- which.max(intervalSummary$meanStepsOverAllDays)

clockIntervalWithMaximumSteps <-
    intervalSummary$clockperiod[maxIntervalStepsIndex]
sequential5MinIntervalWtihMaximumSteps <-
    intervalSummary$sequentialperoid[maxIntervalStepsIndex]

p <- ggplot(intervalSummary, aes(x = sequentialperoid, y = meanStepsOverAllDays )) +
    geom_line(col='blue') +
    ggtitle("Mean Steps / Day") +
    scale_x_continuous(
        breaks=c(0, 250, 500, 750, 1000, 1250, 1500),
        labels=c('0','4:10', '8:20', '12:30', '16:40', '20:50', '25:00')) +
    labs(x='24Hr Time HH:MM', y='Mean steps per interval over all days.')
p

imputedDataFrame <- dailyIntervals %>%
{
    z <- rbind( rbind(.[1,], .[1,]), rbind(., rbind(.[nrow(.),], .[nrow(.),])))
    z[1:2,2:ncol(z)] <- 0
    z[nrow(z)-1, 2:ncol(z)] <- 0
    z[nrow(z), 2:ncol(z)] <- 0
    row.names(z) <- NULL
    z
}

for(interval in 2:ncol(imputedDataFrame)) {
     for (ndx in 3:nrow(imputedDataFrame) - 2)) {
         if(is.na(imputedDataFrame[ndx,interval]))
         {
             imputedDataFrame[ndx,interval] <-
                 mean(
                     imputedDataFrame[ndx-2,interval],
                     imputedDataFrame[ndx-1,interval],
                     0,
                     imputedDataFrame[ndx+1,interval],
                     imputedDataFrame[ndx+2,interval],
                     na.rm=T)
         }
     }
}

dayTypeSummary <- dailyIntervals %>%
    mutate(dayType = factor(
        ifelse(wday(.$date) == 1 | wday(.$date) == 7,"weekend","weekday"))
    ) %>%
    group_by(dayType) %>%
    summarise_each(funs(mean(., na.rm=T)), -c(date, dayType)) %>%
    gather(dayType) %>%
    {
        names(.) <- c('dayType', 'clockinterval', 'avgsteps')
        .
    } %>%
    mutate(sequentialInterval = rep(seq(0, 1435, by=5), each=2 ))
ggplot(dayTypeSummary, aes(x=sequentialInterval, y=avgsteps, group=dayType)) + geom_line() + facet_grid(dayType ~ .)