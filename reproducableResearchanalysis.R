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

intervalSummary <- intervalSummary %>%
    mutate(weekday = )