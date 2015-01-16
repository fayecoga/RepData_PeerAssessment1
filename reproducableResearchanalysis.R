library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

stepsPerDay <- read.csv('activity.csv', stringsAsFactors=F) %>%
{
    .$date <- ymd(.$date)
    .
} %>%
group_by(dayOfYear = yday(.$date)) %>%
summarise(totalSteps = sum(steps))

meanStepsPerDay = mean(stepsPerDay$totalSteps, na.rm=T )
medianStepsPerDay = median(stepsPerDay$totalSteps, na.rm=T)

hist(stepsPerDay$totalSteps, breaks = "Sturges",
     ylab='Frequency in Days', xlab='Step ranges',
     main = 'Occurrences of days for given step range',
     col='peachpuff', border='red')


activity <- read.csv('activity.csv', stringsAsFactors=F) %>%
{
    .$date <- ymd(.$date)
    .
}
dailyIntervals <- activity %>% spread(interval, steps )
intervalSummary <- data.frame(meanStepsOverAllDays =
    sapply(dailyIntervals[,2:ncol(dailyIntervals)],
           function(x) mean(x, na.rm=T)),
    sequentialperoid = seq(0, 1435, by=5),
    clockperiod = names(dailyIntervals[,2:ncol(dailyIntervals)]))

maxIntervalSteps <- which.max(intervalSummary$meanStepsOverAllDays)

p <- ggplot(intervalSummary, aes(x = sequentialperoid, y = meanStepsOverAllDays )) +
    geom_line(col='blue') +
    ggtitle("Mean Steps / Day") +
    scale_x_continuous(
        breaks=c(0, 250, 500, 750, 1000, 1250, 1500),
        labels=c('0','4:10', '8:20', '12:30', '16:40', '20:50', '25:00')) +
    labs(x='24Hr Time HH:MM', y='Mean steps per interval over all days.')
p
