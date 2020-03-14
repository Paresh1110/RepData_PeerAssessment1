---
title: "PA1_template"
author: "Paresh Prabhu"
date: "3/14/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
library(ggplot2)

## Loading and preprocessing the data
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
activityData <- read.csv('activity.csv')


## What is mean total number of steps taken per day?

steps_Day <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
qplot(steps_Day, color = "", xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)

steps_DayMean <- mean(steps_Day)
steps_DayMedian <- median(steps_Day)
##Mean: 9354.2295
##Median: 10395


## What is the average daily activity pattern?
avgSteps_TimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)

ggplot(data=avgSteps_TimeBlock, aes(x=interval, y=meanSteps)) + geom_line() +
xlab("5-minute interval") + ylab("average number of steps taken")

mostSteps <- which.max(avgSteps_TimeBlock$meanSteps)
time_Steps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgSteps_TimeBlock[mostSteps,'interval'])
##Most Steps at: 8:35


## Imputing missing values
library(Hmisc)
Miss_Values <- length(which(is.na(activityData$steps)))
##Number of missing values: 2304
actvDataImputed <- activityData
actvDataImputed$steps <- impute(activityData$steps, fun=mean)
stepsDayImputed <- tapply(actvDataImputed$steps, actvDataImputed$date, sum)

qplot(stepsDayImputed, color="", xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)

stepsByDayMeanImputed <- mean(stepsDayImputed)
stepsByDayMedianImputed <- median(stepsDayImputed)
##Mean (Imputed): 1.0766 × 104
##Median (Imputed): 1.0766 × 104


## Are there differences in activity patterns between weekdays and weekends?
actvDataImputed$dateType <-  ifelse(as.POSIXlt(actvDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
avgActvDataImputed <- aggregate(steps ~ interval + dateType, data=actvDataImputed, mean)

ggplot(avgActvDataImputed, aes(interval, steps)) + geom_line() + facet_grid(dateType ~ .) +
xlab("5-minute interval") + ylab("avarage number of steps")
