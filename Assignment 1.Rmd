---
title: "Assignment 1 Reproducible Research"
author: "Bas"
date: "30 January 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=FALSE, results='hide', warning=FALSE, message=FALSE}
library(ggplot2)
library(scales)
library(ggplot2)
library(lattice)

```

## I will follow the template provided by rdpeng. 

## 1. Loading and preprocessing the data

### 1a. Load the data (i.e. read.csv())
```{r, results='markup', warning=TRUE, message=TRUE}
if(!exists('activity')){
activity <- read.csv('C:\\Users\\BZM11\\Documents\\R\\Reproducible Research\\ass1\\activity.csv')    
}
```

###1b. First I will call a str function to have a first look at the data

```{r}
str(activity)
```
## we can see that the date column is not rightly formatted so I will change the date to class Date

```{r}
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```


## 2. What is the mean total number of steps taken per day
#### For this part of the assignment, you can ignore the missing values in the dataset.

### 2a. Make a Histogram of the total number of steps taken each day



```{r}
activityclean <- na.omit(activity)
```


#### first we calculate the total number of steps per day
```{r}
totalstep <- tapply(activityclean$steps, activityclean$date, sum)
```



#### then we create our histogram

```{r}
hist(totalstep, breaks = 20,  main = 'Total steps taken each day', ylab= 'frequency', xlab = 'Number of steps', col = 'red', border = 'skyblue')
```
### 2b. Calculate and report the mean and median total number of steps taken per day

```{r}
mean(totalstep)

```

```{r}
median(totalstep)
```




## 3. What is the average daily activity pattern

```{r}
averagesteps <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```




### 3a. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
plot(averagesteps$interval, averagesteps$steps, type = 'l', lwd = 2, col = 'skyblue', main = 'Average number of steps taken', xlab = 'Interval', ylab = 'Average steps')
```




### 3b.CWhich 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
averagesteps$interval[which.max(averagesteps$steps)]
```

#### so the interval at 8.35 am contains the maximum number of steps


##4. Imputing missing values

#### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


### 4a. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
 
```{r}
sum(is.na(activity))
```
#### There are 2304 missing values in our data set


### 4b. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#### I will use the 5 minute interval mean to fill the missing values

### 4c. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
activitynew <- activity
for (i in averagesteps$interval) {
    activitynew[activitynew$interval == i & is.na(activitynew$steps), ]$steps <- 
        averagesteps$steps[averagesteps$interval == i]
}
```

```{r}
sum(is.na(activitynew))
```



### 4d. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### first we calculate the total number of steps per day
```{r}
totalstepnew <- tapply(activitynew$steps, activitynew$date, sum)
```

```{r}
hist(totalstepnew, breaks = 20,  main = 'Total steps taken each day', ylab= 'frequency', xlab = 'Number of steps', col = 'red', border = 'skyblue')
```


```{r}
mean(totalstepnew)

```

```{r}
median(totalstepnew)
```

#### The mean is the same as in the first part of the assignment and the median is slightly different. 

#### The difference for the median is 1.19 steps more. 

#### Since we put in the mean of the intervals it is likely that the mean does not change. As for the median more data points have been added to our data set which are closer to the mean. For this reason it does seem to make sense that the median will be closer to the mean for this data set

##5. Are there differences in activity patterns between weekdays and weekends
### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 5a. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
activitynew$wd<-weekdays(activitynew$date)
activitynew$fwd<- as.factor(c("weekend", "weekday"))
activitynew[activitynew$fwd == "Sunday" | activitynew$fwd == "Saturday"]<- factor("weekend")
activitynew[!(activitynew$fwd == "Sunday" | activitynew$fwd == "Saturday")]<- factor("weekday")
```
```{r}
activitynew$date <- as.Date(activitynew$date)

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

activitynew$wDay <-c('weekend','weekday')[(weekdays(activitynew$date) %in% weekdays1)+1L]
```

```{r}
table(activitynew$wDay)
```

### 5b. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```{r}
weeksteps <- aggregate(steps ~ interval + wDay, data = activitynew, mean, na.rm = TRUE)
```

```{r}

xyplot(steps ~ interval | wDay, data = weeksteps, type = 'l', lwd = 2, col = 'skyblue',layout = c(1,2), main = 'Average number of steps taken', xlab = 'Interval', ylab = 'Average steps')
```



