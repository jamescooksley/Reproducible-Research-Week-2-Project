Coursera Data Science Courses Projects
Reproducible Research Week 2 Project
Author: James C

Date: AUG, 2017

Loading and processing the data
Load the data
Process/transform the data (if necessary) into a format suitable for your analysis

'''{r}
setwd("C://Users//u182335//Documents//DataScience//Course 5 Week 2")
'''

Have a look at the summary of data and first five rows of data

'''{r}
acti_data <- read.csv("activity.csv")
summary(acti_data)

head(acti_data)
'''

What is mean total number of steps taken per day?
The missing values in the dataset are ignored.

Calculate the total number of steps taken per day

'''{r}
steps <- aggregate(acti_data$steps, by = list(Date = acti_data$date), FUN = sum)
library(ggplot2)
names(steps)[names(steps) == "x"] <- "Total"
temp <- as.Date(steps$Date, "%Y-%m-%d")
steps$Date <- format(temp, format = "%m-%d")
head(steps)
'''

Make a histogram of the total number of steps taken each day

'''{r}
hist1 <- ggplot(data = na.omit(steps), aes(Total)) + 
  geom_histogram(binwidth = 1500, colour = "white") +
  xlab("Total Number of Steps Taken Each Day") +
  ylab("Count") +
  ggtitle("Histogram of the Total Number of Steps Taken Each Day")
print(hist1)
'''

Calculate and report the mean and median of the total number of steps taken per day

'''{r}
mean(na.omit(steps$Total))

median(na.omit(steps$Total))
'''

What is the average daily activity pattern?
Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis)
and the aveage number of steps taken, averaged across all days (y-axis) type = "l" means the plot is line graph.

'''{r}
five_min_steps <- aggregate(steps ~ interval, data = acti_data, FUN =mean)
TimeSeries1 <- ggplot(data = five_min_steps, aes(x = interval, y = steps)) + 
  geom_line() +
  xlab("Time Intervals (5 Minutes is an unit)") + 
  ylab("Total Number of Steps") +
  ggtitle("Average Number of Steps Taken of the 5-Minute Interval")
print(TimeSeries1)
'''
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

'''{r}
head(five_min_steps)

five_min_steps[which(five_min_steps$steps == max(five_min_steps$steps)),]
'''

Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as ????????). The presence
of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)

'''{r}
sapply(X = acti_data, FUN = function(x) sum(is.na(x)))
'''

Devise a strategy for filling in all of the missing values in the dataset.
The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
or the mean for that 5-minute interval, etc. I will use the mean for that 5 -minute interval to replace all 
the missing values in the dataset. At the end, I will check if all the NAs have been replaced.

'''{r}
library(dplyr)

replace_with_mean <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))
meanday <- (acti_data %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps)))
head(meanday)

sum(is.na(meanday))
'''

Create a new dataset that is equal to the original dataset but with the missing data filled in.

'''{r}
new_dataset <- as.data.frame(meanday)
head(new_dataset)

summary(new_dataset)
'''

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total daily number of steps?
Make a histogram of the total number of steps taken each day first by using the new version dataset

'''{r}
new_steps <- aggregate(new_dataset$steps, by = list(new_dataset$date), FUN = sum)
names(new_steps)[names(new_steps) == "x"] <- "Total"
names(new_steps)[names(new_steps) == "Group.1"] <- "Date"
hist2 <- ggplot(data = new_steps, aes(Total)) + 
  geom_histogram(binwidth = 1500, colour = "white") +
  xlab("Total Number of Steps Taken Each Day") +
  ylab("Count") +
  ggtitle("Histogram of the Total Number of Steps Taken Each Day with New Version Dataset")
print(hist2)
'''

Compare the two plots.

'''{r}
library(grid)
library(gridExtra)

grid.arrange(hist1, hist2, ncol = 2)
'''

Compare the mean and median:

'''{r} 
mean(na.omit(steps$Total))

median(na.omit(steps$Total))

mean(new_steps$Total)

median(new_steps$Total)
'''

We find that the highest count of the new version data is larger than the one we have with NAs.
The means of each dataset are the same. The medians of each dataset are slightly different.

Are there differences in activity patterns between weekdays and weekends?
Use the dataset with the filled-in missing values which is called new_steps dataset.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a 
given date is a weekday or weekend day.

'''{r}
new_dataset$WeekendOrWeekday <- ifelse(weekdays(as.Date(new_dataset$date)) %in% 
                              c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
head(new_dataset)
'''

Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and 
the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

'''{r}
new_dataset <- (new_dataset %>% group_by(interval, WeekendOrWeekday) %>% summarise(Mean = mean(steps)))
ggplot(new_dataset, mapping = aes(x = interval, y = Mean)) + geom_line() +
  facet_grid(WeekendOrWeekday ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")
'''
