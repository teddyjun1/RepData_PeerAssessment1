# install respective packages first and load the libraries

install.packages("dplyr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("timeDate")

# install.packages("plyr")####
library(plyr)
# install.packages("dplyr")#####
library(dplyr)
# install.packages("ggplot2")#####
library(ggplot2)
#install.packages("timeDate")### #
library(timeDate)


# removing missing values

good <- complete.cases(activity)
good_activity <- activity[good,]

# need dplyr
by_day <- group_by(good_activity, date)
steps_by_day <- summarize(by_day, total_steps_daily = sum(steps))
steps_by_day

# summarize data
date_summary <- ddply(good_activity, .(date), summarise, total_steps = sum(steps, na.rm = TRUE))

# plot the summary
qplot(total_steps, data = date_summary, xlab = "Total number of steps per day", binwidth = 500) 


# calculate mean total steps
mean_total_steps <- mean(date_summary$total_steps, na.rm = TRUE)

# calculate median total steps
median_total_steps <- median(date_summary$total_steps, na.rm = TRUE)


# summarize interval mean
interval_mean <- ddply(activity, .(interval), summarise, mean_steps = mean(steps, na.rm = TRUE))

# plot interval means using ggplot
p <- ggplot(interval_mean, aes(interval, mean_steps))
p + geom_line(color = "blue") + xlab("Interval") + ylab("Mean number of steps")

## Function to determine the interval with maximum number of steps
## 
maxinterval <- function(x){
  maxstep = x[1,2]
  maxi = 1
  
  for (i in 2:nrow(x)) 
    if (x[i,2] > maxstep){ 
      maxi = i
      maxstep = x[i,2]
      
    }
  x[maxi,1]
}

# call function to return interval with maximum number of steps
max_interval <- maxinterval(interval_mean)


### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


number_missing_values <- sum(!complete.cases(activity))


#### Strategy for filling in all of the missing values in the dataset.###

imputed_activity <- activity
for (i in 1:nrow(imputed_activity)){
  if (is.na(imputed_activity[i,1]))
    for (j in 1:nrow(interval_mean))
      if (imputed_activity[i,3] == interval_mean[j,1])
        imputed_activity[i,1] = interval_mean[j,2]
}

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
##

##making a histogram of total number of steps taken each day

activity_summary <- ddply(imputed_activity, .(date), summarise, new_total_steps = sum(steps))
qplot(new_total_steps, data = activity_summary, xlab = "Total number of steps", binwidth = 1000)

##Calculate and report the mean and median total number of steps taken per day###

new_mean <- mean(activity_summary$new_total_steps)
new_median <- median(activity_summary$new_total_steps)


#### Are there differences in activity patterns between weekdays and weekends?

#install.packages("timeDate") 
#library(timeDate)

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
imputed_activity$weekdays <- factor(isWeekday(as.Date(as.character(imputed_activity$date), format = "%Y-%m-%d"), wday = 1:5), labels = c("weekend", "weekday"))

## summarize data
imputed_interval_mean <- ddply(imputed_activity, c('weekdays','interval'), summarise, mean_steps = mean(steps, na.rm = TRUE))


### Make a **panel plot** containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
###


library(lattice)
xyplot(mean_steps ~ interval | weekdays, data = imputed_interval_mean, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Mean number of steps")
