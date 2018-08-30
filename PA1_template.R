## loaddata
data <- read.csv("C:/Users/szhang/Desktop/class materials/repdata%2Fdata%2Factivity/activity.csv")

## question 1-----------------------------------------
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm=TRUE)
qplot(total.steps,binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm = TRUE)
median(total.steps,na.rm = TRUE)

##question 2---------------------------------------
averages <- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)

plot(steps ~ interval, data = averages, type = "l", 
       ylab = "average number of steps", xlab="5-min intervals",
       main="Average number of steps by 5-minutes intervals")

averages[which.max(averages$steps),]

##Imputing missing values----------------------------------
missing<-is.na(data$steps)
table(missing)

# Replace each missing value with the mean value of its 5-minute interval

fill.value<-function(steps,interval){
  filled<-NA
  if (!is.na(steps))
    filled<- c(steps)
  else
    filled <- (averages[averages$interval==interval,"steps"])
  return(filled)
}
fill.data <-data
fill.data$steps <-mapply(fill.value, fill.data$steps,fill.data$interval)

# make histogram
total.steps <-tapply(fill.data$steps, fill.data$date, FUN =sum)
qplot(total.steps, binwidth = 1000,  xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)

## Are there differences in activity patterns between weekdays and weekends?
weekday.weekend <- function(date){
  day <-weekdays(date)
  if (day %in% c("Monday", "Tuesday","Wednesday","Thursday","Friday"))
    return("weekday")
  else if (day %in% c("Saturday","Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
fill.data$date <- as.Date(fill.data$date)
fill.data$day <-sapply(fill.data$date, FUN=weekday.weekend)

##plotting---------------------------------------------------
interval.averages <- aggregate(steps ~ interval  + day, data = fill.data, mean)

ggplot(interval.averages, aes(interval, steps)) + geom_line()+
  facet_grid(day ~ .) + xlab("5min interval") + ylab("number of steps")

plot(steps ~ interval , data = interval.averages, type = "l",
     layout = c(1,2), ylab = "Number of steps", xlab="5-min.interval", main = "Average  5-min. activity intervals: Weekdays vs. Weekends")
