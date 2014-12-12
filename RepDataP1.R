
library(dplyr)
library(lattice)
library(grid)

DF1 <- read.csv("activity.csv")
DF2 <- na.omit(DF1)
DF3 <- group_by(DF2,date)
DF4 <- summarise(DF3, steps = sum(steps))
DF5 <- summarise(DF4, steps = mean(steps))
DF6 <- summarise(DF4, steps = median(steps))
hist(DF4$steps, xlab ="Steps", main="Total number of steps taken each day (missing values omitted)")

#Deel 2


DF7 <- group_by(DF2, interval)
DF8 <- summarise(DF7, steps = mean(steps))
DF9 <- data.frame(tel = 0:287, DF8)





with(DF9, plot(tel, steps, type = "l", xaxt = "n", xlab="Interval", ylab="Number of steps", main="Time series plot of the average steps per interval"))
ticks <- c("00:00", "06:00", "12:00","18:00", "23:55")
axis(1,at=c(0,73,145,217,287),labels=ticks)

#interval with max steps

DFmax <- summarise(DF8, steps = max(steps))
DFintmax <- DF8[grep(DFmax, DF8$steps), ]

#Imputing missing values

# 1: total NA

x <- as.numeric(is.na(DF1))
y <- sum(x)

# 2:Strategy missing values

DFimp2 <- DF1

for (i in 1:length(DFimp2$steps)) {
        if (is.na(DFimp2$steps[i]) == TRUE ){
               
                DFimp2$steps[i] <- subset(DF8, interval == DFimp2$interval[i], select = steps)
        }
        
       
        
}
DFimp2$steps <- unlist(DFimp2$steps)
DFimp3 <- group_by(DFimp2,date)
DFimp4 <- summarise(DFimp3, steps = sum(steps))
DFimp5 <- summarise(DFimp4, steps = mean(steps))
DFimp6 <- summarise(DFimp4, steps = median(steps))
hist(DFimp4$steps, xlab="Interval", ylab="Number of steps", main="Total number of steps taken each day with missing values imputed")

#3 turn it into factor variables
DFimp2$date <- as.Date(DFimp2$date, "%Y-%m-%d")
DFimp2$date <- weekdays(DFimp2$date)
DFimp2$date <- factor(DFimp2$date)
levels(DFimp2$date)[1:5] <- "weekdays"
levels(DFimp2$date)[2:3] <- "weekend"



DFweek <- DFimp2[grep("weekdays", DFimp2$date), ]
DFwknd <- DFimp2[grep("weekend", DFimp2$date), ]

#par(mfrow=c(2,1))
DFweek2 <- group_by(DFweek, interval)
DFweek3 <- summarise(DFweek2, steps = mean(steps))
DFweek4 <- data.frame(days = rep("weekday", 288), DFweek3)
#with(DFweek3, plot(interval, steps, type = "l"))

DFwknd2 <- group_by(DFwknd, interval)
DFwknd3 <- summarise(DFwknd2, steps = mean(steps))
DFwknd4 <- data.frame(days = rep("weekend", 288), DFwknd3)
#with(DFwknd3, plot(interval, steps, type = "l"))

DFdays <- rbind(DFweek4, DFwknd4)

xyplot(steps ~ interval | days,
       data=DFdays,
       main="Weekdays vs Weekends",
       ylab="Number of steps", xlab="Interval",
       type="l", layout=c(1,2))

