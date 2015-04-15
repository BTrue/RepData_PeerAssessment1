###################################################
# Brian True
# 04.15.15
# Reproducible Data -- Coursera
# Peer Assesment #1
###################################################
#
# dataset can be found at: 
# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
#
# GitHub repo for assignment:
# http://github.com/rdpeng/RepData_PeerAssessment1
# 
###################################################
## LOADING AND PREPROCESSING THE DATA ##
###################################################

# step0: load necessary packages after simple check to see if installed 
# NOTE: 'pkgs' holds names of all necessary packages
pkgs <- c("data.table","lattice")
for(pkg in pkgs){
     if(!require(pkg,character.only=T)){
          suppressWarnings(require(pkg,quietly=T,warn.conflicts=F))
     }
}

## Set present directory as home directory
WD <- getwd()

# step1: read the dataset
if(!file.exists("./DATA")){dir.create("./DATA")}
setwd("./DATA")
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,"activity.zip",method="wget",quiet=T)
unzip("activity.zip")
activity_DT <- fread("activity.csv")
setwd(WD)

# step2: calculate total number of steps per day
trim_DT <- activity_DT[,list(total.steps=sum(steps),mean.steps=mean(steps)),
                       by='date']

# step3: plot histogram of total number of steps taken per day
hist(trim_DT[,total.steps],
     breaks=25,
     col="red",
     xlab="Total Steps/Day",
     ylab="Number of Days in Each Range (Bins of 1000 Steps Each)",
     main="Histogram of Total Steps per Day",
     las=1
)

# step4: calculate & report mean and median of total number of steps taken per day
mean.total.steps <- mean(trim_DT[,total.steps],na.rm=T)
median.total.steps <- median(trim_DT[,total.steps],na.rm=T)
diff <- abs(mean.total.steps - median.total.steps)
print(noquote(sprintf("THE MEAN OF TOTAL STEPS PER DAY: %.4f steps",
                      mean.total.steps)))
print(noquote(sprintf("THE MEDIAN OF TOTAL STEPS PER DAY: %.4f steps",
                      median.total.steps)))
print(noquote(sprintf("THE SPREAD BETWEEN THE MEAN & MEDIAN IS: %.4f step(s)",
                      diff)))

# step5: plot a time series
datetimes <- seq(ISOdatetime(2012,10,01,0,0,0,tz="GMT"),
                 ISOdatetime(2012,11,30,23,55,0,tz="GMT"),by=(60*5))

times <- as.factor(format(datetimes[1:288],"%H:%M"))

interval_DT <- data.table(
     aggregate(steps ~ interval,
               data=activity_DT,
               mean,
               na.rm=T)
)
interval_DT <- interval_DT[,times:=times]

plot(interval_DT$interval,interval_DT$steps,
     type="l",
     xlab="Time",
     ylab="Average Steps",
     main="Average Steps / Time Interval",
     col="blue",
     yaxt="n",
     xaxt="n"
     
)
abline(v=835,col="red")
legend(x="topright", legend="max", col="red", bty="n",lwd=1)
axis(side=2,
     las=2
)

axis(side=1,
     las=0,
     at=c(0,400,800,1200,1600,2000,2400),
     labels=c("00:00","04:00","08:00","12:00","16:00","20:00","24:00")
)

# step6: find the interval with the greatest average number of steps
max <- interval_DT[which.max(interval_DT$steps),]$interval

print(
     noquote(
          sprintf("THE INTERVAL WITH THE GREATEST AVERAGE NUMBER OF STEPS: Interval %.0f",
                  max))
)

# step7: report the number of rows containing 'NA' values
print(noquote("NUMBER OF 'NA' VALUES IN EACH COLUMN: "))
print(colSums(is.na(activity_DT)))

# step8: Calculate fill value for NA
fill <- mean(activity_DT[,steps],na.rm=T)

activity_DT2 <- replace(activity_DT,is.na(activity_DT),fill)
filled_DT <- activity_DT2[,list(total.steps=sum(steps),mean.steps=mean(steps)),
                          by="date"]

# step10: Histogram of filled data
hist(filled_DT[,total.steps],
     breaks=25,
     col="green",
     xlab="Total Steps/Day",
     ylab="Number of Days in Each Range (Bins of 1000 Steps Each)",
     main="Histogram of Total Steps per Day",
     las=1
)

# step10: calculate the mean & median value of step in filled data
filled.mean.total.steps <- mean(filled_DT[,total.steps],na.rm=T)
filled.median.total.steps <- median(filled_DT[,total.steps],na.rm=T)
filled.diff <- abs(filled.mean.total.steps - filled.median.total.steps)

print(noquote(sprintf("THE MEAN OF TOTAL STEPS PER DAY: %.4f steps",
                      filled.mean.total.steps)))
print(noquote(sprintf("THE MEDIAN OF TOTAL STEPS PER DAY: %.4f steps",
                      filled.median.total.steps)))
print(noquote(sprintf("THE DIFFERENCE BETWEEN THE TWO IS: %.4f step(s)",
                      filled.diff)))

# step11: compare filled data to original data
median_comp <- abs(filled.median.total.steps - median.total.steps)
mean_comp <- abs(filled.mean.total.steps - mean.total.steps)
print(noquote(sprintf("THE ORIGINAL MEAN AND THE FILLED MEAN DIFFER BY: %.4f step(s)",
                      mean_comp)))
print(noquote(sprintf("THE ORIGINAL MEDIAN AND THE FILLED MEDIAN DIFFER BY: %.4f step(s)",
                      median_comp)))

# step12: are the differences in activity patterns between
# weekday and weekends?
activity_DT2[,days:=weekdays(datetimes,abbreviate=T)]
activity_DT2$type <- factor(activity_DT2[,days] %in% c("Sat","Sun"))
levels(activity_DT2$type)[levels(activity_DT2$type)=="TRUE"] <- "weekend"
levels(activity_DT2$type)[levels(activity_DT2$type)=="FALSE"] <- "weekday"

interval_DT2 <- data.table(aggregate(steps ~ interval + type,
                                     data=activity_DT2,
                                     mean,
                                     na.rm=T)
)

print(xyplot(steps ~ interval | as.factor(type),
             data = interval_DT2,
             layout=c(1,2),
             type="l",
             xlab="Interval",
             ylab="Average Steps",
             main="Average Steps / Time Interval",
             col="purple")
)

## Cleanup
rm(activity_DT,
   activity_DT2,
   filled_DT,
   interval_DT,
   interval_DT2,
   trim_DT,
   datetimes,
   diff,
   fill,
   filled.diff,
   filled.mean.total.steps,
   filled.median.total.steps,
   max,
   mean_comp,
   mean.total.steps
   median_comp,
   pkg,
   pkgs,
   times,
   url
)