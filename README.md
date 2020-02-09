# Course5Week2Project
setwd("C:/Users/Emma/Desktop/R/Course5/Week2/Project")
## set directory

zipfile <- "repdata_data_activity.zip"
unzip(zipfile,files=NULL,list=FALSE,overwrite=TRUE,junkpaths=FALSE,exdir=".",unzip="internal",setTimes=FALSE)
## unzip file to the folder

file <- read.csv("./activity.csv", header=TRUE)
## read file


## stepsum <- file%>%group_by(date)%>% summarize(sum(steps,na.rm=TRUE))
stepsum <- aggregate(file$steps, by=list(file$date), sum)
names(stepsum)[2] <- paste("steps_sum")
## crerate a file with sum of steps per day & give easy names to columns


hist(stepsum$steps_sum,main="Histogram of Total Steps Per Day",xlab="Total Steps Per Day")
## create histogram to the total steps per day

summary(stepsum)
## get the mean & median information

NoMissing <- file[!is.na(file$steps),]
## create file with no missing values

## stepmean <-  NoMissing%>%group_by(interval)%>% summarise(mean(steps))
stepmean <- aggregate(NoMissing$steps, by=list(NoMissing$interval), mean)
names(stepmean)[2]<-paste("meansteps")
names(stepmean)[1]<-paste("interval")
library(ggplot2)
ggplot(stepmean,aes(x=factor(interval),y=stepmean$meansteps))+geom_point(lwd=1)+labs(x="interval",y="steps",title="Time series plot of the average number of steps taken")

## Q5
stepmean[stepmean[2]==max(stepmean[2]),]

## Q6
missing <- file[is.na(file$steps),]

com <- merge (missing,stepmean, by="interval")
com <- com[order(com$date), ]
reorder<-com[,c(4,3,1)]
View(reorder)
names(reorder)[1] <- paste("steps")
View(reorder)

final <- rbind(NoMissing,reorder)
final <- final[order(final$date),]
View(final)

## Q7
final$date <- as.Date(final$date)
finalsum <- aggregate(final$steps, by=list(final$date), sum)
finalinterval <- aggregate(final$interval, by=list(final$date), sum)
names(finalsum)[2] <- paste("steps_sum")
View(finalsum)
View(finalinterval)
hist(finalsum$steps_sum,main="Histogram of Total Steps Per Day",xlab="Total Steps Per Day")

## Q8

final["WW"] <- weekdays(as.Date(final$date))
final$WW[final$WW %in% c('Saturday','Sunday')] <- "weekend"
final$WW[final$WW!="weekend"] <- "weekday"
View(final)
ggplot(final,aes(x=factor(interval),y=final$steps))+facet_grid(.~WW)+geom_line()+labs(x="interval",y="steps",title="Weekdays VS Weekends Steps")

git remote add originhttps://github.com/emmayanyan/Course5Week2Project.git
