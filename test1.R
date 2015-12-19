
Loading and preprocessing the data
activity<-read.table("activity.csv", header=TRUE, sep=",", as.is=TRUE)

act2<-activity[!is.na(activity$steps),]


act2 %>% 
  group_by(date) %>% summarise (sum=sum(steps)) -> a3

a<-ggplot(a3,aes(totalsteps))+geom_histogram()
a


act2 %>% 
  group_by(date) %>% summarise (mean=mean(steps)) -> a2


act2 %>% 
  group_by(interval) %>% summarise (mean=mean(steps)) -> a4
b<-ggplot(a4,aes(interval,mean))+geom_line()
b

a4[which.max(a4$mean),]$interval
  
sum(is.na(activity$steps))


vectorNA<-is.na(activity$steps)

activity2<-activity

activity2[vectorNA,1]<-act3[vectorNA,4]

activity2 %>% 
  group_by(date) %>% summarise (sum=sum(steps)) -> a3b

ab<-ggplot(a3b,aes(sum))+geom_histogram()
ab


activity2[vectorNA,3]

act3<-left_join(activity2,a4)





What is mean total number of steps taken per day?

totalsteps<-tapply(act2$steps,act2$date,sum)

total2<-as.data.frame(totalsteps)
total2$X<-as.Date(row.names(total2))

barplot(total2$X,total2$totalsteps)


p<-ggplot(total2, aes(X,totalsteps))+geom_histogram()


p<-ggplot(total2, aes(X,totalsteps))+geom_bar(stat="identity")
+
  scale_x_date()
p

a<-ggplot(total2,aes(totalsteps))+geom_histogram()
a

mean(total2$totalsteps)
median(total2$totalsteps)

mean(a3b$sum)
median(a3$sum)


mean(activity$steps, na.rm=TRUE)

activity2b<-activity2

activity2b$date<-as.Date(activity2$date)

library(lubridate)

week(activity2b$date)
