setwd("C:\\Users\\IT24103847\\Desktop\\Lab 05-20250829")
Delivery_Time<-read.table("Exercise - Lab 05.txt",header=TRUE,sep=",")
fix(Delivery_Time)
names(Delivery_Time)<-c("X1")
fix(Delivery_Time)
histogram<-hist(Delivery_Time$X1,main="Histogram of Delivary time",breaks =seq(20,70,length=10),right = FALSE,xlab="Delivary time",ylab="Frequency")
#3add comments
breaks<-round(histogram$breaks)
breaks
freq<-histogram$counts
freq
mids<-histogram$mids
mids

classes<-c()
for (i in 1:length(breaks)-1){
  classes[i]<-paste0("[",breaks[i],",",breaks[i+1],")")
  
  
}
cbind(classes=classes,frequency=freq)
lines(mids,freq)
plot(mids,freq,type='l',main="Frequancy polygon for delivary time",xlab="Delivery Time")
cum.freq<-cumsum(freq)
new<-c()
for(i in 1 : length(breaks))
{
  if(i==1)
  {
    new[i]=0
  }else{
    new[i]=cum.freq[i-1]
  }
}
plot(breaks,new,type='l',main = "Cumilative Frequency Polygon for Delivery time",
     xlab ="Delivery time",ylab="Frequancy",ylim=c(0,max(cum.freq)))
cbind(Upper=breaks,cum.freq=new)