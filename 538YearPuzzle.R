i=0
d=0
m=0
y=0
ctr=0 ##Tracking Counter
Month=c(1:1000)
Date=c(1:1000)
Year=c(1:1000)
for (y in 1:99){
  for (m in 1:12){
    for (d in 1:31){
      if (y==m*d){
        ctr=ctr+1
        Month[ctr]=m #Month Array
        Date[ctr]=d #Date Array
        Year[ctr]=y #Year Array
      }
    }
  }
}
Indicator = rep(T,ctr) #Indicator to remove cases where dates don't exist
for (i in 1:ctr){
  if (Month[i] %in% c(4,6,9,11)&&Date[i]==31){ #Only 30 days in these months
    Indicator[i]=F
  }
  if (Month[i]==2&&Date[i]>29){ #Maximum of only 29 days in Feb
    Indicator[i]=F
  }
  if(Month[i]==2&&Date[i]==29&&Year[i]%%4!=0){ #Only leap years have 29 day  Februaries
    Indicator[i]=F
  }
}
summary(Indicator)  
DF=data.frame(Date,Month,Year) #Combine To Data Frame
DF=DF[1:ctr,]
DF=DF[Indicator,] #Remove Aberrant Values
View(DF)
sort(table(DF$Year))
