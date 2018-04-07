i=0
d=0
m=0
y=0
ctr=0
Month=c(1:1000)
Date=c(1:1000)
Year=c(1:1000)
for (y in 1:99){
  for (m in 1:12){
    for (d in 1:31){
      if (y==m*d){
        ctr=ctr+1
        Month[ctr]=m
        Date[ctr]=d
        Year[ctr]=y
      }
    }
  }
}
Indicator = rep(T,ctr)
for (i in 1:ctr){
  if (Month[i] %in% c(4,6,9,11)&&Date[i]==31){
    Indicator[i]=F
  }
  if (Month[i]==2&&Date[i]>29){
    Indicator[i]=F
  }
  if(Month[i]==2&&Date[i]==29&&Year[i]%%4!=0){
    Indicator[i]=F
  }
}
summary(Indicator)  
DF=data.frame(Date,Month,Year)
DF=DF[1:ctr,]
DF=DF[Indicator,]
View(DF)
sort(table(DF$Year))
DF[215,]
