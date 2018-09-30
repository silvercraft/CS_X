
mydata = head(read.csv(file="vocabulary.csv"),180)  # read csv file 
library(ggplot2)

theme_set(theme_classic())
gg = ggplot(data=mydata, aes(x=Vertical1))+
  geom_bar(aes(fill=Vertical1,colour = "white"))+
  coord_flip()+
  labs(x="catagories", 
       title="Videos catagory counts")
plot(gg)

qplot(TrainVideoCount,Vertical1,data=mydata)

gg = ggplot(data=mydata, aes(x=as.numeric(Index),y=TrainVideoCount),color=Vertical1)+
  geom_point()+
  geom_smooth(color = "red",size = 2)+
  labs(x="index(1 to 180)", y="videocount",
       title="Videos catagory counts")
plot(gg)

gg = ggplot(data=mydata, aes(x=as.numeric(Index),y=log(TrainVideoCount)))+
  geom_point(aes(col=Vertical1))+
  geom_smooth(color = "yellow",size = 1)+
  labs(x="index(1 to 180)",y="videocount in log" ,
       title="Videos catagory counts")
plot(gg)

mydata[1:180,c(1,2,6), drop=FALSE]
print("source:https://research.google.com/youtube8m/")
