mydata = read.csv(file="C:/Users/andy/Documents/CS+X/CS_X/week_3/hw_3/vocabulary.csv")  # read csv file 
library(ggplot2)
theme_set(theme_classic())
gg = ggplot(mydata, aes(x=as.numeric(Index),y=as.numeric(TrainVideoCount),color=Name))+
  geom_point() + 
  #geom_smooth()
  xlim(c(0, 4000)) + 
  ylim(c(0, 2500)) + 
  labs(subtitle="Index to Vids", 
       y="TrainVideoCount", 
       x="Index", 
       title="text", 
       caption = "Source: google")

plot(gg)

