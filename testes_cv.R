#Between and Within Photo CV Scores
#Last updated 2023-24-1
#This analysis uses the "RawTestes.csv" file

#Examining Volumes
tests<- read.csv("RawTestes.csv")

library(tidyverse)
library(magrittr)

t_sum<- tests %>% group_by(Individual2) %>% summarize(n_photo=max(Photo)) %>% drop_na


t_sum2<- tests %>% group_by(Individual2, Photo) %>% summarize(avg_size=mean(Total.Testes.Volume), sd=sd(Total.Testes.Volume)) %>% drop_na
t_sum2$cv<- t_sum2$sd/t_sum2$avg_size *100
mean(t_sum2$cv)
range(t_sum2$cv)

t_sum3<- t_sum2 %>% group_by(Individual2) %>% summarize(n=n(), ind_avg=mean(avg_size), sd_photos=sd(avg_size))
t_sum3$cv<- t_sum3$sd_photos/t_sum3$ind_avg *100
mean(t_sum3$cv, na.rm=T)
range(t_sum3$cv, na.rm=T)

#Examining Lengths & Widths Separately
t_sum<- tests %>% group_by(Individual2) %>% summarize(n_photo=max(Photo)) %>% drop_na

#Left Testis Width
t_sum2<- tests %>% group_by(Individual2, Photo) %>% summarize(avg_size=mean(Left.Testis.Width), sd=sd(Left.Testis.Width)) %>% drop_na
t_sum2$cv<- t_sum2$sd/t_sum2$avg_size *100
mean(t_sum2$cv)
range(t_sum2$cv)

#Left Testis Length
t_sum3<- tests %>% group_by(Individual2, Photo) %>% summarize(avg_size=mean(Left.Testis.Length), sd=sd(Left.Testis.Length)) %>% drop_na
t_sum3$cv<- t_sum3$sd/t_sum3$avg_size *100
mean(t_sum3$cv)
range(t_sum3$cv)

#Right Testis Width
t_sum4<- tests %>% group_by(Individual2, Photo) %>% summarize(avg_size=mean(Right.Testis.Width), sd=sd(Right.Testis.Width)) %>% drop_na
t_sum4$cv<- t_sum4$sd/t_sum4$avg_size *100
mean(t_sum4$cv)
range(t_sum4$cv)

#Right Testis Length
t_sum5<- tests %>% group_by(Individual2, Photo) %>% summarize(avg_size=mean(Right.Testis.Length), sd=sd(Right.Testis.Length)) %>% drop_na
t_sum5$cv<- t_sum5$sd/t_sum5$avg_size *100
mean(t_sum5$cv)
range(t_sum5$cv)