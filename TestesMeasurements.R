#Testes Measurements; Individuals; Photo Score; CV; Linear Model
#Last Updated 2023-20-12
#This script uses the "Testes.csv" file

library(lme4)

dat<-read.csv("Testes.csv")

#Shapiro-Wilks Test of Normality for Testes Size-Pass
IndividualAverages<-
  aggregate(dat$Testes.Size, list(dat$Individual), FUN=mean)
shapiro.test(IndividualAverages$x)
boxplot(Testes.Size~Individual, data=dat)

#Test of Equal Variance-Fail
bartlett.test(Testes.Size ~ Individual, data = dat)

#ANOVA showing relationship between testes size and individual
fit2<-kruskal.test(Testes.Size~Individual,data=dat)
fit<-aov(Testes.Size~Individual,data=dat)
fit2
summary(fit)
View(fit)



windowsFonts(font=windowsFont("Posterama"))

library(tidyverse)
library(ggplot2)
library(ggbeeswarm)


#Plot displaying testes size variation among individuals, while also showing
#between and with-in photo measurements

windowsFonts(Times=windowsFont("Courier"))
individualbreaks<- seq(1, 18, by = 1)
testesbreaks<-seq(15,60, by=5)
dat$Photo <- as.factor(dat$Photo)

ggplot(data=dat) +
  aes(y = Testes.Size, x = Individual, color=Photo)+
  geom_beeswarm(cex=1.5)+
  ylab(bquote('Testes Size '(cm^3)))+
  scale_colour_manual(values = c("#d73027", "#af8dc3", "#5ab4ac", "#8c510a"))+
  theme_classic()+
  scale_x_continuous(breaks = individualbreaks)+
  scale_y_continuous(breaks = testesbreaks)+
  theme(legend.position=c(.75,.08),
        legend.direction="horizontal")+
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 4,
    shape = 95,
  )


#Plot displaying relationship between photo score and within photo CV
    
photoscorebreaks<- seq(3, 5, by = 1)
CVbreaks<-seq(0,10, by=1)

ggplot(data=dat)+
  aes(y=Within.photo.CV, x=Photo.score) +
  geom_point(shape=19, size=2.5)+
  labs(y="Within-Photo CV (%)",x="Photo Score")+
  theme_classic()+
  scale_x_continuous(breaks = photoscorebreaks)+
  scale_y_continuous(breaks = CVbreaks)+
  theme(plot.margin = margin(10, 150, 10, 150))

#T Test comparing our PLP measurements with Kelaita hand measurements
sum(IndividualAverages$x)/18
t.test(IndividualAverages$x, mu=22.66)
wilcox.test(IndividualAverages$x, mu=22.66)

#Simple Linear Model between Within-Photo CV and Photo Quality
model<-lm(dat$Within.photo.CV ~ dat$Photo.score)
summary(model)

dat2<-na.omit(dat)
library(ggplot2)
ggplot(dat2, aes(Photo.score, Within.photo.CV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Photo Score",
       y = "Within-Photo CV (%)")+
  theme_classic()+
  scale_x_continuous(breaks = seq(floor(min(dat2$Photo.score)), ceiling(max(dat2$Photo.score)), by = 1)) 
