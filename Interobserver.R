#Interobserver Reliability of Testes Measurements 
#12 May 2023
#This script uses the "InterobserverReliability.csv" file

#Packages
library(ggplot2)
library(irr)
library(psych)

df<-read.csv("InterobserverReliability.csv")

#Excluding outliers
df2<-df[-c(5,17),]

#Individual Error Rates
df$difference<-df$Measurement2-df$Measurement1
df$error<-df$difference/df$Measurement1
df$error<-abs(df$error)
df$errorpercent<-df$error*100
summary(df$errorpercent)

df2$difference<-df2$Measurement2-df2$Measurement1
df2$error<-df2$difference/df2$Measurement1
df2$error<-abs(df2$error)
df2$errorpercent<-df2$error*100
summary(df2$errorpercent)

boxplot(df$errorpercent,frame=FALSE,ylab="Error Rate Percentage",
        ylim=c(0,40))
par(mar=c(4,4,4,4))

#Overall reliability- Intraclass Correlation (ICC)
df3<-df[,-c(1,4,5,6)]
icc(df3, model = "twoway", type = "agreement", unit = "single")

df4<-df2[,-c(1,4,5,6)]
icc(df4, model = "twoway", type = "agreement", unit = "single")

#Comparison between with outliers vs excluding outliers
ICC(df3)
ICC(df4)
