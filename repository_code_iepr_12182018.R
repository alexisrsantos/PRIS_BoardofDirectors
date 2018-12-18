library(ggplot2)
library(tidyverse)
library(stringr)
library(doBy) #Library for group comparisons

#Import the data
data<-read.csv("PATH HERE/data.csv")

#Reformat variables as dates
data$Date_Appointed2<-as.Date(data$Date_Appointed2) #Make it a date
data$Date_Left2<-as.Date(data$Date_Left2) #Make it a data

#Transformation and calculation of variables
data$Initials<-factor(data$Initials,levels=data$Initials) #Make a factor
data$length<-(data$Date_Left2-data$Date_Appointed2) #Calculate length
data$length<-as.numeric(data$length) #Make it a number 

#Boxplots to see means
boxplot(length~Government_Officer,data=data) #Boxplot to see median (line)
boxplot(length~Education,data=data) #Boxplot to see median (line)
boxplot(length~Sex,data=data) #Boxplot to see median (line)

#Analysis by groups
summaryBy(length~Government_Officer,data=data,FUN=list(mean,median))
summaryBy(length~Education,data=data,FUN=list(mean,median))
summaryBy(length~Sex,data=data,FUN=list(mean,median))
dataframe<-as.data.frame(summaryBy(length~Year_Appointment,data=data,FUN=list(mean,median)))

#Import the long form dataset
data2<-read.csv("PATH HERE/data2.csv")

#Do the data transformations similar as before
data2$Date<-as.Date(data2$Date)
data2$Initials<-as.factor(data2$Initials,levels=data2$Initials)
data2$Date_Appointed<-as.Date(data2$Date_Appointed)
data2$Initials<-factor(data2$Initials,levels=data2$Initials)
data2$Government_Officer<-factor(data2$Gov_Officer)

#Make first figure
a<- ggplot(data2,aes(x=Date,y=reorder(Initials,-Rank),shape=Sex,group=Education,color=Education))+
  geom_point(size=4)+  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_line(aes(group=Initials,linetype=Gov_Officer),size=1)+  theme(legend.position="bottom")+
  geom_vline(data=data2,aes(xintercept=as.Date("2009-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2013-01-30")),colour="red",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2017-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2005-01-30")),colour="red",size=1.20)+
  labs(title = "Members of the PRIS Board of Directors", subtitle = "Education (Line color), Sex (Shape of Point), Time of Service (length of segment), and Government Representative (Segment type)", y = "Initials of Member of the Board", x ="Date",
  caption = "Produced by Alexis R. Santos-Lozada with historical records",
  linetype="Government Appointee",point="Sex",group="Education")+
  scale_x_date(date_breaks = "1 year")

a #Show the first image

#Make second figure
b<-ggplot(data2,aes(x=Date,y=reorder(Initials,-Rank),shape=Sex,group=Education,color=Education))+
  geom_point(size=4)+  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_line(aes(group=Initials,linetype=Gov_Officer),size=1)+  theme(legend.position="bottom")+
  geom_vline(data=data2,aes(xintercept=as.Date("2009-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2013-01-30")),colour="red",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2017-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2005-01-30")),colour="red",size=1.20)+
  facet_grid(Education~., scales="free", space="free")+
  theme(strip.text.y = element_text(size=8,family="sans"))+
  labs(title = "Members of the PRIS Board of Directors, by Educational Attainment", subtitle = "Education (Line color), Sex (Shape of Point), Time of Service (length of segment), and Government Representative (Segment type)", y = "Initials of Member of the Board", x ="Date",
       caption = "Produced by Alexis R. Santos-Lozada with historical records",
       linetype="Government Appointee",point="Sex",group="Education")+
  scale_x_date(date_breaks = "1 year")

b # Show figure 2

#Make third figure
c<-ggplot(data2,aes(x=Date,y=reorder(Initials,-Rank),shape=Sex,group=Education,color=Education))+
  geom_point(size=4)+  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_line(aes(group=Initials,linetype=Gov_Officer),size=1)+  theme(legend.position="bottom")+
  geom_vline(data=data2,aes(xintercept=as.Date("2009-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2013-01-30")),colour="red",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2017-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2005-01-30")),colour="red",size=1.20)+
  facet_grid(Sex~., scales="free", space="free")+
  theme(strip.text.y = element_text(size=11,family="sans"))+
  labs(title = "Members of the PRIS Board of Directors, by Sex", subtitle = "Education (Line color), Sex (Shape of Point), Time of Service (length of segment), and Government Representative (Segment type)", y = "Initials of Member of the Board", x ="Date",
       caption = "Produced by Alexis R. Santos-Lozada with historical records",
       linetype="Government Appointee",point="Sex",group="Education")+
  scale_x_date(date_breaks = "1 year")

c #Show figure 3

# Make fourth figure
d<-ggplot(data2,aes(x=Date,y=reorder(Initials,-Rank),shape=Sex,group=Education,color=Education))+
  geom_point(size=4)+  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_line(aes(group=Initials,linetype=Gov_Officer),size=1)+  theme(legend.position="bottom")+
  geom_vline(data=data2,aes(xintercept=as.Date("2009-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2013-01-30")),colour="red",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2017-01-30")),colour="blue",size=1.20)+
  geom_vline(data=data2,aes(xintercept=as.Date("2005-01-30")),colour="red",size=1.20)+
  facet_grid(Government_Officer~., scales="free", space="free")+
  theme(strip.text.y = element_text(size=11,family="sans"))+
  labs(title = "Members of the PRIS Board of Directors, by Government Appointments", subtitle = "Education (Line color), Sex (Shape of Point), Time of Service (length of segment), and Government Representative (Segment type)", y = "Initials of Member of the Board", x ="Date",
       caption = "Produced by Alexis R. Santos-Lozada with historical records",
       linetype="Government Appointee",point="Sex",group="Education")+
  scale_x_date(date_breaks = "1 year")

d #Show figure 4
