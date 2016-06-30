library(car)
setwd("C:/Users/Joshua/Dropbox/University of Arizona/Projects/Evangelical Voting Project/Experiment/Data")

data<-read.csv("recoded.csv")

data$god<-as.factor(data$god) #1=believe in God or higher power
data$relimp<-6-data$relimp #1=very unimportant, 5=very important
data$denom<-as.factor(data$denom) #see codebook
data$trad<-as.factor(data$trad) #1=Fundamentalist/Charismatic, 2=Traditional/Conservative, 3=Mainline/Moderate, 4=Progressive/Liberal, 5=Not Religious, 6=Pentecostal/Evangelical, 7=Other Religion
data$biblit<-recode(as.numeric(data$biblit), "1=4; 2=3; 3=2; 5=1; else=NA") #1=book of fables, 2=still has value, 3=God's word not literal, 4=God's word and literal
data$jesus<-6-data$jesus #reverse-coding
data$jesus[data$jesus==0]<-NA #1=I do not believe Jesus Christ is the Son of God, 2 = not certain at all, 5 = absolutely certain
data$born<-as.factor(data$born)
data$race1<-as.factor(data$race1)
data$race2<-as.factor(data$race2)
data$race3<-as.factor(data$race3)
data$race4<-as.factor(data$race4)
data$race5<-as.factor(data$race5)
data$race6<-as.factor(data$race6)
data$race7<-as.factor(data$race7)
data$native<-as.factor(data$native)
data$birth<-as.character(data$birth)
data$english<-as.factor(data$english)

save(data, file="recoded.RData")
