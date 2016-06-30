library(car)

setwd("C:/Users/Joshua/Dropbox/University of Arizona/Projects/Evangelical Voting Project/Experiment/Data")

data<-read.csv("recoded.csv")

data$treatlib<-NA
data$treatcon<-NA
data$treatlib[data$manip1==4]<-1
data$treatcon[data$manip2==4]<-2
data$god<-as.factor(data$god) #1=believe in God or higher power
data$relimp<-6-data$relimp #1=very unimportant, 5=very important
data$denom<-as.factor(data$denom)
data$trad<-as.factor(data$trad)
data$biblit<-recode(as.numeric(data$biblit), "1=4; 2=3; 3=2; 5=1; else=NA") #1=book of fables, 2=still has value, 3=God's word not literal, 4=God's word and literal
data$jesus<-6-data$jesus
data$jesus[data$jesus==0]<-NA
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

rm(list=ls())
setwd("C:/Users/Joshua/Dropbox/University of Arizona/Projects/Evangelical Voting Project/Experiment/Data")
load("recoded.RData")
constim<-subset(data, data$manip1==4)
libstim<-subset(data, data$manip2==4)

library(psych)

########################################################################################################################################################################

##### Test for Partisan Polarization Effect #####

#Religious Liberals
t.test(libstim$pid[libstim$trad==4], constim$pid[constim$trad==4])

#Religious Conservatives
t.test(libstim$pid[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$pid[constim$trad==1 | constim$trad==2 | constim$trad==6])
#data:  libstim$pid[libstim$trad == 1 | libstim$trad == 2 | libstim$trad ==  and constim$pid[constim$trad == 1 | constim$trad == 2 | constim$trad ==     6] and     6]
#t = 2.6664, df = 92.998, p-value = 0.009043
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.2243385 1.5335329
#sample estimates:
#  mean of x mean of y 
#5.024390  4.145455 









###### WHY IS THIS NEXT LINE DIFFERENT? IS IT LOOKING FOR PEOPLE WHO ARE [[BOTH]] IN TRAD CATEGORY 1 AND 2? ######

#t.test(libstim$pid[libstim$trad==1:2 | libstim$trad==6], constim$pid[constim$trad==1:2 | constim$trad==6])
# data:  libstim$pid[libstim$trad == 1:2 | libstim$trad == 6] and constim$pid[constim$trad == 1:2 | constim$trad == 6]
# t = 2.4835, df = 53.942, p-value = 0.01615
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1952516 1.8310988
# sample estimates:
#   mean of x mean of y 
# 5.043478  4.030303 

##### Test for Ideological Polarization Effect #####

#Religious Liberals
t.test(libstim$ideo[libstim$trad==4], constim$ideo[constim$trad==4])

#Religious Conservatives
t.test(libstim$ideo[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ideo[constim$trad==1 | constim$trad==2 | constim$trad==6])
#data:  libstim$ideo[libstim$trad == 1 | libstim$trad == 2 | libstim$trad ==  and constim$ideo[constim$trad == 1 | constim$trad == 2 | constim$trad ==     6] and     6]
#t = 2.0186, df = 93.166, p-value = 0.0464
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.01097083 1.33803139
#sample estimates:
#  mean of x mean of y 
#5.292683  4.618182 




#t.test(libstim$ideo[libstim$trad==1:2 | libstim$trad==6], constim$ideo[constim$trad==1:2 | constim$trad==6])
# data:  libstim$ideo[libstim$trad == 1:2 | libstim$trad == 6] and constim$ideo[constim$trad == 1:2 | constim$trad == 6]
# t = 2.6967, df = 54, p-value = 0.00932
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.3025268 2.0558394
# sample estimates:
#   mean of x mean of y 
# 5.391304  4.212121


########################################################################################################################################################################


##### Test for Affective Polarization Among Religious Liberals #####

#Democrats
t.test(libstim$ftdem[libstim$trad==4], constim$ftdem[constim$trad==4])

#Republicans
t.test(libstim$ftrep[libstim$trad==4], constim$ftrep[constim$trad==4])

#Liberals
t.test(libstim$ftlib[libstim$trad==4], constim$ftlib[constim$trad==4])

#Conservatives
t.test(libstim$ftcon[libstim$trad==4], constim$ftcon[constim$trad==4])

#Independents
t.test(libstim$ftind[libstim$trad==4], constim$ftind[constim$trad==4])

#Theists
t.test(libstim$fttheist[libstim$trad==4], constim$fttheist[constim$trad==4])

#Atheists
t.test(libstim$ftathe[libstim$trad==4], constim$ftathe[constim$trad==4])

#The Religious Right
t.test(libstim$ftrr[libstim$trad==4], constim$ftrr[constim$trad==4])
# data:  libstim$ftrr[libstim$trad == 4] and constim$ftrr[constim$trad == 4]
# t = 2.2465, df = 43.095, p-value = 0.02985
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1706775 3.1649747
# sample estimates:
#   mean of x mean of y 
# 3.320000  1.652174 

#Pro-Lifers
t.test(libstim$ftlife[libstim$trad==4], constim$ftlife[constim$trad==4])

#Gays
t.test(libstim$ftgays[libstim$trad==4], constim$ftgays[constim$trad==4])

#Christians
t.test(libstim$ftchristi[libstim$trad==4], constim$ftchristi[constim$trad==4])

#Transgender People
t.test(libstim$fttrans[libstim$trad==4], constim$fttrans[constim$trad==4])

#Pro-Choicers
t.test(libstim$ftchoice[libstim$trad==4], constim$ftchoice[constim$trad==4])



##### Test for Affective Polarization Among Religious Liberals #####

#Democrats
t.test(libstim$ftdem[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftdem[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Republicans
t.test(libstim$ftrep[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftrep[constim$trad==1 | constim$trad==2 | constim$trad==6])
#data:  libstim$ftrep[libstim$trad == 1 | libstim$trad == 2 | libstim$trad ==  and constim$ftrep[constim$trad == 1 | constim$trad == 2 | constim$trad ==     6] and     6]
#t = 1.7912, df = 88.119, p-value = 0.07669
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1205061  2.3227234
#sample estimates:
#  mean of x mean of y 
#5.682927  4.581818 

#Liberals
t.test(libstim$ftlib[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftlib[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Conservatives
t.test(libstim$ftcon[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftcon[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Independents
t.test(libstim$ftind[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftind[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Theists
t.test(libstim$fttheist[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$fttheist[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Atheists
t.test(libstim$ftathe[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftathe[constim$trad==1 | constim$trad==2 | constim$trad==6])

#The Religious Right
t.test(libstim$ftrr[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftrr[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Pro-Lifers
t.test(libstim$ftlife[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftlife[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Gays
t.test(libstim$ftgays[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftgays[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Christians
t.test(libstim$ftchristi[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftchristi[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Transgender People
t.test(libstim$fttrans[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$fttrans[constim$trad==1 | constim$trad==2 | constim$trad==6])

#Pro-Choicers
t.test(libstim$ftchoice[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$ftchoice[constim$trad==1 | constim$trad==2 | constim$trad==6])


########################################################################################################################################################################


##### Test for Social Distance Effect Among Religious Liberals #####

#Republican
t.test(libstim$sdrep[libstim$trad==4], constim$sdrep[constim$trad==4])

#Democrat
t.test(libstim$sddem[libstim$trad==4], constim$sddem[constim$trad==4])


#Liberal
t.test(libstim$sdlib[libstim$trad==4], constim$sdlib[constim$trad==4])


#Conservative
t.test(libstim$sdcon[libstim$trad==4], constim$sdcon[constim$trad==4])


#Feminist
t.test(libstim$sdfem[libstim$trad==4], constim$sdfem[constim$trad==4])
# data:  libstim$sdfem[libstim$trad == 4] and constim$sdfem[constim$trad == 4]
# t = 1.9313, df = 43.86, p-value = 0.05992
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01926227  0.90274053
# sample estimates:
#   mean of x mean of y 
# 3.920000  3.478261 

#Pro-Lifer
t.test(libstim$sdlife[libstim$trad==4], constim$sdlife[constim$trad==4])


#Pro-Choicer
t.test(libstim$sdchoice[libstim$trad==4], constim$sdchoice[constim$trad==4])
# data:  libstim$sdchoice[libstim$trad == 4] and constim$sdchoice[constim$trad == 4]
# t = 1.9875, df = 45.952, p-value = 0.05285
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.006178335  0.969656596
# sample estimates:
#   mean of x mean of y 
# 3.960000  3.478261 

#Christian
t.test(libstim$sdchristi[libstim$trad==4], constim$sdchristi[constim$trad==4])
# data:  libstim$sdchristi[libstim$trad == 4] and constim$sdchristi[constim$trad == 4]
# t = 1.9769, df = 41.043, p-value = 0.05479
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01025453  0.96329801
# sample estimates:
#   mean of x mean of y 
# 3.520000  3.043478 

#Atheist
t.test(libstim$sdathe[libstim$trad==4], constim$sdathe[constim$trad==4])

#Religious Right
t.test(libstim$sdrr[libstim$trad==4], constim$sdrr[constim$trad==4])



##### Test for Social Distance Effect Among Religious Conservatives #####

#Republican
t.test(libstim$sdrep[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdrep[constim$trad==1 | constim$trad==2 | constim$trad==6])
# data:  libstim$sdrep[libstim$trad == 1 | libstim$trad == 2 | libstim$trad ==  and constim$sdrep[constim$trad == 1 | constim$trad == 2 | constim$trad ==     6] and     6]
# t = 1.7285, df = 88.386, p-value = 0.08738
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05679684  0.81599861
# sample estimates:
#   mean of x mean of y 
# 3.634146  3.254545 

#Democrat
t.test(libstim$sddem[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sddem[constim$trad==1 | constim$trad==2 | constim$trad==6])
# data:  libstim$sddem[libstim$trad == 1 | libstim$trad == 2 | libstim$trad ==  and constim$sddem[constim$trad == 1 | constim$trad == 2 | constim$trad ==     6] and     6]
# t = -1.8523, df = 92.904, p-value = 0.06716
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.76911788  0.02676755
# sample estimates:
#   mean of x mean of y 
# 2.756098  3.127273 

#Liberal
t.test(libstim$sdlib[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdlib[constim$trad==1 | constim$trad==2 | constim$trad==6])


#Conservative
t.test(libstim$sdcon[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdcon[constim$trad==1 | constim$trad==2 | constim$trad==6])


#Feminist
t.test(libstim$sdfem[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdfem[constim$trad==1 | constim$trad==2 | constim$trad==6])


#Pro-Lifer
t.test(libstim$sdlife[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdlife[constim$trad==1 | constim$trad==2 | constim$trad==6])


#Pro-Choicer
t.test(libstim$sdchoice[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdchoice[constim$trad==1 | constim$trad==2 | constim$trad==6])


#Christian
t.test(libstim$sdchristi[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdchristi[constim$trad==1 | constim$trad==2 | constim$trad==6])


#Atheist
t.test(libstim$sdathe[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdathe[constim$trad==1 | constim$trad==2 | constim$trad==6])


#Religious Right
t.test(libstim$sdrr[libstim$trad==1 | libstim$trad==2 | libstim$trad==6], constim$sdrr[constim$trad==1 | constim$trad==2 | constim$trad==6])


########################################################################################################################################################################

t.test(libstim$pid[libstim$trad==1], constim$pid[constim$trad==1])
# data:  libstim$pid[libstim$trad == 1] and constim$pid[constim$trad == 1]
# t = 2, df = 7.7143, p-value = 0.08184
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3209554  4.3209554
# sample estimates:
#   mean of x mean of y 
#           5         3 

t.test(libstim$ideo[libstim$trad==1], constim$ideo[constim$trad==1])
# data:  libstim$ideo[libstim$trad == 1] and constim$ideo[constim$trad == 1]
# t = 2.0887, df = 7.5797, p-value = 0.07208
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2077362  3.8267839
# sample estimates:
#   mean of x mean of y 
# 5.666667  3.857143

t.test(libstim$pid[libstim$trad==2], constim$pid[constim$trad==2])

t.test(libstim$ideo[libstim$trad==2], constim$ideo[constim$trad==2])

t.test(libstim$pid[libstim$trad==3], constim$pid[constim$trad==3])

t.test(libstim$ideo[libstim$trad==3], constim$ideo[constim$trad==3])

t.test(libstim$pid[libstim$trad==4], constim$pid[constim$trad==4])

t.test(libstim$ideo[libstim$trad==4], constim$ideo[constim$trad==4])

t.test(libstim$pid[libstim$trad==5], constim$pid[constim$trad==5])

t.test(libstim$ideo[libstim$trad==5], constim$ideo[constim$trad==5])

t.test(libstim$pid[libstim$trad==6], constim$pid[constim$trad==6])

t.test(libstim$ideo[libstim$trad==6], constim$ideo[constim$trad==6])
# data:  libstim$ideo[libstim$trad == 6] and constim$ideo[constim$trad == 6]
# t = 2.2947, df = 13.923, p-value = 0.03782
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1011607 3.0197185
# sample estimates:
#   mean of x mean of y 
# 4.714286  3.153846 


########################################################################################################################################################################

##### TEST OF POLARIZATION EFFECTS FOR THOSE WITH VARYING CERTAINTIES IN JESUS' DIVINITY

