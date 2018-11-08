source('myfunc/myfunc.R')
library(psych)
library(irtoys)
library(ltm)

item.res01 <-read.csv('恋愛尺度本.csv',fileEncoding="cp932")
水戸<-item.res01[1:20]
水戸02<-水戸[c(1:5,7:15,17,18)]
水戸03 <-XtoU(水戸02)
a<-grm.theta(水戸03, a=iplgx水戸[,6]/1.7, bc=iplgx水戸[,c(1,2,3,4,5)],  D=1.7, method = "ML")