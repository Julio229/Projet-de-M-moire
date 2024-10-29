rm(list = ls())
library(xts)
library(hyfo)
library(readxl)
library(hydroTSM)
require(hydroTSM) #analyse hydrologique
require(ggplot2) #grammaire de graphiques
library(zoo)
require(zoo)
library(scales)
library(lubridate)
require(lubridate)
library(reshape2)
require(reshape2)
require(dplyr)
require(trend)
library(tidyr)
library(tidyverse)
library(Metrics)

ETP_futur= read_excel("C:/Users/USER/Downloads/Pour Jules/ETP_2011_2100.xlsx")
ETP_futur
ETP_futur.xts1<- xts(ETP_futur[,-1],seq.Date(as.Date("2011-01-01"),
                                             as.Date("2100-12-31"),"day"))
Precip_HIRHAM <- read_excel("C:/Users/USER/Downloads/Pour Jules/Precipitations HIRHAM.xlsx")

Pluie.mensuel=tapply(as.numeric(ETP_futur$ETP), list(year(seq.Date(as.Date("2011-01-01"),
                                                                   as.Date("2100-12-31"),"day")),month(seq.Date(as.Date("2011-01-01"),as.Date("2100-12-31"),"day"))),sum,na.rm=T)
date3 <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2040-12-31"), by = "day")
length(date3)

ETP_2011_2040=ETP_futur$ETP[1:10958]
ETP_2011_2040
Precip_RCP4.5=Precip_HIRHAM$`Scénario RCP4.5`[1:10958]
ETP_2011=data.frame(date3,ETP_2011_2040)
Long <- ETP_2011 %>% group_by(Jour=format(date3,format="%m%d")) %>%
  summarise(ETP1=mean(ETP_2011$ETP_2011_2040),.groups='drop')%>%
  as.data.frame()
Long

Precip_4.5=data.frame(date3,Precip_RCP4.5)
Long1 <- Precip_4.5 %>% group_by(Jour=format(date3,format="%m%d")) %>%
  summarise(Prec.4.5=mean(Precip_4.5$Precip_RCP4.5),.groups='drop')%>%
  as.data.frame()
Long1




Débit <- read_excel("C:/Memoire/Données Objectifs Spécifiques 1/Pour Jules/Pour OS2/Débit.xlsx")

long <- Débit %>%
  pivot_longer(
    cols = paste0(1961:1990), names_to = "Date",
    values_to = "Débit",
    values_drop_na = TRUE)
long
long1<-long[order(long$Date,decreasing=F),]
long1


dates <- seq.Date(from = as.Date("1961-01-01"), to = as.Date("1991-01-23"), by = "day")
dates <- dates[1:10980]
debit=long1$Débit
Debit0=data.frame(dates,debit)

Date <- seq.Date(from = as.Date("1961-01-01"), to = as.Date("1991-01-23"), by = "day")
moy.debit.an=data.frame(Date,debit)
ggplot(moy.debit.an) + aes( x = Date, y = as.numeric(moy.debit.an$debit) )+labs( x=NULL, y="Debit" ) + geom_line()

Donnée_Pluie_ETP <- read_excel("C:/Memoire/Données Objectifs Spécifiques 1/Pour Jules/Pour OS2/Données+Pluie_ETP.xlsx")
Pluie_ETP=data.frame(Donnée_Pluie_ETP)
Pluie_ETP
PET=ETP_futur$ETP[1:10958]
Precip= Precip_HIRHAM$`Scénario RCP4.5`[1:10958]

Mu=0.9689
LANDA=15.365
TX=0.038
SF=0.016
P2=0.62


effective_rainfall <- function(Precip,PET){
  q<-rep(NA, 10958)
  n=length(Precip)
  for (i in 1:n) {
    if(Precip[i]-PET[i]>0){
      q[i]=Precip[i]-PET[i]
    }
    else{
      q[i]=0
    }
  }
  return(q)
}
q

Rsquare <- function(x,y,p){
  Ymeasure=y
  Ycalculate=(p[1]*x)+p[2]
  meanY=mean(Ymeasure)
  deltaY2=sum((Ycalculate-Ymeasure)^2)
  distanceY2=sum((Ymeasure-meanY)^2)
  r2=1-(deltaY2/distanceY2)
  return(r2)
}

state_soil <- function(MU,LANDA,q,P2){
  X<-rep(0, 10958)
  X[1]=0
  X[2]=0
  X[3]=0
  n=length(q)
  for (i in 4:n) {
    if (q[i]==0){
      X[i]=X[i-1]-MU/LANDA*X[i-1]
    }else{
      X[i]=X[i-1]+MU/LANDA*(q[i])^(2*MU-P2)
    }
  }
  return(X)
}

nash <- function(fobs,fcal){
  N=364
  errcal=numeric(N+1)
  errobs=numeric(N+1)
  errcal_som=numeric(N+1)
  errobs_som=numeric(N+1)
  
  for (t in 1:N+1) {
    errcal[t]=(fobs[t]-fcal[t])^2
    errobs[t]=(fobs[t]-mean(fobs))^2
  }
  
  for (t in 1:N){
    errcal_som[1]=errcal[1]
    errcal_som[t+1]=errcal_som[t]+errcal[t+1]
    
    errobs_som[1]=errobs[1];
    errobs_som[t+1]=errobs_som[t]+errobs[t+1]
  }
  
  Nash=1-(errcal_som[N+1])/(errobs_som[N+1])
  
  return(Nash)
}

debit=Debit0$debit
debit <- debit[1:10958]
HyMoLAP <- function(Parameters,Data,P2){
  
  Data=data.frame(Precip,PET,debit)
  Precip = Data[,1]
  PET = Data[,2]
  Qobs = Data[,3]
  Parameters=c(0.9689,15.365,0.038)
  MU = Parameters[1]
  LANDA = Parameters[2]
  TX = Parameters[3]
  SF=0.016
  P2=0.62
  
  q1 = effective_rainfall(Precip, PET)
  X = state_soil(MU,LANDA,q,P2)
  
  X[1]=0
  X[2]=0
  X[3]=0
  
  
  for (i in 4:length(Precip)) {
    if (X[i]*SF>TX){
      X[i]=X[i]
    } else {
      X[i]=0
    }
  }
  
  q = effective_rainfall(Precip, PET)
  
  X = state_soil(MU,LANDA,q,P2)
  X[1]=0
  X[2]=0
  X[3]=0
  
  Qsim<-rep(NA, 10958)
  
  Qsim[1]=Qobs[1]
  Qsim[2]=Qobs[2]
  
  
  for (i in 3:length(Precip)) {
    Qsim[i]=Qsim[i-1]-MU/LANDA*(Qsim[i-1])^(2*MU-1)+X[i]*q[i-1]/LANDA
    
    if (Qsim[i]<=0){
      Qsim[i]=0
    } else {
      Qsim[i]=Qsim[i]
    }
  }
  
  return(Qsim)
}
Qsim=HyMoLAP(Parameters = Parameters,Data=Data,P2=P2)
Data=data.frame(Precip,PET,debit)
Qobs=Data[,3]
Qobs
Qsim
Date1=Pluie_ETP$Date
Date1
table1=data.frame(Date1,Qsim,Qobs)
long1 <- table1 %>%
  pivot_longer(
    cols=c("Qsim","Qobs"),
    names_to = "Debit",
    values_to = "Value",
  )
long1

graphique1=ggplot(long1)+aes(x = Date1, y=as.numeric(Value), group=Debit, color=Debit)+
  geom_line(size=1)+ theme(axis.title.x = element_blank())+
  labs( x=NULL, y="Debit" )+
  theme(axis.title.y = element_blank())

##Periode de calage



date2 <- seq.Date(from = as.Date("1961-01-01"), to = as.Date("1966-12-31"), by = "day")
length(date2)
date2=date2[1:2191]
QobsC=Qobs[1:2191]
QsimC=Qsim[1:2191]
table2=data.frame(date2,QobsC,QsimC)
long2 <- table2 %>%
  pivot_longer(
    cols=c("QsimC","QobsC"),
    names_to = "Debit",
    values_to = "Value",
  )
long2
graphique2=ggplot(long2)+aes(x = date2, y=as.numeric(Value), group=Debit, color=Debit)+
  geom_line(size=1)+ theme(axis.title.x = element_blank())+
  labs( x=NULL, y="Debit" )+
  theme(axis.title.y = element_blank())

#Période de validation

date3 <- seq.Date(from = as.Date("1974-01-01"), to = as.Date("1979-12-31"), by = "day")
date3=date3[1:2191]
Qobsv=Qobs[4749:6939]
Qsimv=Qsim[4749:6939]
table3=data.frame(date3,Qobsv,Qsimv)
long3 <- table3 %>%
  pivot_longer(
    cols=c("Qsimv","Qobsv"),
    names_to = "Debit",
    values_to = "Value",
  )
long3
graphique3=ggplot(long3)+aes(x = date3, y=as.numeric(Value), group=Debit, color=Debit)+
  geom_line(size=1)+ theme(axis.title.x = element_blank())+
  labs( x=NULL, y="Debit" )+
  theme(axis.title.y = element_blank())

table3=data.frame(date3,Qobsv,Qsimv)
long4 <- table3 %>% group_by(Jour=format(date3,format="%m%d")) %>%
  summarise(Qobsv=mean(Qobsv),Qsimv=mean(Qsimv),.groups='drop')%>%
  as.data.frame()
long4
long5 <- long4 %>%
  pivot_longer(
    cols=c("Qobsv","Qsimv"),
    names_to = "Debit",
    values_to = "Value",
  )
long5

graphique4=ggplot(long5)+aes(x=Jour, y=as.numeric(Value), group=Debit, color=Debit)+
  geom_line(size=1)+ theme(axis.title.x = element_blank())+
  labs( x=NULL, y="Debit" )+
  theme(axis.title.y = element_blank())+ ggtitle(
    "Bétérou Validation 1961-1966"
  )+scale_x_discrete(breaks=c(0,100,200,300))


table2=data.frame(date2,QobsC,QsimC)
long6 <- table2 %>% group_by(Jour1=format(date2,format="%m%d")) %>%
  summarise(Qobsc=mean(QobsC),Qsimc=mean(QsimC),.groups='drop')%>%
  as.data.frame()
long6
long7 <- long6 %>%
  pivot_longer(
    cols=c("Qobsc","Qsimc"),
    names_to = "Debit",
    values_to = "Value",
  )
long7

graphique5=ggplot(long7)+aes(x = Jour1, y=as.numeric(Value), group=Debit, color=Debit)+
  geom_line(size=1)+ theme(axis.title.x = element_blank())+
  labs( x=NULL, y="Debit" )+
  theme(axis.title.y = element_blank())+  ggtitle(
    "Bétérou Calibration 1961-1966"
  )+scale_x_discrete(breaks=c(0,100,200,300))

