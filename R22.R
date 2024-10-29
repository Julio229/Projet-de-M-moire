#Library utilisé
rm(list = ls())
library(xts) # série temporel
require(xts)
library(tidyverse)
require(downscaleR)
library(downscaleR)
library(hydroTSM)
require(hydroTSM) #analyse hydrologique
require(ggplot2) # grammaire de graphiques
library(zoo)
require(zoo)
library(scales)
library(lubridate)
require(lubridate)
library(reshape2)
require(reshape2)
require(dplyr)
require(trend)
library(readxl)

setwd("C:/Users/user/Desktop/Jules/Code R")

data <- read_excel("HIRHAM5 historique_1961_1990.xlsx")
data

#transformation en série temporelle
data.xts<- xts(data[,-1],seq.Date(as.Date("1961-01-01"),
                                  as.Date("1990-12-31"),"day"))
data.xts
#Résumés saisonniers et annuels
#moyenne
moyenne.mensuel=apply.monthly(data.xts,mean,na.rm=T)
moyenne.mens=moyenne.mensuel
Pluie.mensuel=tapply(as.numeric(data$`Pluie non corrigée`), list(year(seq.Date(as.Date("1961-01-01"),as.Date("1990-12-31"),"day")),month(seq.Date(as.Date("1961-01-01"),as.Date("1990-12-31"),"day"))),sum,na.rm=T)
#annuel
moyenne.annuel=apply.yearly(data.xts[,-8],mean,na.rm=T)
moyenne.annuel
pluie.maxi.an=apply.yearly(data.xts$`Pluie non corrigée`,max, na.rm=T)

#manipulation des données pour les graphiques
moyenne.mensuel=as.data.frame(moyenne.mensuel)
colnames(moyenne.mensuel)=c("Pmoy")
attach(moyenne.mensuel)
moyenne.mensuel$year=year(row.names(moyenne.mensuel))
moyenne.mensuel$month=month(row.names(moyenne.mensuel))
moyenne.mensuel=reshape2::melt(data=moyenne.mensuel,na.rm=F,id.vars =c("year","month"))
moyenne.mensuel$Date=as.Date(paste(moyenne.mensuel$year,
                                   moyenne.mensuel$month,"01",sep = "-"))
moy.61.90.param=tapply(moyenne.mensuel$value,list(moyenne.mensuel$month, moyenne.mensuel$variable),mean,na.rm=T)
pluie.=apply(Pluie.mensuel,2,mean,na.rm=T)
 moy.61.90.param=cbind(moy.61.90.param,pluie.)
monthOrder <- c('janv.', 'févr.', 'mars', 'avr.', 'mai', 'juin', 'juil.',
                'août', 'sept.', 'oct.', 'nov.', 'déc.')
moyenne.mensuel$Month <- factor(format(moyenne.mensuel$Date, "%b"),
                                levels = monthOrder)

detach(moyenne.mensuel)

#Moyennes mensuelles sur 61-1990
x1=as.data.frame(cbind(Mois=month.abb[1:12],round(moy.61.90.param,2)))
x1

data1 <- read_excel("Pluie_observe_1961_1990.xlsx")
data1

#transformation en série temporelle
data1.xts<- xts(data1[,-1],seq.Date(as.Date("1961-01-01"),
                                    as.Date("1990-12-31"),"day"))
data1.xts
#Résumés saisonniers et annuels
#moyenne
moyenne.mensuel1=apply.monthly(data1.xts,mean,na.rm=T)
moyenne.mens1=moyenne.mensuel1
Pluie.mensuel1=tapply(as.numeric(data1$`Pluie observée`), list(year(seq.Date(as.Date("1961-01-01"),as.Date("1990-12-31"),"day")),month(seq.Date(as.Date("1961-01-01"),as.Date("1990-12-31"),"day"))),sum,na.rm=T)
#annuel
moyenne.annuel1=apply.yearly(data1.xts[,-8],mean,na.rm=T)
moyenne.annuel1
pluie1.maxi.an=apply.yearly(data1.xts$`Pluie observée`,max, na.rm=T)

#manipulation des données pour les graphiques
moyenne.mensuel1=as.data.frame(moyenne.mensuel1)
colnames(moyenne.mensuel1)=c("Pmoy")
attach(moyenne.mensuel1)
moyenne.mensuel1$year=year(row.names(moyenne.mensuel1))
moyenne.mensuel1$month=month(row.names(moyenne.mensuel1))
moyenne.mensuel1=reshape2::melt(data=moyenne.mensuel1,na.rm=F,id.vars =c("year","month"))
moyenne.mensuel1$Date=as.Date(paste(moyenne.mensuel1$year,
                                    moyenne.mensuel1$month,"01",sep = "-"))
moy.61.90.param1=tapply(moyenne.mensuel1$value,list(moyenne.mensuel1$month, moyenne.mensuel1$variable),mean,na.rm=T)
pluie1.=apply(Pluie.mensuel1,2,mean,na.rm=T)
moy.61.90.param1=cbind(moy.61.90.param1,pluie1.)
monthOrder <- c('janv.', 'févr.', 'mars', 'avr.', 'mai', 'juin', 'juil.',
                'août', 'sept.', 'oct.', 'nov.', 'déc.')
moyenne.mensuel1$Month <- factor(format(moyenne.mensuel1$Date, "%b"),
                                 levels = monthOrder)

detach(moyenne.mensuel1)

#Moyennes mensuelles sur 61-1990
x2=as.data.frame(cbind(Mois=month.abb[1:12],round(moy.61.90.param1,2)))
x2
#x2=round(x1,2)

x=data.frame(data)
x$Pluie.non.corrigée<-as.numeric(x$Pluie.non.corrigée)


y=data.frame(data1)
y$Pluie.observée<-as.numeric(y$Pluie.observée)


#eqm1 <-biasCorrection(y = y, x = x,precipitation = TRUE, method = "eqm",window = NULL,
#wet.threshold = 0.1,join.members = TRUE)


eqm<- biasCorrect(x, x, y, method = "eqm",preci = TRUE)
scaling <- biasCorrect(x, x, y, preci = TRUE)
delta <- biasCorrect(x, x, y, method= "delta", preci = TRUE)
gqm <- biasCorrect(x, x, y, method= "gqm", preci = TRUE)

tableau <- eqm %>%
  mutate(month = lubridate::month(Date, label=TRUE, abbr=TRUE), 
         obs = y$Pluie.observée,
         n_cr = x$Pluie.non.corrigée,
         scaling = scaling$Pluie.non.corrigée,
         eqm = Pluie.non.corrigée,
         delta = delta$Pluie.non.corrigée,
         gqm = gqm$Pluie.non.corrigée
         ) %>%
  group_by(month) %>%
  summarise(
    mean.n_cr=mean(n_cr),
    mean.obs=mean(obs),
    
    
    mean.eqm=mean(Pluie.non.corrigée),
    mean.scaling=mean(scaling),
    mean.delta=mean(delta),
    mean.gqm=mean(gqm),
    .groups = 'drop') 
               
  
tableau %>%
  pivot_longer( 
    cols=c("mean.n_cr","mean.obs","mean.eqm","mean.scaling","mean.delta","mean.gqm"),
    names_to = "mean",
    values_to = "value",
  )%>%
  ggplot(aes(x = month, y=value, group=mean, color=mean))+
  geom_line(size=0.5)+
  geom_point()




  



