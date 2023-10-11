install.packages("astsa")
library(maptools)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(lubridate)
library(ggsci)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
library(astsa)

x_seq <- seq(from = as.POSIXct("2010-01-01", tz = "GMT"),length.out = 365, by = "days")
coord <- matrix(c(-0.13, 51.5), nrow = 1)
sunrise <- sunriset(coord, x_seq, direction = "sunrise",POSIXct.out = TRUE)


setwd("~/Dropbox/Articoli/Perspective Frontiers/Hyperossia2/Analysis/")
ambiente<-read.table("Environmental_Variabiles_Total.txt",header=T,sep="\t")
str(ambiente)
ambiente<-na.omit(ambiente)
hist(ambiente$Dissolved_Oxygen_mgL2)
##converto la data in maniera corretta con lubridate

pippo<-as_datetime(ambiente$UTC_Date_._Time) ##best conversion in datetime
head(pippo)
ambiente$datetime<-pippo
## plotto in maniera corretta il grafico
ambiente$Location<-factor(ambiente$Location,levels = c("StAbbs","Chioggia","RedSea"),labels = c("Cold Temperate","Warm Temperate","Tropical"))
a<-ggplot(ambiente, aes(ambiente$datetime, Temperature_C)) + 
  theme_classic() +
  geom_point(aes(color=DissolvedOxygenSaturation2),size=0.5) +
               facet_wrap(~Location, ncol= 1) +
  scale_color_gradient2(low = "#ffffd9", high = "#081d58", mid="#41b6c4",midpoint = 100) +
  ylab("Temperature °C") +
  xlab("Date") +
  scale_x_datetime(date_breaks = "30 day") +
  labs(color = "Dissolved Oxygen (%)") +
  ylim(0,40)
 
  a + ggpubr::rotate_x_text()
  
  ###controllo i dati pubblicati in Giomi et al., 2019 con quelli presi da noi
  MangroveDuarte<-read.table("MangroveDataDuarte.txt",header=T,sep="\t")
  pippo<-as_datetime(MangroveDuarte$Data) ##best conversion in datetime
  head(MangroveDuarte)
  MangroveDuarte$Dat<-pippo
  
  b<-ggplot(MangroveDuarte, aes(Dat, T )) + 
    theme_classic() +
    geom_point(aes(color=X.O2),size=0.5) +
    scale_color_gradient2(low = "red", mid = "yellow",high = "blue", midpoint = 7.5) +
    ylab("Temperature °C") +
    xlab("Date") +
    scale_x_datetime(date_breaks = "30 day") +
    labs(color = "Dissolved Oxygen %") 
  
  b + ggpubr::rotate_x_text()
  ###i dati sono giusti
  
  
  ###procedo a migliorare la figura compressando i dati per ora anzinche' ogni 10 minuti
  ambiente$grouped_time = lubridate::floor_date(ambiente$datetime, unit = "hour")
  test2 = ambiente %>%
    group_by(grouped_time, Location) %>%
    summarise_all(funs(mean))
  
  ###controllo il miglioramento
  
 c<-ggplot(test2, aes(datetime, Temperature_C)) + 
    theme_classic() +
    geom_point(aes(color=DissolvedOxygenSaturation2),size=0.5) +
    facet_wrap(~Location, ncol= 1) +
    scale_color_gradient2(low = "#ffffd9", high = "#081d58", mid="#41b6c4",midpoint = 100) +
    ylab("Temperature °C") +
    xlab("Date") +
    scale_x_datetime(date_breaks = "30 day") +
    labs(color = "Dissolved Oxygen (%)") +
    ylim(0,40)
  
  c + ggpubr::rotate_x_text()
  
write.table(test2,"Environmental_Variabiles_Total_hourly.txt",sep='\t')

##richiamo il logger delle temperature
acqua<-read.table("Water4.txt",header=T,sep="\t")
summary(acqua)
pippo<-as_datetime(acqua$Datetime) ##best conversion in datetime
head(pippo)
acqua$datetime<-pippo

acqua$grouped_time = lubridate::floor_date(acqua$datetime, unit = "hour")

acqua2 = acqua %>%
  group_by(grouped_time) %>%
  summarise_all(funs(mean))
write.table(acqua2,"Water4hourly.txt",sep='\t')

ambiente<-read.table("Environmental_Variabiles_Total_hourly.txt",header=T,sep="\t")
##converto la data in maniera corretta con lubridate
ambiente$Location<-factor(ambiente$Location,levels = c("Cold Temperate","Warm Temperate","Tropical"),labels = c("Cold Temperate","Warm Temperate","Tropical"))

pippo<-as_datetime(ambiente$Date) ##best conversion in datetime
head(pippo)
ambiente$datetime<-pippo
## plotto in maniera corretta il grafico
a<-ggplot(ambiente, aes(ambiente$datetime, Temperature_C)) + 
  theme_classic() +
  geom_point(aes(color=DissolvedOxygenSaturation2),size=0.5) +
  facet_wrap(~Location, ncol= 1) +
  scale_color_gradient2(low = "#deebf7", high = "#081d58", mid="#9ecae1",midpoint = 100) +
  ylab("Temperature °C") +
  stat_smooth(color="red", method = "loess",size=0.5)+
  xlab("Date") +
  scale_x_datetime(date_breaks = "30 day",date_labels = "%b %Y") +
  labs(color = "Oxygen Saturation (%)") +
  ylim(0,45)

aa<-a + ggpubr::rotate_x_text()




##provo qua a proporre una soluzione come per le perspective
Oxy_density<-ggplot(data=ambiente,aes(x=DissolvedOxygenSaturation2))+
  geom_density(color="cyan4",alpha=0.4,fill="cyan3")+
  facet_wrap(~Location,nrow=1) + theme(legend.position = "none")+
  xlab(" Oxygen Saturation (%)")+
  ylab("Density")+
  geom_vline(aes(xintercept = 100),colour = "forestgreen",linetype = "dashed")+
  theme_classic()+theme(text = element_text(size=14)) 
Oxy_density



ggarrange(aa,Oxy_density,heights=c(2,1),nrow=2,ncol=1,labels = c("A","B"),common.legend = TRUE)



##Bin delle temperature 
##correggo per eccesso l'ossigeno e temperatura
str(ambiente)
ambiente$OxigenBin<-round(ambiente$Dissolved_Oxygen_mgL,digits = 0)
ambiente$T_bin<-round(ambiente$Temperature_C,digits = 0)


###DA FARE GIORNO E NOTTE -mettiamo in stand by per il momento 20200430
natural<-ggplot(ambiente, aes(as.factor(ambiente$T_bin), Dissolved_Oxygen_mgL)) + 
  geom_violin() +
  theme_classic() +
  ylim(0,20) + 
  facet_wrap(~Location, ncol= 1) +
  xlab("Temperature °C") +
  ylab("Oxygen (mg/L)") 
ggsave("Rploty2.tiff", natural, bg = "transparent")


###faccio la curva di saturazione dell'ossigeno
str(ambiente)

temp<-seq(0, 42, by=0.1)  
###Oxygen Solubility in seawater at 40ppt f(x) = -2.446ln(x) + 14.188
ox<-function(x) {-2.446*log(x) + 14.188}
summary(ambiente)
ambiente$T_arrotondato<-round(ambiente$Temperature_C)
axy100_sat<-read.table("oxygen_physiscal_Saturation.txt",header=T,sep='\t')
summary(axy100_sat)
axy100_sat$Climate<-factor(axy100_sat$Climate,levels = c("Cold Temperate","Warm Temperate","Tropical"),labels = c("Cold Temperate","Warm Temperate","Tropical"))

base <- ggplot(axy100_sat, aes(factor(Temperature),oxy,group=1)) + geom_line(size=0.5,color="red") + 
  facet_wrap(~Climate, ncol= 1) +
  ylim(0,20)  +
  theme_classic() +
  xlab("") + 
  ylab("") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
   axis.ticks = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
 )
base

ggsave("Rploty.tiff", base, bg = "transparent")

ambiente$Location<-factor(ambiente$Location,levels = c("Cold Temperate","Warm Temperate","Tropical"),labels = c("Cold Temperate","Warm Temperate","Tropical"))

riempio <- ggplot(ambiente, aes(factor(ambiente$T_arrotondato),Dissolved_Oxygen_mgL2)) + geom_violin() + 
  facet_wrap(~Location, ncol= 1) +
  ylim(0,20)+
  theme_classic() +
  ylab("Oxygen concentration (mg/L)") + 
  xlab("Temperature °C") 

riempio
###figura per le temperature orizzontali FIG2

#reverse the factor level for grpah preparation

ambiente$Location = with(ambiente, factor(Location, levels = rev(levels(Location))))
fig2_temp<-ggplot(ambiente, aes(ambiente$Location, Temperature_C, fill=Location)) + 
  theme_classic() +
  geom_violin()+
 # facet_wrap(~Location,ncol=1) +
  scale_fill_manual(values = c("red","forestgreen","blue"))+
  xlab(" ") +
  ylab("Temperature (°C)") +
  coord_flip() + theme(legend.position='none') +
  scale_y_continuous(breaks = pretty(ambiente$Temperature_C, n = 10)) +
  theme(
  #axis.text.x = element_blank(),
  #axis.text.y = element_blank(),
  #axis.ticks = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

fig2_temp

ggsave("fig2_temp.tiff", fig2_temp, bg = "transparent")


ambiente_chioggia<-subset(ambiente,ambiente$Location=='Chioggia')
fig2_temp_chioggia<-ggplot(ambiente_chioggia, aes(Temperature_C, fill="forestgreen")) + 
  theme_classic() +
  geom_violin()+
  facet_wrap(~Location,ncol=1) +
  xlab(" ") +
  ylab("Temperature (°C)") +
  coord_flip() + theme(legend.position='none') + ylim(0,50)

##provo qua a proporre una soluzione come per le perspective
Oxy_density<-ggplot(data=ambiente,aes(x=DissolvedOxygenSaturation2))+
  geom_density(color="cyan4",alpha=0.4,fill="cyan3")+
  facet_wrap(~Location) + theme(legend.position = "none")+
  xlab("Dissolved Oxygen %a.s.")+
  ylab("Density")+
  geom_vline(aes(xintercept = 100),colour = "forestgreen",linetype = "dashed")+
  theme_classic()+theme(text = element_text(size=14)) 
Oxy_density

Temp_density<-ggplot(data=ambiente,aes(x=Temperature_C))+
  geom_density(color="red",alpha=0.4,fill="brown3")+
  facet_wrap(~Location) + theme(legend.position = "none")+
  xlab("Temperature °C")+
  ylab("Density")+
   theme_classic()+theme(text = element_text(size=14)) 
Temp_density
ggarrange(Oxy_density,Temp_density,ncol=1, nrow = 2)


#provo qua a calcolare le correlazioni
ccf(ambiente$Temperature_C, ambiente$DissolvedOxygenSaturation2)


ambiente_Tropical<-subset(ambiente,ambiente$Location=='Tropical')
ambiente_WTemperate<-subset(ambiente,ambiente$Location=='Warm Temperate')
ambiente_CTemperate<-subset(ambiente,ambiente$Location=='Cold Temperate')


Tropical_Temp<-ts(ambiente_Tropical$Temperature_C)
Tropical_Oxygen<-ts(ambiente_Tropical$DissolvedOxygenSaturation2)

WTemp_Temp<-ts(ambiente_WTemperate$Temperature_C)
WTemp_Oxygen<-ts(ambiente_WTemperate$DissolvedOxygenSaturation2)

CTemp_Temp<-ts(ambiente_CTemperate$Temperature_C)
CTemp_Oxygen<-ts(ambiente_CTemperate$DissolvedOxygenSaturation2)


ccf2(Tropical_Temp, Tropical_Oxygen,max.lag=10, main="Tropical Temperature and Oxygen Saturation")
ccf2(WTemp_Temp,WTemp_Oxygen, max.lag=10, main="Warm Temp Temperature and Oxygen Saturation")
ccf2(CTemp_Temp, CTemp_Oxygen,max.lag=10, main="Cold Temp Temperature and Oxygen Saturation")

lag2.plot(Tropical_Temp, Tropical_Oxygen)
lag2.plot(WTemp_Temp,WTemp_Oxygen)
lag2.plot(CTemp_Temp, CTemp_Oxygen)


install.packages("dbplyr")
library(tidyquant)
#provo qua a calcolare le correlazioni con le variabile detrendend
Tropical_Temp<-diff(ambiente_Tropical$Temperature_C,24)
Tropical_Oxygen<-diff(ambiente_Tropical$DissolvedOxygenSaturation2,24)


WTemp_Temp<-diff(ambiente_WTemperate$Temperature_C,24)
WTemp_Oxygen<-diff(ambiente_WTemperate$DissolvedOxygenSaturation2,24)

CTemp_Temp<-diff(ambiente_CTemperate$Temperature_C,24)
CTemp_Oxygen<-diff(ambiente_CTemperate$DissolvedOxygenSaturation2,24)

par(mfrow=c(1,1))
uno<-ccf2(Tropical_Temp, Tropical_Oxygen,max.lag=12, main="Tropical Temperature and Oxygen Saturation")
due<-ccf2(WTemp_Temp,WTemp_Oxygen, max.lag=12, main="Warm Temp Temperature and Oxygen Saturation")
tre<-ccf2(CTemp_Temp, CTemp_Oxygen,max.lag=12, main="Cold Temp Temperature and Oxygen Saturation")

uno<-data.frame(uno)
plot(uno$LAG,uno$CCF)


par(mfrow=c(2,2))
lag2.plot(Tropical_Temp, Tropical_Oxygen)
lag2.plot(WTemp_Temp,WTemp_Oxygen)
lag2.plot(CTemp_Temp, CTemp_Oxygen)



#testo
res_tropical <- cor.test(Tropical_Temp, Tropical_Oxygen,method = "spearman")
res_WTemp<-cor.test(WTemp_Temp,WTemp_Oxygen, method = "spearman")
res_CTemp<-cor.test(CTemp_Temp, CTemp_Oxygen,method = "spearman")

res_tropical <- cor.test(Tropical_Temp, Tropical_Oxygen,method = "pearson")
res_WTemp<-cor.test(WTemp_Temp,WTemp_Oxygen, method = "pearson")
res_CTemp<-cor.test(CTemp_Temp, CTemp_Oxygen,method = "pearson")




#provo qua a proporre una soluzione come per le perspective

Oxy_var<-ggplot(data=ambiente,aes(y=Temperature_C,x=Location))+
  theme_classic() +
  geom_jitter(aes(color=Dissolved_Oxygen_mgL2),size=1) +
  scale_color_gradient2(low = "green", mid = "red",high = "cyan4", midpoint = 7.5) +
  ylab("Temperature °C") +
  xlab("Location") +
  labs(color = "Dissolved Oxygen (mg/L)") 
Oxy_var
Temp_var<-ggplot(data=ambiente,aes(y=Temperature_C,x=Location))+
  theme_classic() +
  geom_jitter(size=1) +
  ylab("Temperature °C") +
  xlab("Location") 
Temp_var
Oxy_var<-ggplot(data=ambiente,aes(y=DissolvedOxygenSaturation2,x=Location))+
  theme_classic() +
  geom_jitter(size=1,color="cyan4") +
  ylab("Dissolved Oxygen (%)") +
  xlab("Location") +
  geom_hline(aes(yintercept = 100),colour = "forestgreen",linetype = "dashed")
  
Oxy_var
