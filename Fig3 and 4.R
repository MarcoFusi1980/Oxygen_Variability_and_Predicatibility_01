library(lubridate)
library(dplyr)
library(ggplot2)
library(lavaan)

##Here the evironemntal table from TabS1A-D provided in the supplementary Data 1 along with the paper.
ambiente<-read.table()
pippo<-as_datetime(ambiente$grouped_time, format = '%Y-%m-%d %H:%M')
##best conversion in datetime
head(pippo)
ambiente$datetime<-pippo


ambiente$grouped_time = lubridate::floor_date(ambiente$datetime, unit = "day")
test2max = ambiente %>%
  group_by(grouped_time, Location) %>%
  summarise_if(is.numeric, max, na.rm = TRUE)
test2min = ambiente %>%
  group_by(grouped_time, Location) %>%
  summarise_if(is.numeric, min, na.rm = TRUE)
test2mean = ambiente %>%
  group_by(grouped_time, Location) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)


test2mean$VarianzaOx<-(test2max$DissolvedOxygenSaturation2-test2min$DissolvedOxygenSaturation2)
test2mean$Varianzatemp<-(test2max$Temperature_C-test2min$Temperature_C)



test2mean$Location<-factor(test2mean$Location,levels = c("Cold Temperate","Warm Temperate","Tropical"),labels = c("Cold Temperate","Warm Temperate","Tropical"))

ggplot(test2mean,aes(test2mean$T,test2mean$VarianzaOx))+geom_point()+facet_wrap(~test2mean$Location)+stat_smooth(method="lm")


ggplot(test2mean,aes(test2mean$Temp,test2mean$VarianzaOx))+
  geom_point()+stat_smooth(method="lm",color="red")+ylim(0,300)+
  theme_classic()+
  xlab('Daily Mean Temperature (°C)')+
  ylab('Daily Mean Oxygen Variation (% a.s.)')+
  scale_x_continuous(breaks=seq(0,40,5))+
  theme(legend.title = element_text(angle = 90))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust=0.5),
        legend.title = element_text(angle = 0, size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        text=element_text(size=14)
  )+xlim(5,40)
  


# analysis of daily deltaT as function of daily deltaOx

  ggplot(test2mean,aes(test2mean$Temperature_C,test2mean$VarianzaOx))+geom_point()+facet_wrap(~test2mean$Location,ncol = 1)+stat_smooth(method="lm",color="red")+
  theme_classic() + xlab('Temperature variance (°C)')+ylab('Oxygen variance (% a.s.)')+
    scale_x_continuous(breaks=seq(0,40,5))+theme(text=element_text(size=14))


model1<-lm(test2mean$VarianzaOx~test2mean$Temperature_C*test2mean$Location)
plot(model1,2)
summary(model1)
require(broom)  
tidy(model1)

coefficients(model1)

plot()
test2mean$Location
esse<-test2mean[which(test2mean$Location=='Cold Temperate'),]
model1<-lm(esse$VarianzaOx~esse$Temperature_C)
plot(model1,2)
summary(model1)
require(broom)  
tidy(model1)

coefficients(model1)


# analysis of daily deltaT as function of daily deltaOx

ggplot(test2mean,aes(test2mean$Temperature_C,test2mean$DissolvedOxygenSaturation2))+geom_point()+facet_wrap(~test2mean$Location)+stat_smooth(method="lm")


ggplot(test2mean,aes(test2min$Temperature_C,test2max$Temperature_C))+geom_point()+facet_wrap(~test2mean$Location)+stat_smooth(method="lm")


varto<-ggplot(test2mean,aes(Location,test2mean$Varianzatemp))+geom_boxplot()
test2mean$Varianzatemp_percent<-(test2mean$Varianzatemp/max(test2max$Temperature_C))*100
ggplot(test2mean,aes(Location,test2mean$Varianzatemp_percent))+geom_boxplot()

varox<-ggplot(test2mean,aes(Location,test2mean$VarianzaOx))+geom_boxplot()
test2mean$VarianzaOx_percent<-(test2mean$VarianzaOx/max(test2max$DissolvedOxygenSaturation2))*100
ggplot(test2mean,aes(Location,test2mean$VarianzaOx_percent))+geom_boxplot()

ggplot(test2mean,aes(Varianzatemp_percent,test2mean$VarianzaOx_percent))+geom_point()+facet_wrap(~test2mean$Location)
a<-tapply(test2mean$VarianzaOx,test2mean$Location,max)
b<-tapply(test2max$Temperature_C,test2max$Location,mean)

plot(a,b)


##Max temp for tropical

test2mean_Tropical<-test2mean%>%filter(grouped_time > "2018-04-30" & grouped_time <"2018-06-30"& Location=="Tropical")
test2mean_WT<-test2mean%>%filter(grouped_time > "2017-06-01" & grouped_time <"2017-09-01"& Location=="Warm Temperate")
test2mean_CT<-test2mean%>%filter(grouped_time > "2018-06-30" & grouped_time <"2018-08-01"& Location=="Cold Temperate")

test2max_Tropical<-test2max%>%filter(grouped_time > "2018-04-30" & grouped_time <"2018-06-30"& Location=="Tropical")
test2max_WT<-test2max%>%filter(grouped_time > "2017-06-01" & grouped_time <"2017-09-01"& Location=="Warm Temperate")
test2max_CT<-test2max%>%filter(grouped_time > "2018-06-30" & grouped_time <"2018-08-01"& Location=="Cold Temperate")

test2min_Tropical<-test2min%>%filter(grouped_time > "2018-04-30" & grouped_time <"2018-06-30"& Location=="Tropical")
test2min_WT<-test2min%>%filter(grouped_time > "2017-06-01" & grouped_time <"2017-09-01"& Location=="Warm Temperate")
test2min_CT<-test2min%>%filter(grouped_time > "2018-06-30" & grouped_time <"2018-08-01"& Location=="Cold Temperate")


tabella_varianze<-rbind(test2mean_Tropical,test2mean_WT,test2mean_CT)
tabella_varianze_max<-rbind(test2max_Tropical,test2max_WT,test2max_CT)
tabella_varianze_min<-rbind(test2min_Tropical,test2min_WT,test2min_CT)

a<-tapply(tabella_varianze_max$DissolvedOxygenSaturation2,tabella_varianze_max$Location,max)
b<-tapply(tabella_varianze_max$Temperature_C,tabella_varianze_max$Location,max)

##Riprendo i LT50

setwd("~/Dropbox/Articoli/Perspective Frontiers/Hyperossia2/Analysis/")
letale<-read.table("TabS2-S4.txt",header=T,sep="\t")
letale$Location<-factor(letale$Location,levels = c("StAbbs","Chioggia","ReSea"),labels = c("Cold Temperate","Warm Temperate","Tropical"))

varla<-ggplot(data=letale,aes(x=Location,y=deltlt50))+geom_boxplot()
model<-aov(data=letale,deltlt50~Location)
varsa<-ggplot(data=letale,aes(x=Location,y=safety))+geom_boxplot()

varsa_norm<-ggplot(data=letale,aes(x=Location,y=safety_Normo))+geom_boxplot()
color_flag <- c("blue","darkgreen","gold")

carlone<-ggplot(data=letale,aes(x=H,y=deltlt50))+
  geom_point(stat='identity', position='identity',size=3,shape=21,aes(fill=Location))+
  geom_smooth(method = "lm",color="blue",se=FALSE,level = 0.95)+
  theme_classic()+
  scale_y_continuous(limits = c(-0.3,6),breaks=seq(0,5,1))+
  scale_x_continuous(limits = c(-3,28),breaks=seq(-5,35,2.5))+
  scale_fill_manual(name = "",values=color_flag) +
  ylab(expression(paste(Delta,LT["50"],"(°C)")))+
         xlab("Thermal Safety Margin (°C)")+
  theme(legend.position = "none",
        legend.box = "vertical")+
  ggtitle('B')+
  geom_vline(aes(xintercept = 0),colour="red", linetype = "longdash")


carlone

model_carlone<-lm(data=letale,deltlt50~H)
plot(model_carlone,2)

summary(model_carlone)

expression(alpha)




carlone2<-ggplot(data=letale,aes(x=N,y=deltlt50,color=Location, label =species))+geom_point()+stat_smooth(method = "lm",color="red")
carlone2<-ggplot(data=letale,aes(x=N,y=deltlt50))+
  geom_point(stat='identity', position='identity',size=3,shape=21,aes(fill=Location))+
  geom_smooth(method = "lm",color="blue",se=FALSE,level = 0.95)+
  theme_classic()+
  scale_y_continuous(limits = c(-0.3,6),breaks=seq(0,5,1))+
  scale_x_continuous(limits = c(-3,28),breaks=seq(-5,35,2.5))+
  scale_fill_manual(name = "",values=color_flag) +
  ylab(expression(paste(Delta,LT["50"],"(°C)")))+
  xlab("Thermal Safety Margin (°C)")+
  theme(legend.position = "none",
        legend.box = "vertical")+
  ggtitle('B')+
  geom_vline(aes(xintercept = 0),colour="red", linetype = "longdash")


carlone2

plot(model,2)
summary(model)
TukeyHSD(model,"Location")
is.data.frame(letale)

summary(lt)



carlone4<-ggplot(data=letale,aes(x=safety,y=deltlt50))+
  geom_point(size=3,shape=21,aes(fill=Location))+
  geom_smooth(method = "lm",color="blue",se=FALSE,level = 0.95)+
  theme_classic()+
  scale_y_continuous(limits = c(-0.3,6),breaks=seq(0,5,1))+
  scale_x_continuous(limits = c(-3,28),breaks=seq(-5,35,2.5))+
  scale_fill_manual(values=color_flag) +
  ylab(expression(paste(Delta,LT["50"],"(°C)")))+
  xlab("Thermal Safety Margin (°C)")+
  theme(legend.position = "none",
        legend.box = "vertical")+
  ggtitle('C')+
  geom_vline(aes(xintercept = 0),colour="red", linetype = "longdash")

carlone4


carlone5<-ggplot(data=letale,aes(x=safety_Normo,y=deltlt50))+
  geom_point(size=3,shape=21,aes(fill=Location))+
  geom_smooth(method = "lm",color="blue",se=FALSE,level = 0.95)+
  theme_classic()+
  scale_y_continuous(limits = c(-0.3,6),breaks=seq(0,5,1))+
  scale_x_continuous(limits = c(-3,28),breaks=seq(-5,35,2.5))+
  scale_fill_manual(values=color_flag) +
  ylab(expression(paste(Delta,LT["50"],"(°C)")))+
  xlab("Thermal Safety Margin (°C)")+
  theme(legend.position = "none",
        legend.box = "vertical")+
  ggtitle('A')+
  geom_vline(aes(xintercept = 0),colour="red", linetype = "longdash")

carlone5


library(ggpubr)
ggarrange(varla,varto,varox,varsa,varsa_norm,nrow=5)
ggarrange(carlone,carlone4,carlone2,carlone5,ncol=2,nrow=2)
gridExtra::grid.arrange(carlone2,carlone5,ncol=2)
gridExtra::grid.arrange(carlone5,carlone,ncol=1)

model_carlone<-lm(data=letale,deltlt50~N)
summary(model_carlone)

model_carlone2<-lm(data=letale,deltlt50~safety_Normo)
summary(model_carlone2)
load("Fig_3_Save_data.Rdata")
names(ambiente)

media<-ambiente%>%group_by(Location)%>%summarise(Temp_mean=mean(Temperature_C))
mediaox<-ambiente%>%group_by(Location)%>%summarise(OxMean_mean=mean(DissolvedOxygenSaturation2))
struttura<-read.table("TabS5.txt",header=T,sep="\t")
str(struttura)


#High preditability --->  control of satefty margin   ----> efficient thermic response
struttura2<-abs(scale(struttura[,c(3:17)]))
struttura2<-as.data.frame(struttura2)
names(struttura2)

struttura3<-struttura2%>%select(delta_lt50,Temperature_predicatibility,Oxygen_predicatibility,TSM_Hyper,TSM_Normo,Oscilla_T2,Oscilla_O2)
struttura3<-as.matrix(struttura3)

is.matrix(struttura3)

str(struttura3)
model <-'
# STRUCTURAL EQUATION MODELLING
delta_lt50~Oxygen_predicatibility+Temperature_predicatibility
TSM_Hyper~Oxygen_predicatibility+Temperature_predicatibility
TSM_Normo~Oxygen_predicatibility+Temperature_predicatibility
delta_lt50~TSM_Normo+TSM_Hyper
TSM_Normo~~TSM_Hyper+Oscilla_O2
TSM_Hyper~~Oscilla_O2
'
fit <- sem(model, data = struttura3)


totale_ox_temp<-summary(fit, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
minimal

plot(data=struttura, Oscilla_O2~Oxygen_predicatibility)
plot(data=struttura, Oscilla_T2~Temperature_predicatibility)

summary(lm(data=struttura, Oscilla_O2~Oxygen_predicatibility))
summary(lm(data=struttura, Oscilla_T2~Temperature_predicatibility))
summary(lm(data=struttura, Oscilla_O2~Temperature_predicatibility))
summary(lm(data=struttura, Oscilla_T2~Oxygen_predicatibility))
summary(lm(data=struttura, Oscilla_T2~Oscilla_O2))


library(lavaanPlot)
lavaanPlot(fit)
semPlot::semPaths(fit,"est",curvePivot = TRUE, layout = "circle")
