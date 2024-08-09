library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

##Refer to the Supplementary Data 1 provided with the paper
mangrovie<-read.table("TabS1D.txt",header=T,sep=",")
##data conversion using lubridate
pippo<-as_datetime(mangrovie$grouped_time,format = "%Y-%m-%d %H:%M") ##best conversion in datetime
head(pippo)
mangrovie$datetime<-pippo
names(mangrovie)

a1mangrovie<-ggplot(mangrovie, aes(datetime, O2sat)) + 
  theme_classic() +
  geom_point(aes(color=Temp),stroke = 0.01) +
  ylim(0,300)+
  #facet_wrap(~Location, ncol= 1) +
  scale_colour_gradient2(low = "blue",  mid = "yellow", high = "red",  midpoint = 20)+
  ylab("Oxygen Saturation (%)") +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1)+
  ## stat_smooth(color="red", method = "loess",size=0.5)+
  xlab("Date") +
  scale_x_datetime(date_breaks = "30 day",date_labels = "%b %Y") +
  labs(color = "Temperature °C")+theme(legend.title = element_text(angle = 90))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(angle = 0, size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        text=element_text(size=18))+xlab("")

a1mangrovie

##TabS1A+TabS1B+TabS1C is the collate table forom the three suppplementay table provided with the Supplementary Data 1 provided with the paper
ambiente<-read.table("TabS1A+TabS1B+TabS1C.txt",header=T,sep="\t")
##data conversion using lubridate
pippo<-as_datetime(ambiente$Date,format = "%Y-%m-%d %H:%M") ##best conversion in datetime
head(pippo)
ambiente$datetime<-pippo

## renaming the factors
ambiente$Location<-factor(ambiente$Location,levels = c("Cold Temperate","Warm Temperate","Tropical"),labels = c("Cold Temperate","Warm Temperate","Tropical"))

cold<-subset(ambiente,ambiente$Location=="Cold Temperate")
cold<-subset(cold,cold$Temperature_C<16)

temp<-subset(ambiente,ambiente$Location=="Warm Temperate")
hot<-subset(ambiente,ambiente$Location=="Tropical")

## plot
a1<-ggplot(temp, aes(datetime, DissolvedOxygenSaturation2)) +
  theme_classic() +
  geom_point(aes(color=Temperature_C),stroke = 0.01) +
  #facet_wrap(~Location, ncol= 1) +
  scale_colour_gradient2(low = "blue",  mid = "yellow", high = "red",  midpoint = 20)+
  ylab("Oxygen Saturation (%)") +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1)+
## stat_smooth(color="red", method = "loess",size=0.5)+
  xlab("Date") +
  scale_x_datetime(date_breaks = "30 day",date_labels = "%b %Y") +
  labs(color = "Temperature °C")+theme(legend.title = element_text(angle = 90))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(angle = 0, size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        text=element_text(size=14))

a1
a2<-ggplot(cold, aes(datetime, DissolvedOxygenSaturation2)) + 
  theme_classic() +
  geom_point(aes(color=Temperature_C),stroke = 0.01) +
  #facet_wrap(~Location, ncol= 1) +
  scale_colour_gradient2(low = "blue",  mid = "yellow", high = "red",  midpoint = 20)+
  ylab("Oxygen Saturation (%)") +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1)+
  ## stat_smooth(color="red", method = "loess",size=0.5)+
  xlab("Date") +
  ylim(0,200) +
  scale_x_datetime(date_breaks = "30 day",date_labels = "%b %Y") +
  labs(color = "Temperature °C")+theme(legend.title = element_text(angle = 90))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(angle = 0, size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        text=element_text(size=14)
)
  
a2
a3<-ggplot(hot, aes(datetime, DissolvedOxygenSaturation2)) + 
  theme_classic() +
  geom_point(aes(color=Temperature_C),stroke = 0.01) +
  #facet_wrap(~Location, ncol= 1) +
  scale_colour_gradient2(low = "blue",  mid = "yellow", high = "red",  midpoint = 20)+
  ylab("Oxygen Saturation (%)") +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1)+
  ## stat_smooth(color="red", method = "loess",size=0.5)+
  xlab("Date") +
  ylim(0,300)+
  scale_x_datetime(date_breaks = "30 day",date_labels = "%b %Y") +
  labs(color = "Temperature °C")+
  theme(legend.title = element_text(angle = 90))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(angle = 0, size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        text=element_text(size=18))+xlab("")



a3
mylegend<-g_legend(a3)
p3 <- grid.arrange(arrangeGrob(a2 + theme(legend.position="none"),
                               a1 + theme(legend.position="none"),
                               a3 + theme(legend.position="none"),
                               ncol=1))




a4<-Oxy_density<-ggplot(data=ambiente,aes(x=DissolvedOxygenSaturation2))+
  geom_density(color="cyan4",alpha=0.4,fill="cyan3")+
  facet_wrap(~Location,ncol=1) + theme(legend.position = "none")+
  xlab(" Oxygen Saturation (%)")+
  ylab("Density")+
  geom_vline(aes(xintercept = 100),colour = "forestgreen",linetype = "dashed")+
  theme_classic()+theme(text = element_text(size=14)) 
Oxy_density
gridExtra::grid.arrange(a2+ theme(legend.position="none"),a1+ theme(legend.position="none"),a3+ theme(legend.position="none"))
a4

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

ggplot(test2mean,aes(test2mean$Varianzatemp,test2mean$VarianzaOx))+geom_point()+facet_wrap(~test2mean$Location)+stat_smooth(method="lm")




# Plot for analyis of the daily DeltaT as a function of daily DeltaOx
ggplot(test2mean[test2mean$Location=='Cold Temperate',],aes(x=Temperature_C,y=VarianzaOx))+
  geom_point()+stat_smooth(method="lm",color="red")+
  theme_classic() + 
  ylim(0,200)+
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

ggplot(test2mean[test2mean$Location=='Warm Temperate',],aes(Temperature_C,VarianzaOx))+geom_point()+stat_smooth(method="lm",color="red")+
  theme_classic() + xlab('Daily Mean Temperature (°C)')+ylab('Daily Mean Oxygen Variation (% a.s.)')+
  scale_x_continuous(breaks=seq(0,40,5))+theme(legend.title = element_text(angle = 90))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust=0.5),
        legend.title = element_text(angle = 0, size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        text=element_text(size=14)
  ) +xlim(5,40)

ggplot(test2mean[test2mean$Location=='Tropical',],aes(Temperature_C,VarianzaOx))+geom_point()+stat_smooth(method="lm",color="red")+
  theme_classic() + xlab('Daily Mean Temperature (°C)')+ylab('Daily Mean Oxygen Variation (% a.s.)')+
  scale_x_continuous(breaks=seq(0,40,5))+theme(legend.title = element_text(angle = 90))+
  xlim(5,40)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust=0.5),
        legend.title = element_text(angle = 0, size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        text=element_text(size=14)
  )


getwd()


ggpubr::ggarrange(a1mangrovie,a3,ncol=1)

save.image("Analysis.Rdata")
