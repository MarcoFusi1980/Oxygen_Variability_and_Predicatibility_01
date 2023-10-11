install.packages("flexsurv")
library(survival)
library(survminer)
library(flexsurv)
setwd("~/Dropbox/KAUST/Projects/Hyperoxia/Manuscript/Analysis/")
surva<-read.table("Holoturia_RedSea.txt",header=T,sep="\t")
str(surva)
fit = survfit(Surv(T_water, ID) ~ Oxygen, data = surva)
fit1 = survreg(Surv(T_water, ID) ~ Oxygen, data = surva)

plot(fit,xlim=c(30,50))
s <- with(surva,Surv(T_water, ID))


sWei  <- flexsurvreg(s ~ as.factor(surva$Oxygen),dist='weibull',data=lung)
sLno  <- flexsurvreg(s ~ as.factor(surva$Oxygen),dist='lnorm',data=lung)   

plot(sLno,xlim=c(30,50))
lines(sLno, col="blue")






##prima prova
ggsurvplot(fit)
summary(fit)
ggsurvplot(fit,
conf.int = TRUE, # Add confidence interval 
ggtheme = theme_bw() # Change ggplot2 theme 
)
###second prova con mediana
ggsurv <- ggsurvplot(fit,
                     conf.int = TRUE,
                     risk.table = TRUE, # Add risk table
                     risk.table.col = "strata", # Change risk table color by groups
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#2E9FDF")
)

# Drawing a horizontal line at 50% survival
surv_median <- as.vector(summary(fit)$table[, "median"])
df <- data.frame(x1 = surv_median, x2 = surv_median,
                 y1 = rep(0, length(surv_median)), y2 = rep(0.5, length(surv_median)))

ggsurv$plot <- ggsurv$plot + 
  geom_segment(aes(x = 0, y = 0.5, xend = max(surv_median), yend = 0.5),
               linetype = "dashed", size = 0.5)+ # horizontal segment
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df,
               linetype = "dashed", size = 0.5) # vertical segments

print(ggsurv)


###prova con lethal dose
install.packages("drc")
library("drc") # dose response curves


species<-read.table("Mortality_oloturia_redsea.txt",header=T,sep="\t")
str(species)
a <- drm(Percentuale~water, data = species, Oxygen,
         fct = LL.4())


a <- drm(Percentuale~water, data = species, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

par(oma=c(2,2,0,2))
plot(a, col =  c("chartreuse4","mediumseagreen","lightpink1","indianred3","olivedrab3"), pch=16, xlab = "Temperature (C)", 
     ylab = "Mortality (%)",lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE,type = "confidence")
abline(h=50,lty=2)
plot(b, col =  c("chartreuse4","mediumseagreen","lightpink1","indianred3","olivedrab3"), pch=16, xlab = "Temperature (C)", 
     ylab = "Mortality (%)",lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE)


summary(a)
anova(a)


##disegno adesso il grafico LT50 con CI per tutte le specie
LT50<-read.table("LT50_figure_2.txt",header=T,sep="\t")
str(LT50)
# Use 95% confidence interval instead of SEM
summary(LT50)
LT50$Location<-factor(LT50$Location,levels = c("StAbbs","Chioggia","ReSea"),labels = c("Cold Temperate","Warm Temperate","Tropical"))


lt50_fig2<-ggplot(LT50, aes(x=species, y=LT50, colour=Oxygen)) + 
  geom_errorbar(aes(ymin=LT50-LT_95_CI, ymax=LT50+LT_95_CI), width=.5) +
  scale_color_manual(values=c("cyan4","brown1"))+
  geom_point(size=2) +
  facet_wrap(~Location,ncol=1)+
  coord_flip()+
  theme_classic() +
  theme(
   panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA) +ylim(0,40)
  ) +ylim(0,50)
ggsave("fig2_lt50.tiff", lt50_fig2, bg = "transparent")



###produzione grafici per ogni location

summary(LT50 )
chioggia<-subset(LT50, Location=='Chioggia')
                 
base <- ggplot(chioggia, aes(x=species, y=LT50, colour=Oxygen)) + 
  geom_errorbar(aes(ymin=LT50-LT_95_CI, ymax=LT50+LT_95_CI), width=.5) +
  scale_color_manual(values=c("cyan4","brown1"))+
  geom_point(size=2) +
  coord_flip()+
  theme_classic() +
  xlab("") + 
  ylab("") +
  ylim(0,40) +
  theme(
    axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
base

ggsave("chiogia.tiff", base, bg = "transparent")


