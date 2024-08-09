###prova con lethal dose
install.packages("drc")
library("drc") # dose response curves

setwd("~/Dropbox/KAUST/Projects/Hyperoxia/Manuscript/Analysis/")

#####all the file is from the mortality data contained in TabS2-4 provided with the Supplementary Data File 1 provided with the paper
caprella<-read.table("ChioggiaLT50_Caprella.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = caprella, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",xlim=c(21,50),lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, main=substitute(paste(italic("Caprella sp."))))
abline(h=50,lty=2)

summary(a)

gammarus<-read.table("ChioggiaLT50_Gammarus.txt",header=T,sep="\t")
b <- drm(LT50~T_.water, data = gammarus, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(b, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",xlim=c(21,50),lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Gammarus sp."))))
abline(h=50,lty=2)
summary(b)


haminoea<-read.table("ChioggiaLT50_Haminoea.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = haminoea, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Haminoea sp."))))
abline(h=50,lty=2)
summary(a)


mytilus<-read.table("ChioggiaLT50_Mytilus.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = mytilus, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Mytilus sp."))))
abline(h=50,lty=2)
summary(a)

ophitrix<-read.table("ChioggiaLT50_Ophitrix.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = ophitrix, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Ophitrix sp."))))
abline(h=50,lty=2)
summary(a)

paracentrotus<-read.table("ChioggiaLT50_Paracentrotus.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = paracentrotus, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Paracentrotus sp."))))
abline(h=50,lty=2)
summary(a)


#####mortalityredsea
aterina<-read.table("RedSeaLT50_Aterina.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = aterina, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,legendPos = c(31,100),lty=1,xlim=c(21,50),cex.axis=1.3,cex.lab=1.5,legend=FALSE,
     main=substitute(paste(italic("Atherinomorus sp."))))
abline(h=50,lty=2)

summary(a)
anova(a)

df<-read.table("RedSeaLT50_Damselfish.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = df, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Dascyllus sp."))))
abline(h=50,lty=2)
summary(a)

modo<-read.table("RedSeaLT50_Modiuolus.txt",header=T,sep="\t")
str(modo)
a <- drm(LT50~T_.water, data = modo, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Modiolus sp."))))
abline(h=50,lty=2)
summary(a)

ophio<-read.table("RedSeaLT50_Ophiocoma.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = ophio, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",xlim=c(21,50),lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Ophiocoma sp."))))
abline(h=50,lty=2)
summary(a)
olo<-read.table("RedSeaLT50_Holoturia.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = olo, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",xlim=c(21,50),lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Holoturia sp."))))
abline(h=50,lty=2)
summary(a)
tala<-read.table("RedSeaLT50_Thalamita.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = tala, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",xlim=c(21,50),lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Thalamita sp."))))
abline(h=50,lty=2)
summary(a)

#####mortality stabbs
echi<-read.table("StAbbs_LT50Echinus.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = echi, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,legendPos = c(31,100),lty=1,xlim=c(21,50),cex.axis=1.3,cex.lab=1.5,legend=FALSE,
     main=substitute(paste(italic("Echinus sp."))))
abline(h=50,lty=2)

summary(a)
anova(a)

os<-read.table("StAbbs_LT50Ostrea.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = os, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Ostrea sp."))))
abline(h=50,lty=2)
summary(a)

op<-read.table("StAbbs_LT50Ophiotorphix.txt",header=T,sep="\t")
str(modo)
a <- drm(LT50~T_.water, data = op, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",lwd=4,xlim=c(21,50),legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Ophiothrix sp."))))
abline(h=50,lty=2)


mi<-read.table("StAbbs_LT50Mytilus.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = mi, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",xlim=c(21,50),lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Mytilus sp."))))
abline(h=50,lty=2)

necora<-read.table("StAbbs_LT50Necora.txt",header=T,sep="\t")
a <- drm(LT50~T_.water, data = necora, Oxygen,  
         fct = LL.4(fixed=c(NA, NA, 100, NA), 
                    names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
par(oma=c(2,2,0,2))
plot(a, col =  c("brown2","cyan4"), pch=16, xlab = "Temperature (°C)", 
     ylab = "Mortality (%)",xlim=c(21,50),lwd=4,legendPos = c(31,100),lty=1,cex.axis=1.3,cex.lab=1.5,legend=FALSE, 
     main=substitute(paste(italic("Necora sp."))))
abline(h=50,lty=2)
summary(a)
