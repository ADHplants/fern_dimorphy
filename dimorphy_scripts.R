library(readxl)
library(tidyverse)
library(ggpubr)

##### Reading in data and preparing variables #####

#Water and light vs fertility data 
CovSwc <- read_excel(here::here("dimorphy and light.xlsx"), 
                     sheet = "hab pref with water", col_types = c("text", "numeric", "numeric", 
                                                                  "numeric", "numeric","text"))
CovSwc$Spp<-as.factor(CovSwc$Spp)
CovSwc$invest<-CovSwc$fert/(CovSwc$fert+CovSwc$ster)#investment ratio
CovSwc$inv2<-CovSwc$fert/CovSwc$ster
CovSwc$uncov<-100-(CovSwc$cov)
CovSwc$State<-as.factor(ifelse(CovSwc$fert==0,"Sterile","Fertile"))
CovSwc$total<-CovSwc$fert+CovSwc$ster

#Main biomass data
bio <- read_excel("~/UNDERC/dimorphy and light/dimorphy and light.xlsx", 
                  sheet = "biomass")
bio<-bio[which(bio$spp!="DryCri"),] #exclude cristata since collected seperately
bio$light<-as.factor(bio$light)
bio$spp<-as.factor(bio$spp)
bio$mass<-bio$above+bio$below
bio$fracb<-bio$below/(bio$above+bio$below)#fraction belowground biomass relative

#cristata data
cris <- read_excel("~/UNDERC/dimorphy and light/dimorphy and light.xlsx", 
                   sheet = "cristata")
cris$light<-as.factor(cris$light)
cris$lf_mass<-cris$f_mass+cris$s_mass
cris$stem_mass<-cris$s_stem+cris$f_stem
cris$rel_bel<-cris$b_mass/cris$tot_mass
cris$rel_lf<-cris$lf_mass/cris$tot_mass
cris$rel_stem<-cris$stem_mass/cris$tot_mass
cris$tot_lf<-cris$fert+cris$ster
cris$f_prop<-cris$fert/(cris$fert+cris$ster)


##### Plots #####

#biomass belowground
boxplot(bio$fracb~bio$light:bio$spp,col=c("white","dark gray"),
        xaxt="n",ylim=c(0.4,1),xlab="",ylab="Fraction of biomass belowground") #normal version
axis(side = 1,at=c(1.5,3.5,5.5,7.5,9.5),labels = c("AthFil","DryInt","OsmReg","OsmCla","OsmCin"))
legend(x="bottomright",c("High Light", "Low Light"),fill=c("white", "dark gray"))
title(main="Fraction of biomass belowground versus light")
text(x=c(1.5,3.5,5.5,7.5,9.5),y=c(1,1,1,1,1),labels = c("A***","B","AC","CD***","D"))

abline(v=c(4.5,8.5),lty=2)
text(x=c(2.5,6.5,9.7),y=c(1,1,1),labels = c("Monomorphic","Hemidimorphic","Holodimorphic"))





##### Stats #####

#Binomial logisitc regression

plot(CovSwc[CovSwc$Spp=="DryInt",]$invest~CovSwc[CovSwc$Spp=="DryInt",]$uncov)
y<-cbind(CovSwc[CovSwc$Spp=="DryInt",]$fert,CovSwc[CovSwc$Spp=="DryInt",]$ster)
glm1<-glm(y~trans2+swc+uncov,data = CovSwc[CovSwc$Spp=="DryInt",],family = quasibinomial)
glm2<-glm(y~swc+uncov,data = CovSwc[CovSwc$Spp=="DryInt",],family = quasibinomial)
anova(glm1,glm2)
plot(glm1)
summary(glm1)

plot(CovSwc[CovSwc$Spp=="AthFil",]$invest~CovSwc[CovSwc$Spp=="AthFil",]$uncov)
y2<-cbind(CovSwc[CovSwc$Spp=="AthFil",]$fert,CovSwc[CovSwc$Spp=="AthFil",]$ster)
glm2<-glm(y2 ~ uncov*swc, data = CovSwc[CovSwc$Spp=="AthFil",],family = quasibinomial())
summary(glm2)
anova(glm2)
plot(glm2)

plot(CovSwc[CovSwc$Spp=="OsmReg",]$swc,CovSwc[CovSwc$Spp=="OsmReg",]$uncov,xlim=c(0,100))
y3<-cbind(CovSwc[CovSwc$Spp=="OsmReg",]$fert,CovSwc[CovSwc$Spp=="OsmReg",]$ster)
glm3<-glm(y3 ~ uncov+swc, data = CovSwc[CovSwc$Spp=="OsmReg",],family = binomial())
summary(glm3)

plot(CovSwc[CovSwc$Spp=="OsmCla",]$invest,CovSwc[CovSwc$Spp=="OsmCla",]$uncov)
y4<-cbind(CovSwc[CovSwc$Spp=="OsmCla",]$fert,CovSwc[CovSwc$Spp=="OsmCla",]$ster)
glm4<-glm(y4 ~ uncov+swc, data = CovSwc[CovSwc$Spp=="OsmCla",],family = quasibinomial())
summary(glm4)

plot(CovSwc[CovSwc$Spp=="OsmCin",]$invest~CovSwc[CovSwc$Spp=="OsmCin",]$uncov)
y5<-cbind(CovSwc[CovSwc$Spp=="OsmCin",]$fert,CovSwc[CovSwc$Spp=="OsmCin",]$ster)
glm5<-glm(y5 ~ uncov+swc, data = CovSwc[CovSwc$Spp=="OsmCin",],family = quasibinomial())
summary(glm5)
