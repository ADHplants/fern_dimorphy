library(readxl)
library(tidyverse)
library(ggpubr)
library(lme4)

CovSwc <- read_excel("C:/Users/harr3281/Documents/UNDERC/dimorphy and light/dimorphy and light.xlsx", 
            sheet = "hab pref with water", col_types = c("text", "numeric", "numeric", 
                  "numeric", "numeric","text"))
CovSwc$Spp<-as.factor(CovSwc$Spp)
CovSwc$invest<-CovSwc$fert/(CovSwc$fert+CovSwc$ster)#investment ratio
CovSwc$inv2<-CovSwc$fert/CovSwc$ster
CovSwc$uncov<-100-(CovSwc$cov)
CovSwc$State<-as.factor(ifelse(CovSwc$fert==0,"Sterile","Fertile"))
CovSwc$total<-CovSwc$fert+CovSwc$ster

par(mfrow=c(1,1))
plot(CovSwc[CovSwc$Spp=="AthFil",]$invest~CovSwc[CovSwc$Spp=="AthFil",]$uncov)
plot(CovSwc[CovSwc$Spp=="DryInt",]$invest~CovSwc[CovSwc$Spp=="DryInt",]$uncov)
plot(CovSwc[CovSwc$Spp=="OsmReg",]$total~CovSwc[CovSwc$Spp=="OsmReg",]$uncov)
plot(CovSwc[CovSwc$Spp=="OsmCla",]$invest~CovSwc[CovSwc$Spp=="OsmCla",]$uncov)
plot(CovSwc[CovSwc$Spp=="OsmCin",]$invest~CovSwc[CovSwc$Spp=="OsmCin",]$uncov)
plot(CovSwc$inv2~CovSwc$total,col=CovSwc$Spp,pch=19)

plot(CovSwc$uncov,CovSwc$swc,col=CovSwc$Spp,pch=19) #water vs light plot, color by species
plot(CovSwc$uncov,CovSwc$invest, col=CovSwc$Spp, pch=19)#plots for can cover
plot(CovSwc$swc,CovSwc$invest, col=CovSwc$Spp, pch=19)# plots for soil water

ggscatter(CovSwc, x = "uncov", y = "invest",add="reg.line", palette = "jco", #linear models all together
          color = "Spp")


####Plots I probably wont use####
#nothing to see here yet more data must be gathered
ggscatter(CovSwc, x = "uncov", y = "invest",add="reg.line", palette = "jco", #linear models all together
          color = "Spp")
ggplot(data=CovSwc)+
  geom_point(mapping=aes(uncov,swc,color=Spp,size=invest))

ggscatter(CovSwc, x = "uncov", y = "swc", palette = "jco", #scatter with ellipses for all
          shape = "Spp",
          ellipse = T)



##Fertile vs sterile ellipses for all at once
a<-ggscatter(CovSwc, x = "uncov", y = "swc", palette = "ggplot2", #fert vs sterile ellipse
          shape = "State",
          ellipse = T)
a<-a+labs(x="Canopy Openness (%)",y="Soil Water Content (%)")
a<-a+facet_grid(rows=vars(Spp))
a


#stats for everything 

##### A) light and water vs investment (use gams within species there is no comparison across species)####
library(mblm) #this is for thielsen nonparametric regressions
library(mgcv) #this is for GAMs (not using)
gam.c
b1 <- gam(invest ~ s(uncov)+ s(uncov,swc), data = CovSwc[CovSwc$Spp=="AthFil",], method="REML", select=T)
b2 <- gam(invest ~ s(uncov)+ s(swc), data = CovSwc[CovSwc$Spp=="AthFil",], method="REML", select=TRUE)
summary(b1)
summary(b2)
plot(b1)
anova(b1,b2)
AIC(b1,b2)
gam.check(b1)
p

#thiel-sen
tsY<-CovSwc[CovSwc$Spp=="DryInt",]$invest
tsX<-CovSwc[CovSwc$Spp=="DryInt",]$uncov
plot(tsY~tsX)
summary(mblm(tsY~tsX))

CovSwc$trans<-as.factor(as.numeric(CovSwc$trans))


a<-lmer(invest~uncov+swc+(1|trans2),data = CovSwc[CovSwc$Spp=="DryInt",])

summary(a)
isSingular(a)
CovSwc %>% group_by(Spp)%>%count(trans2)

##GLM binomial logistic regression
plot(CovSwc[CovSwc$Spp=="DryInt",]$invest~CovSwc[CovSwc$Spp=="DryInt",]$uncov)
y<-cbind(CovSwc[CovSwc$Spp=="DryInt",]$fert,CovSwc[CovSwc$Spp=="DryInt",]$ster)
glm1<-glm(y~trans2+swc+uncov,data = CovSwc[CovSwc$Spp=="DryInt",],family = quasibinomial)
glm2<-glm(y~swc+uncov,data = CovSwc[CovSwc$Spp=="DryInt",],family = quasibinomial)
anova(glm1,glm2)
plot(glm1)
summary(glm1)

glm1$y


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

#figure for A
#ferile vs sterile colored points by investment
b<-ggpar(ggscatter(CovSwc, x = "uncov", y = "swc", #scatter plots colored by investmet
                   color = "invest") + gradient_color(c("light grey", "black")), xlim=c(0,100),ylim=c(0,100))+labs(title="Fertility vs Soil and Canopy Cover")+
  theme(plot.title = element_text(hjust = 0.5))
b<-b+facet_grid(rows=vars(Spp))+labs(x="Canopy Openness (%)",y="Soil Saturation (%)",color="Proportion Fertile")
dat_text <- data.frame(
  label = c("Model A", "Model C", "Model A", "Model C","Model B"), #a canopy only, #b canopy + water, #c all three
  Spp   = c("AthFil","DryInt","OsmCin","OsmCla","OsmReg"),
  x     = c(90,90,90,90,90),
  y     = c(10,10,10,10,10))
b + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label))

#means for A
CovSwc %>% 
  group_by(Spp) %>% 
  summarize(mean(fert), sd(fert),mean(ster), sd(ster),mean(invest),sd(invest))

#### B) Biomass stats and figures####

library(readxl)
bio <- read_excel("~/UNDERC/dimorphy and light/dimorphy and light.xlsx", 
                  sheet = "biomass")
bio$light<-as.factor(bio$light)
bio$spp<-as.factor(bio$spp)
bio$mass<-bio$above+bio$below

summary(lm(bio$above~bio$spp:bio$light))

bio$fracb<-bio$below/(bio$above+bio$below)#fraction belowground biomass relative

boxplot(bio$fracb~bio$light:bio$spp,col=c("white","dark gray"),
        xaxt="n",ylim=c(0.4,1),xlab="Species",ylab="Fraction of biomass belowground") #normal version
axis(side = 1,at=c(1.5,3.5,5.5,7.5,9.5),labels = c("AthFil","DryInt","OsmReg","OsmCla","OsmCin"))
legend(9.4,0.46,c("High Light", "Low Light"),fill=c("white", "dark gray"))
title(main="Fraction of biomass belowground versus light")
text(x=c(1.5,3.5,5.5,7.5,9.5),y=c(1,1,1,1,1),labels = c("A***","B","AC","CD***","D"))

plot(fracb~mass,data=bio[bio$spp=="DryInt",],col=light)
plot(fracb~mass,data=bio[bio$spp=="AthFil",],col=light)
plot(fracb~mass,data=bio[bio$spp=="OsmReg",],col=light)
plot(fracb~mass,data=bio[bio$spp=="OsmCla",],col=light)
plot(fracb~mass,data=bio[bio$spp=="OsmCin",],col=light)
ggplot(data = bio)+
  geom_point(aes(x=mass,y=fracb,color=light))+
  facet_grid(~spp,scales = "free_x")

abline(v=c(4.5,8.5),lty=2)
text(x=c(2.5,6.5,9.7),y=c(1,1,1),labels = c("Monomorphic","Hemidimorphic","Holodimorphic"))

aov1<-aov(fracb~spp*light,data=bio) #anova is inappropriate most likely, but passes assumption
summary(aov1)
shapiro.test(aov1$residuals)
plot(aov1)
tk1<-TukeyHSD(aov1)
tk1<-data.frame(tk1$`spp:light`)
tk1<-tk1[tk1$p.adj<0.05,]
tk1

library(betareg)
library(emmeans)
library(boot)
null<-betareg(fracb~1,data=bio)
breg1<-betareg(fracb~spp+light,data=bio)
breg2<-betareg(fracb~spp+light|light,data=bio)
breg3<-betareg(fracb~spp+light|spp+light,data=bio)
summary(breg1)
results1<-test(pairs(emmeans(breg1, ~spp, mode = "link")))
results2
results1[results1$p.value>0.05,]
lrtest(null,breg1,breg2,breg3)
AIC(null,aov1,breg1,breg2,breg3)
lrtest(aov1,breg1)
AIC(aov1,breg1)
#so i think I need to establish differences by species with breg1 (the best model according to AIC ad lrtest)
#then once I show differences by species, I can mark if there are differences within species pair

#####C) Hab preference ####
m1<-aov(uncov~Spp,data = CovSwc)
shapiro.test(m1$residuals)
library(FSA)
kruskal.test(uncov~Spp,data = CovSwc) #significant (summarized in table)
d1<-dunnTest(uncov~Spp,data = CovSwc)
d1$res[d1$res$P.adj<0.05,]#differences aa bb c
d1$res[d1$res$P.adj>0.05,]#same

kruskal.test(swc~Spp,data=CovSwc) #significant (summarized in table)
d2<-dunnTest(swc~Spp,data = CovSwc)
d2$res[d2$res$P.adj<0.05,]#differences aa bb c
d2$res[d2$res$P.adj>0.05,]#same

par(mfrow=c(1,2)) #figure for preferences
boxplot(CovSwc$uncov~CovSwc$Spp,ylim=c(0,100),col="white",xlab = "Species",ylab = "Canopy Openness (%)",main="A) Light Habitat")
text(x=c(1:5),y=c(40,40,90,100,102),labels =c("A","A","B","B","C"))
boxplot(CovSwc$swc~CovSwc$Spp,xlab = "Species",ylab = "Soil Saturation (%)",main="B) Water Habitat",ylim=c(20,100))
text(x=c(1:5),y=c(70,72,86,71,92),labels =c("A","A","B","A","B"))
par(mfrow=c(1,1))

