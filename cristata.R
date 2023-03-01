
#cristata playing around
library(readxl)
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

plot(cris$tot_lf~cris$light) #number of leaves does not predict investment in leaves
plot(cris$f_prop~cris$b_mass,col=cris$light,pch=19)

plot(rel_lf~tot_mass,data = cris,ylim=c(0,1),col="green")
abline(lm(rel_lf~tot_mass,data = cris))
points(rel_bel~tot_mass,data = cris,col="brown")
abline(lm(rel_bel~tot_mass,data = cris))
points(rel_stem~tot_mass,data = cris,col="blue")
abline(lm(rel_stem~tot_mass,data = cris))

cris$sla_ratio<-cris$s_sla_stem/cris$f_sla_stem
boxplot(sla_ratio~light,data = cris)

#cris fertile leaf proportion plot and stats
boxplot(cris$f_prop~cris$light,col=c("white","grey"),ylab = "Proportion of Leaves Fertile", 
        xlab = "Light Environment", main="Fertile Leaf Proportion vs Light Environment",ylim=c(0,1))
text(c(1,2),c(0.9,0.75),c("A","B"))
t.test(cris$f_prop[cris$light=="high"],cris$f_prop[cris$light=="low"],alternative = "greater")

#cris SLA plot and stats
cr_sla<-read_excel("~/UNDERC/dimorphy and light/dimorphy and light.xlsx", 
                   sheet = "cris R2")
cr_sla$light<-as.factor(cr_sla$light)
cr_sla$lfty<-as.factor(cr_sla$lfty)
cr1<-aov(SLA~light*lfty,data = cr_sla)
summary(cr1)
TukeyHSD(cr1)
boxplot(SLA~light+lfty,data = cr_sla, xaxt="n",col=c("white","grey","white","grey"),
        xlab = "Leaf Type",main="SLA of Fertile and Sterile Leaves in Shade vs Light",
        ylim=c(50,350))
axis(side = 1,at=c(1.5,3.5),labels = c("Fertile","Sterile"))
legend(4,89,legend =c("High","Low"),fill=c("white","grey"),title = "Light Environment")
abline(v=2.5,lty=2)
text(c(1,2,3,4),c(175,210,230,320),c("A","B","B","C"))



#cris biomass stuff
library(readxl)
crbm<-read_excel("~/UNDERC/dimorphy and light/dimorphy and light.xlsx", 
                 sheet = "cris R1")
boxplot(prop~light+sect,data=crbm) #broken down
boxplot(prop2~light+sect2,data = crbm,col=c("white","grey","white","grey","white","grey"),
        border=c("brown","brown","dark green","dark green","dark green","dark green"),
        xaxt="n",xlab = "Plant Tissue",ylab="Proportion of Biomass",
        main="Biomass Allocation Across Light Environment",ylim=c(0,1)) #above and below
legend(5.65,1,legend =c("High","Low"),fill=c("white","grey"),title = "Light Environment")
axis(side = 1,at=c(1.5,3.5,5.5),labels = c("Rhizome + Roots","Fertile Leaves","Sterile Leaves"))
text(c(1.5,3.5,5.5),c(0.92,0.65,0.43),c("***","***","n.s.d."))
lines(c(1,1,2,2),c(0.89,0.9,0.9,0.89))
lines(c(3,3,4,4),c(0.62,0.63,0.63,0.62))
lines(c(5,5,6,6),c(.4,.41,.41,.4))
abline(v=2.5,lty=2)

cr2<-aov(prop~light*sect2,data = crbm)
summary(cr2)
TukeyHSD(cr2)

#leaf area ratio (total leaf area over whole plant mass)
cris[is.na(cris)] = 0
cris$lar<-(cris$fla+cris$stla)/cris$tot_mass
plot(lar~light,data=cris)
#larm (leaf area to root mass ratio)
cris$larm<-(cris$fla+cris$stla)/cris$b_mass

##### clustering ####
library(factoextra)
library(FactoMineR)
pca1<-PCA(CovSwc[,6:7],graph = F)
fviz_pca_biplot(pca1, label = "var",  habillage = CovSwc$Spp,addEllipses = TRUE,col.var = "black",repel = TRUE)


####Ignore for now (only use if going to do contingency tests)####
row.names <- c("MatStr", "OsmCla", "OsmCin", "OnoSen", "AthFil", "DryInt")
col.names <- c("edge","interior")
light_pref <- matrix(c("insert ratios her by column"),nrow=6, dimnames =list(row.names,col.names))
light_pref
chisq.test(light_pref)

fisher.test(light_pref[0:2,0:2]) #ostrich vs interrupt for example
fisher.test(light_pref[0:2,0:2])