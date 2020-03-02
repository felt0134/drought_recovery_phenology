#effect size code
library(effsize)

#####extra###########
drad.gwc<-read.csv(file.choose(),header=TRUE)
head(drad.gwc)
drad.effsizes<-read.csv(file.choose(),header=TRUE)
head(drad.effsizes)

drad.gwc<-read.csv(file.choose(),header=TRUE)
head(drad.gwc)
head(drad.biomass)
summary(drad.biomass)
head(konza.summary)

#standard error function

ser <- function(x) {
  
  n <- sum(!is.na(x))
  se <- sd(x, na.rm = T)/sqrt(n)
  
  return(se)
}


hundred<- function(x) {
  
  new<-(x*100)
  
  return(new)
}



drad.gwc.new.2$new.name = factor(drad.gwc.new.2$plotid, levels=c("1","2","3","4"), labels=c('Mesic prairie','Mixed-grass prairie','Semi-arid steppe','Desert grassland'))


drad.gwc.new<-aggregate(newgwc~Day + Species + Treatment + plotid,mean,data=drad.gwc)
drad.gwc.new.2<-aggregate(newgwc~Day + Species + Treatment + plotid,hundred,data=drad.gwc.new)

library(ggplot2)
head(drad.gwc.new)
ggplot(drad.gwc.new.2,aes(Day,newgwc,color=Treatment,na.rm=TRUE)) +
  #geom_smooth(method = "lm", se = FALSE, linetype = "dashed",color="black",size=.5) +
  #geom_hline(yintercept=27,color="black") +
  #geom_hline(yintercept = 0,color="black",size=.5) +
  #ylab(bquote('Specific leaf area ('*cm^2/g*')')) +
  #ylab(bquote('Net photosynthesis ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) +
  ylab("% Gravimetric water content") +
  #ylab("Effect Size (Cohen's d)") +
  #stat_summary(fun.y="mean",geom="bar")
  #ggtitle("Net photosynthesis") +
  xlab("Day") +
  #geom_point(size=5,pch=21,position=dodge) +
  #geom_errorbar(aes(ymin=inf, ymax=sup), width=.5,position=dodge) +
  #stat_summary(geom="line",fun.y="mean",position=dodge) +
  #geom_errorbar(aes(ymin=SLA.x-SLA.y,ymax=SLA.x+SLA.y),position=dodge,width=.1) +
  #stat_summary(geom="point",fun.y="mean",size=1,aes(fill=as.factor(treatment)),shape=21,position=dodge) +
  stat_summary(geom="line",fun.y="mean",size=.75,aes(color=as.factor(Treatment)),position=dodge) +
  scale_color_manual(values=c('DR'='red','C'='blue'),name="") +
  #facet_grid(~Week,labeller = labeller(Week = wp_labeller )) +
  facet_wrap(~new.name,nrow=2) +
  #scale_x_discrete("Week",labels=c("1" = "Pre-drought","7"="Peak drought","11" = "Recovery")) +
  #geom_bar(stat="identity",color="black",fill="grey50",width=.5) +
  theme(
    axis.text.x = element_text(color='black',size=15),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=18),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=18),
    legend.text = element_text(size=15),
    legend.position = c(.07,.63),
    legend.key = element_blank(),
    panel.background = element_rect(fill=NA),
    #panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

ggsave("drad_gwc_plot.pdf",width = 8, height = 6, units = c("in"))

dodge<-position_dodge(width=.3)


######SLA Effect Sizes###########
head(drad.sla)

#bogr
bogr.sla.c<-subset(bogr.sla.2,Treatment=="C",na.rm=TRUE) #use one with outlier removed
bogr.sla.d<-subset(bogr.sla.2,Treatment=="DR",na.rm=TRUE)

#andro
andro.sla.c<-subset(andro.sla,Treatment=="C",na.rm=TRUE)
andro.sla.d<-subset(andro.sla,Treatment=="DR",na.rm=TRUE)

#boer
boer.sla.c<-subset(boer.sla,Treatment=="C",na.rm=TRUE)
boer.sla.d<-subset(boer.sla,Treatment=="DR",na.rm=TRUE)

#pasm
pasm.sla.c<-subset(pasm.sla.2,Treatment=="C",na.rm=TRUE)
pasm.sla.d<-subset(pasm.sla.2,Treatment=="DR",na.rm=TRUE)


#vectors: peak drought

#andro
andro.sla.d.peak<-subset(andro.sla.d,Time=="Peak drought",na.rm=TRUE)
andro.sla.d.peak.vec<-as.vector(andro.sla.d.peak$SLA)

andro.sla.c.peak<-subset(andro.sla.c,Time=="Peak drought",na.rm=TRUE)
andro.sla.c.peak.vec<-as.vector(andro.sla.c.peak$SLA)

cohen.d(andro.sla.d.peak.vec, andro.sla.c.peak.vec,pooled=TRUE,paired=FALSE,
         na.rm=TRUE, hedges.correction=FALSE,
         conf.level=0.95,noncentral=FALSE)

#bogr
bogr.sla.d.peak<-subset(bogr.sla.d,Time=="Peak drought",na.rm=TRUE)
bogr.sla.d.peak.vec<-as.vector(bogr.sla.d.peak$SLA)

bogr.sla.c.peak<-subset(bogr.sla.c,Time=="Peak drought",na.rm=TRUE)
bogr.sla.c.peak.vec<-as.vector(bogr.sla.c.peak$SLA)

#get effect size
cohen.d(bogr.sla.c.peak.vec, bogr.sla.d.peak.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#boer
boer.sla.d.peak<-subset(boer.sla.d,Time=="Peak drought",na.rm=TRUE)
boer.sla.d.peak.vec<-as.vector(boer.sla.d.peak$SLA)

boer.sla.c.peak<-subset(boer.sla.c,Time=="Peak drought",na.rm=TRUE)
boer.sla.c.peak.vec<-as.vector(boer.sla.c.peak$SLA)

#get effect size
cohen.d(boer.sla.d.peak.vec, boer.sla.c.peak.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#pasm
pasm.sla.d.peak<-subset(pasm.sla.d,Time=="Peak drought",na.rm=TRUE)
pasm.sla.d.peak.vec<-as.vector(pasm.sla.d.peak$SLA)

pasm.sla.c.peak<-subset(pasm.sla.c,Time=="Peak drought",na.rm=TRUE)
pasm.sla.c.peak.vec<-as.vector(pasm.sla.c.peak$SLA)

#get effect size
cohen.d(pasm.sla.d.peak.vec, pasm.sla.c.peak.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#Recovery
andro.sla.d.recovery<-subset(andro.sla.d,Time=="Recovery",na.rm=TRUE)
andro.sla.d.recovery.vec<-as.vector(andro.sla.d.recovery$SLA)

andro.sla.c.recovery<-subset(andro.sla.c,Time=="Recovery",na.rm=TRUE)
andro.sla.c.recovery.vec<-as.vector(andro.sla.c.recovery$SLA)

#get effect size
cohen.d(andro.sla.d.recovery.vec, andro.sla.c.recovery.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#bogr
bogr.sla.d.recovery<-subset(bogr.sla.d,Time=="Recovery",na.rm=TRUE)
bogr.sla.d.recovery.vec<-as.vector(bogr.sla.d.recovery$SLA)

bogr.sla.c.recovery<-subset(bogr.sla.c,Time=="Recovery",na.rm=TRUE)
bogr.sla.c.recovery.vec<-as.vector(bogr.sla.c.recovery$SLA)

#get effect size
cohen.d(bogr.sla.d.recovery.vec, bogr.sla.c.recovery.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#boer
boer.sla.d.recovery<-subset(boer.sla.d,Time=="Recovery",na.rm=TRUE)
boer.sla.d.recovery.vec<-as.vector(boer.sla.d.recovery$SLA)

boer.sla.c.recovery<-subset(boer.sla.c,Time=="Recovery",na.rm=TRUE)
boer.sla.c.recovery.vec<-as.vector(boer.sla.c.recovery$SLA)

#get effect size
cohen.d(boer.sla.d.recovery.vec, boer.sla.c.recovery.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#pasm
pasm.sla.c.recovery<-subset(pasm.sla.c,Time=="Recovery",na.rm=TRUE)
pasm.sla.c.recovery.vec<-as.vector(pasm.sla.c.recovery$SLA)

pasm.sla.d.recovery<-subset(pasm.sla.d,Time=="Recovery",na.rm=TRUE)
pasm.sla.d.recovery.vec<-as.vector(pasm.sla.d.recovery$SLA)

cohen.d(pasm.sla.d.recovery.vec, pasm.sla.c.recovery.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

########root:shoot#############
drad.biomass <-read.csv('G:/My Drive/Other projects/doe-drought-recovery/DOE_2019/data/DRAD_ANPP.csv')
head(drad.biomass)

drad.effect.rs.peak <-subset(drad.biomass,Time=="Peak drought")

#bogr
drad.effect.rs.peak.bogr <-subset(drad.effect.rs.peak ,Species=="B.gracilis")
drad.effect.rs.peak.bogr.vec<-as.vector(drad.effect.rs.peak.bogr$root.shoot)

#andro
drad.effect.rs.peak.andro <-subset(drad.effect.rs.peak ,Species=="A.gerardii")
drad.effect.rs.peak.andro.vec<-as.vector(drad.effect.rs.peak.andro$root.shoot)

#boer
drad.effect.rs.peak.boer <-subset(drad.effect.rs.peak ,Species=="B.eriopoda")
drad.effect.rs.peak.boer.vec<-as.vector(drad.effect.rs.peak.boer$root.shoot)

#pasm
drad.effect.rs.peak.pasm <-subset(drad.effect.rs.peak ,Species=="P.smithii")
drad.effect.rs.peak.pasm.vec <-as.vector(drad.effect.rs.peak.pasm$root.shoot)

#recovery
drad.effect.rs.recovery <-subset(drad.biomass,Time=="Recovery")

#bogr
drad.effect.rs.recovery.bogr <-subset(drad.effect.rs.recovery ,Species=="B.gracilis")
drad.effect.rs.recovery.bogr.vec <-as.vector(drad.effect.rs.recovery.bogr$root.shoot)

cohen.d(drad.effect.rs.recovery.bogr.vec,drad.effect.rs.peak.bogr.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#andro
drad.effect.rs.recovery.andro <-subset(drad.effect.rs.recovery ,Species=="A.gerardii")
drad.effect.rs.recovery.andro.vec <-as.vector(drad.effect.rs.recovery.andro$root.shoot)

cohen.d(drad.effect.rs.recovery.andro.vec,drad.effect.rs.peak.andro.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#boer
drad.effect.rs.recovery.boer <-subset(drad.effect.rs.recovery ,Species=="B.eriopoda")
drad.effect.rs.recovery.boer.vec <-as.vector(drad.effect.rs.recovery.boer$root.shoot)

cohen.d(drad.effect.rs.recovery.boer.vec,drad.effect.rs.peak.boer.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#pasm
drad.effect.rs.recovery.pasm <-subset(drad.effect.rs.recovery ,Species=="P.smithii")
drad.effect.rs.recovery.pasm.vec <-as.vector(drad.effect.rs.recovery.pasm$root.shoot)

cohen.d(drad.effect.rs.recovery.pasm.vec,drad.effect.rs.peak.pasm.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#####ANPP#######

drad.effect.rs.peak <-subset(drad.biomass,Time=="Peak drought")

drad.effect.rs.peak.bogr <-subset(drad.effect.anpp.peak ,Species=="B.gracilis")
drad.effect.rs.peak.bogr.vec<-as.vector(drad.effect.anpp.peak.bogr$ANPP)

drad.effect.rs.peak.andro <-subset(drad.effect.anpp.peak ,Species=="A.gerardii")
drad.effect.rs.peak.andro.vec<-as.vector(drad.effect.anpp.peak.andro$ANPP)

drad.effect.rs.peak.boer <-subset(drad.effect.anpp.peak ,Species=="B.eriopoda")
drad.effect.rs.peak.boer.vec<-as.vector(drad.effect.anpp.peak.boer$ANPP)

drad.effect.rs.peak.pasm <-subset(drad.effect.anpp.peak ,Species=="P.smithii")
drad.effect.anpp.peak.pasm.vec <-as.vector(drad.effect.anpp.peak.pasm$ANPP)

#recovery
drad.effect.rs.recovery <-subset(drad.biomass,Time=="Recovery")

drad.effect.anpp.recovery.bogr <-subset(drad.effect.anpp.recovery ,Species=="B.gracilis")
drad.effect.anpp.recovery.bogr.vec <-as.vector(drad.effect.anpp.recovery.bogr$ANPP)

drad.effect.anpp.recovery.andro <-subset(drad.effect.anpp.recovery ,Species=="A.gerardii")
drad.effect.anpp.recovery.andro.vec <-as.vector(drad.effect.anpp.recovery.andro$ANPP)

drad.effect.anpp.recovery.boer <-subset(drad.effect.anpp.recovery ,Species=="B.eriopoda")
drad.effect.anpp.recovery.boer.vec <-as.vector(drad.effect.anpp.recovery.boer$ANPP)

drad.effect.anpp.recovery.pasm <-subset(drad.effect.anpp.recovery ,Species=="P.smithii")
drad.effect.anpp.recovery.pasm.vec <-as.vector(drad.effect.anpp.recovery.pasm$ANPP)

##Morphology

#########area per leaf#########

#from the mixed effetcs script. Inclusive of outlier removals

#week 7

#bogr
leaf.size.week7.bogr.dr<-subset(leaf.bogr.week.7,treatment=="DR")
leaf.size.week7.bogr.dr.vec <-as.vector(leaf.size.week7.bogr.dr$average)
leaf.size.dr.week7.bogr.c<-subset(leaf.bogr.week.7,treatment=="C")
leaf.size.dr.week7.bogr.c.vec <-as.vector(leaf.size.dr.week7.bogr.c$average)

cohen.d(leaf.size.week7.bogr.dr.vec, leaf.size.dr.week7.bogr.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#andro
leaf.size.week7.andro.dr<-subset(andro.leaf.size.week.7,treatment=="DR")
leaf.size.week7.andro.dr.vec <-as.vector(leaf.size.week7.andro.dr$average)
leaf.size.dr.week7.andro.c<-subset(andro.leaf.size.week.7,treatment=="C")
leaf.size.dr.week7.andro.c.vec <-as.vector(leaf.size.dr.week7.andro.c$average)

cohen.d(leaf.size.week7.andro.dr.vec, leaf.size.dr.week7.andro.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#boer
leaf.size.week7.boer.dr<-subset(leaf.boer.week.7,treatment=="DR")
leaf.size.week7.boer.dr.vec <-as.vector(leaf.size.week7.boer.dr$average)
leaf.size.dr.week7.boer.c<-subset(leaf.boer.week.7,treatment=="C")
leaf.size.dr.week7.boer.c.vec <-as.vector(leaf.size.dr.week7.boer.c$average)

cohen.d(leaf.size.week7.boer.dr.vec, leaf.size.dr.week7.boer.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#pasm
leaf.size.week7.pasm.dr<-subset(leaf.pasm.week.7.2,treatment=="DR")
leaf.size.week7.pasm.dr.vec <-as.vector(leaf.size.week7.pasm.dr$average)
leaf.size.dr.week7.pasm.c<-subset(leaf.pasm.week.7.2,treatment=="C")
leaf.size.dr.week7.pasm.c.vec <-as.vector(leaf.size.dr.week7.pasm.c$average)

cohen.d(leaf.size.week7.pasm.dr.vec, leaf.size.dr.week7.pasm.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#week11

#bogr
leaf.size.week11.bogr.dr<-subset(leaf.bogr.week.11,treatment=="DR")
leaf.size.week11.bogr.dr.vec <-as.vector(leaf.size.week11.bogr.dr$average)
leaf.size.dr.week11.bogr.c<-subset(leaf.bogr.week.11,treatment=="C")
leaf.size.dr.week11.bogr.c.vec <-as.vector(leaf.size.dr.week11.bogr.c$average)

cohen.d(leaf.size.week11.bogr.dr.vec, leaf.size.dr.week11.bogr.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#andro
leaf.size.week11.andro.dr<-subset(andro.leaf.size.week.11,treatment=="DR")
leaf.size.week11.andro.dr.vec <-as.vector(leaf.size.week11.andro.dr$average)
leaf.size.dr.week11.andro.c<-subset(andro.leaf.size.week.11,treatment=="C")
leaf.size.dr.week11.andro.c.vec <-as.vector(leaf.size.dr.week11.andro.c$average)

cohen.d(leaf.size.week11.andro.dr.vec, leaf.size.dr.week11.andro.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#boer
leaf.size.week11.boer.dr<-subset(leaf.boer.week.11,treatment=="DR")
leaf.size.week11.boer.dr.vec <-as.vector(leaf.size.week11.boer.dr$average)
leaf.size.dr.week11.boer.c<-subset(leaf.boer.week.11,treatment=="C")
leaf.size.dr.week11.boer.c.vec <-as.vector(leaf.size.dr.week11.boer.c$average)

cohen.d(leaf.size.week11.boer.dr.vec, leaf.size.dr.week11.boer.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#pasm
leaf.size.week11.pasm.dr<-subset(leaf.pasm.week.11,treatment=="DR")
leaf.size.week11.pasm.dr.vec <-as.vector(leaf.size.week11.pasm.dr$average)
leaf.size.dr.week11.pasm.c<-subset(leaf.pasm.week.11,treatment=="C")
leaf.size.dr.week11.pasm.c.vec <-as.vector(leaf.size.dr.week11.pasm.c$average)

cohen.d(leaf.size.week11.pasm.dr.vec, leaf.size.dr.week11.pasm.c.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#####stem number########
stem.ag.morph<-aggregate(tillers~Plant + Species + Block + Week + treatment,mean,data=drad.morph)
View(stem.ag.morph)

stem.ag.morph.2 <-stem.ag.morph %>% dplyr::filter(!(Plant==c('BOGR29'))) %>%
  dplyr::filter(!(Plant==c('ANGE29'))) #remove unclear plant ids

stem.ag.morph.dr<-subset(stem.ag.morph.2,treatment=='DR')

#andro
stem.dr.andro <- subset(stem.ag.morph.dr,Species=='A.gerardii')
stem.dr.andro.7 <- subset(stem.dr.andro, Week=='7')
stem.dr.andro.11 <- subset(stem.dr.andro, Week=='11')
stem.dr.andro.7.vec <-as.vector(stem.dr.andro.7$tillers)
stem.dr.andro.11.vec <-as.vector(stem.dr.andro.11$tillers)

cohen.d(stem.dr.andro.11.vec, stem.dr.andro.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#bogr
stem.dr.bogr <- subset(stem.ag.morph.dr,Species=='B.gracilis')
stem.dr.bogr.7 <- subset(stem.dr.bogr, Week=='7')
stem.dr.bogr.11 <- subset(stem.dr.bogr, Week=='11')
stem.dr.bogr.7.vec <-as.vector(stem.dr.bogr.7$tillers)
stem.dr.bogr.11.vec <-as.vector(stem.dr.bogr.11$tillers)

cohen.d(stem.dr.bogr.11.vec, stem.dr.bogr.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)
#boer
stem.dr.boer <- subset(stem.ag.morph.dr,Species=='B.eriopoda')
stem.dr.boer.7 <- subset(stem.dr.boer, Week=='7')
stem.dr.boer.11 <- subset(stem.dr.boer, Week=='11')
stem.dr.boer.7.vec <-as.vector(stem.dr.boer.7$tillers)
stem.dr.boer.11.vec <-as.vector(stem.dr.boer.11$tillers)

cohen.d(stem.dr.boer.11.vec, stem.dr.boer.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#pasm
stem.dr.pasm <- subset(stem.ag.morph.dr,Species=='P.smithii')
stem.dr.pasm.7 <- subset(stem.dr.pasm, Week=='7')
stem.dr.pasm.11 <- subset(stem.dr.pasm, Week=='11')
stem.dr.pasm.7.vec <-as.vector(stem.dr.pasm.7$tillers)
stem.dr.pasm.11.vec <-as.vector(stem.dr.pasm.11$tillers)

cohen.d(stem.dr.pasm.11.vec, stem.dr.pasm.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

######leaf number#########
green.leaf.ag.morph<-aggregate(green.leaves~Plant + Species + Block + Week + treatment,mean,data=drad.morph)
View(green.leaf.ag.morph)

green.leaf.ag.morph.2 <- green.leaf.ag.morph %>% dplyr::filter(!(Plant==c('BOGR29'))) %>%
  dplyr::filter(!(Plant==c('ANGE29'))) #remove unclear plant ids

green.leaf.ag.morph.dr<-subset(green.leaf.ag.morph.2,treatment=='DR')

#andro
green.leaf.dr.andro <- subset(green.leaf.ag.morph.dr,Species=='A.gerardii')
green.leaf.dr.andro.7 <- subset(green.leaf.dr.andro, Week=='7')
green.leaf.dr.andro.11 <- subset(green.leaf.dr.andro, Week=='11')
green.leaf.dr.andro.7.vec <-as.vector(green.leaf.dr.andro.7$green.leaves)
green.leaf.dr.andro.11.vec <-as.vector(green.leaf.dr.andro.11$green.leaves)

cohen.d(green.leaf.dr.andro.11.vec, green.leaf.dr.andro.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#bogr
green.leaf.dr.bogr <- subset(green.leaf.ag.morph.dr,Species=='B.gracilis')
green.leaf.dr.bogr.7 <- subset(green.leaf.dr.bogr, Week=='7')
green.leaf.dr.bogr.11 <- subset(green.leaf.dr.bogr, Week=='11')
green.leaf.dr.bogr.7.vec <-as.vector(green.leaf.dr.bogr.7$green.leaves)
green.leaf.dr.bogr.11.vec <-as.vector(green.leaf.dr.bogr.11$green.leaves)

cohen.d(green.leaf.dr.bogr.11.vec, green.leaf.dr.bogr.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)
#boer
green.leaf.dr.boer <- subset(green.leaf.ag.morph.dr,Species=='B.eriopoda')
green.leaf.dr.boer.7 <- subset(green.leaf.dr.boer, Week=='7')
green.leaf.dr.boer.11 <- subset(green.leaf.dr.boer, Week=='11')
green.leaf.dr.boer.7.vec <-as.vector(green.leaf.dr.boer.7$green.leaves)
green.leaf.dr.boer.11.vec <-as.vector(green.leaf.dr.boer.11$green.leaves)

cohen.d(green.leaf.dr.boer.11.vec, green.leaf.dr.boer.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#pasm
green.leaf.dr.pasm <- subset(green.leaf.ag.morph.dr,Species=='P.smithii')
green.leaf.dr.pasm.7 <- subset(green.leaf.dr.pasm, Week=='7')
green.leaf.dr.pasm.11 <- subset(green.leaf.dr.pasm, Week=='11')
green.leaf.dr.pasm.7.vec <-as.vector(green.leaf.dr.pasm.7$green.leaves)
green.leaf.dr.pasm.11.vec <-as.vector(green.leaf.dr.pasm.11$green.leaves)

cohen.d(green.leaf.dr.pasm.11.vec, green.leaf.dr.pasm.7.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

####extra#######

#b.gracilis
bogr.leaves.merge<-merge(drad.ag.morph.green.leaves.dr.week11.bogr,drad.ag.morph.green.leaves.dr.week9.bogr,by=c("Week"))


#assess effect sizes
?effsize
library(effsize)
?effsize
cohen.d( boer.d.11.A.vec, boer.c.11.A.vec,pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)




##ggplot

outlier.test(drad.ag.morph.green.leaves.dr.week11.boer.vec)
#leaf.area subset


drad.effsizes<-read.csv(file.choose(),header=TRUE)
head(drad.effsizes)
morphology.ggplot <-subset(drad.effsizes,subgroup=="morphology")
head(morphology.ggplot)
sla.ggplot <-subset(drad.effsizes,subgroup=="SLA")
head(sla.ggplot)
gas.ex.ggplot <-subset(drad.effsizes,subgroup=="gas.exchange")
gas.ex.ggplot.A <-subset(drad.effsizes,Process=="A")
wp.ggplot<-subset(drad.effsizes,Process=="WP")
head(wp.ggplot)
root.shoot.ggplot<-subset(drad.effsizes,Process=="root:shoot")

effect.ag.a<-aggregate(effect~Week + Species + inf + sup,mean,data=gas.ex.ggplot.A)


gas.ex.ggplot.A.pasm <-subset(gas.ex.ggplot.A ,Species=="P.smithii")

t.test(drad.effect.rs.recovery.bogr.vec,drad.effect.rs.peak.bogr.vec )

library(ggplot2)

limits <- aes(ymax =  sup, ymin= inf)
dodge<-position_dodge(width=.3)

scale
summary(drad.gwc)
?t.test
ggplot(root.shoot.ggplot,aes(as.factor(ID),as.numeric(effect),na.rm=TRUE)) +
  geom_hline(yintercept = 0,color="black")  +
  geom_errorbar(limits,width=0.08) + #needs to be treated as a factor for dodge to work
  #scale_x_discrete(labels=c("1" = "Pre-drought","7" = "Peak drought", "11" ="Recovery")) +
  #geom_point(size=3,pch=21,color=black,position=dodge) +
  #geom_point(size=3.5,shape=21,aes(fill=Species),position=dodge) +
  #stat_summary(geom="line",fun.y="mean",aes(color=as.factor(Treatment)),position=dodge) +
  #stat_summary(geom="point",fun.y="mean",size=3,color="black",aes(fill=as.factor(Treatment)),shape=21) +
  stat_summary(geom="point",fun.y="mean",size=6,color="black") +
  #scale_colour_manual(values=c('DR'='red','C'='blue'),name="") +
  #geom_line(aes(fill=Species)) +
  #stat_summary(geom="point",fun.y="mean",size=5,color="black",position=dodge,aes(fill=as.factor(ID)),shape=21) +
  scale_fill_manual(values=c('1'='blue','2'='darkgreen','3'='orange','4'='red'),name="",
  labels=c('3'='Semi-arid steppe','1'='Mesic prairie','2'='Mixed grass prairie','4'='Desert grassland')) +
  #ylab(bquote('SLA ('*cm^2/g*') effect size')) +
  ylab("Post-drought change in root:shoot") +
  scale_x_discrete(limit=c("1","2","3", "4"), labels=c("Mesic prairie","Mixed grass prairie", "Semi-arid steppe","Desert grassland")) +
  #ggtitle("Net photosynthesis") +
  #facet_wrap(~Process) +
  xlab("") +
  #coord_flip() +
  theme(
    #axis.text.x = element_text(color='black',size=22),
    axis.text.x = element_text(color='black',size=20,angle=45,hjust=1),
    axis.text.y = element_text(color='black',size=14),
    axis.title = element_text(color='black',size=18),
    axis.ticks = element_line(color='black'),
    legend.title = element_blank(),
    #legend.text = element_blank(),
    legend.text = element_text(size=20),
    legend.key = element_blank(),
    #legend.position = "none",
    legend.position = c(.22,.25),
    panel.background = element_rect(fill=NA),
    strip.background = element_blank(),
    #panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

ggsave("root.shoot.effsizes.plot.pdf",width = 8, height = 7.5, units = c("in"))


