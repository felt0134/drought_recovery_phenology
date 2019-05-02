#linear mixed effects
library(nlme)
library(car)

#photosynthesis
drad.phys<-read.csv('C:/Users/A02296270/Desktop/Side_projects/DOE/doe_greenhouse_experiment_data/DRAD_GAS_EXCHANGE.csv',header=TRUE)
head(drad.phys)

#look at distribtuion of data
hist(drad.phys$A)

#models
drought<-subset(drad.phys,subset=="Drought")
hist(drought$A)
recovery<-subset(drad.phys,subset.recovery=="Recovery")
hist(recovery$A)

#full no ar term
full.A<-lme(A~Species*treatment*Week,random=~1|Block/Plant,data=drad.phys,na.action = na.exclude)
summary(full.A)
Anova(full.A,type=3)
#full.ar term
full.A.ar<-lme(A~Species*treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1),data=drad.phys,na.action = na.exclude)
summary(full.A.ar)
Anova(full.A.ar,type=3)
#see if an autoregressive term matters
anova(full.A.ar, full.A) #it does, include it

a.means<-aggregate(A~Species + Week + subset,mean,data=drad.phys)

#put everything in relatvistic terms to standardize
head(drad.phys)
plot(A~gsw,data=drad.phys)

controls<-subset(drad.phys,treatment=='C')
control.A<-aggregate(A~Species + Week + subset + subset.recovery,mean,data=controls)
controls.averaged.merged<-merge(drad.phys,control.A,by=c('Species','Week','subset','subset.recovery'))
controls.averaged.merged$a.perc <- ((controls.averaged.merged$A.x - controls.averaged.merged$A.y)/controls.averaged.merged$A.y)*100

final.a.average<-aggregate(a.perc~Week + Species,mean,data=controls.averaged.merged)
final.a.sd<-aggregate(a.perc~Week + Species,ser,data=controls.averaged.merged)
merge.final.a<-merge(final.a.average,final.a.sd,by=c('Week','Species'))

#seperate by 
full.drought.A<-lme(A~Species*treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1),data=drought,na.action = na.exclude)
Anova(full.drought.A,type=3)
#all interactions singificant except the three-way interaction. 

full.recovery.A<-lme(A~Species*treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1),data=recovery,na.action = na.exclude)
Anova(full.recovery.A,type=3)
#Species all recovered similarly, with respect to species*treatment and species*week, not significant

#separatation by species

#b.gracilis
b.gracilis<-subset(drad.phys,Species=="B.gracilis")
b.gracilis.drought<-subset(b.gracilis,subset=="Drought")
b.gracilis.recovery<-subset(b.gracilis,subset.recovery=="Recovery")

#total experiment
full.b.gracilis<-lme(A~treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1),data=b.gracilis,na.action = na.exclude)
Anova(full.b.gracilis,type=3)

#drought full model
full.b.gracilis.drought<-lme(A~treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1),data=b.gracilis.drought,na.action = na.exclude)
Anova(full.b.gracilis.drought,type=3)
#treatment*week highly signiciant, treatment weakly significant (p =.08)

#recovery full model
full.b.gracilis.recovery<-lme(A~treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1),data=b.gracilis.recovery,na.action = na.exclude)
Anova(full.b.gracilis.recovery,type=3)
plot(resid(full.b.gracilis.recovery))

#Week1bogr
bogr.week.1<-subset(b.gracilis,Week=="1",na.rm=TRUE)
bogr.week1.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.1,na.action = na.exclude)
plot(resid(bogr.week1.lm.a))
anova(bogr.week1.lm.a)
#n.s.
#Week3bogr
bogr.week.3<-subset(b.gracilis,Week=="3",na.rm=TRUE)
hist(bogr.week.3$A)
bogr.week3.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.3,na.action = na.exclude)
plot(resid(bogr.week3.lm.a))
anova(bogr.week3.lm.a)
#n.s.

#Week5bogr
bogr.week.5<-subset(b.gracilis,Week=="5",na.rm=TRUE)
bogr.week5.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.5,na.action = na.exclude)
anova(bogr.week5.lm.a)
#n.s., p-value = .0956

#Week7bogr
bogr.week.7<-subset(b.gracilis,Week=="7",na.rm=TRUE)
bogr.week7.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.7,na.action = na.exclude)
anova(bogr.week7.lm.a)
#*** significant

#Week8bogr
bogr.week.8<-subset(b.gracilis,Week=="8",na.rm=TRUE)
bogr.week8.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.8,na.action = na.exclude)
anova(bogr.week8.lm.a)
#n.s.

#Week9bogr
bogr.week.9<-subset(b.gracilis,Week=="9",na.rm=TRUE)
bogr.week9.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.9,na.action = na.exclude)
anova(bogr.week9.lm.a)
#n.s.

#Week10bogr
bogr.week.10<-subset(b.gracilis,Week=="10",na.rm=TRUE)
bogr.week10.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.10,na.action = na.exclude)
anova(bogr.week10.lm.a)
#n.s.

#Week11bogr
bogr.week.11<-subset(b.gracilis,Week=="11",na.rm=TRUE)
bogr.week11.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.11,na.action = na.exclude)
anova(bogr.week11.lm.a)
#n.s.
#one week to recover

#A.gerardii
A.gerardii<-subset(drad.phys,Species=="A.gerardii")
A.gerardii.drought<-subset(A.gerardii,subset=="Drought")
A.gerardii.recovery<-subset(A.gerardii,subset.recovery=="Recovery")

full.a.gerardii<-lme(A~treatment*Week,random=~1|Block/Plant,data=A.gerardii,na.action = na.exclude)
Anova(full.a.gerardii,type=3)
#only treatment signficant in full model

#drought full model
full.A.gerardii.drought<-lme(A~treatment*Week,random=~1|Block/Plant,data=A.gerardii.drought,na.action = na.exclude)
Anova(full.A.gerardii.drought,type=3)
#treatment*week signiciant

#recovery full model
full.A.gerardii.recovery<-lme(A~treatment*Week,random=~1|Block/Plant,data=A.gerardii.recovery,na.action = na.exclude)
Anova(full.A.gerardii.recovery,type=3)
#treatment*week significant

#week1.andro
andro.week.1.a<-subset(A.gerardii,Week=="1",na.rm=TRUE)
andro.week1.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.1.a,na.action = na.exclude)
anova(andro.week1.lm.a)
#n.s.

#week3.andro
andro.week.3.a<-subset(A.gerardii,Week=="3",na.rm=TRUE)
andro.week3.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.3.a,na.action = na.exclude)
anova(andro.week3.lm.a)
#n.s.

#week5.andro
andro.week.5.a<-subset(A.gerardii,Week=="5",na.rm=TRUE)
andro.week5.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.5.a,na.action = na.exclude)
anova(andro.week5.lm.a)
#**signiciant

#week7.andro
andro.week.7.a<-subset(A.gerardii,Week=="7",na.rm=TRUE)
andro.week7.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.7.a,na.action = na.exclude)
anova(andro.week7.lm.a)
#***signiciant

#week8.andro
andro.week.8.a<-subset(A.gerardii,Week=="8",na.rm=TRUE)
andro.week8.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.8.a,na.action = na.exclude)
anova(andro.week8.lm.a)
#**signiciant

#week9.andro
andro.week.9.a<-subset(A.gerardii,Week=="9",na.rm=TRUE)
andro.week9.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.9.a,na.action = na.exclude)
anova(andro.week9.lm.a)
#n.s.

#week10.andro
andro.week.10.a<-subset(A.gerardii,Week=="10",na.rm=TRUE)
andro.week10.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.10.a,na.action = na.exclude)
anova(andro.week10.lm.a)
#n.s.

#week11.andro
andro.week.11.a<-subset(A.gerardii,Week=="11",na.rm=TRUE)
andro.week11.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.11.a,na.action = na.exclude)
anova(andro.week11.lm.a)
#n.s.
#two weeks for andorpogon to recover to level


#P.smithii
P.smithii<-subset(drad.phys,Species=="P.smithii")
p.smithii.drought<-subset(P.smithii,subset=="Drought")
p.smithii.recovery<-subset(P.smithii,subset.recovery=="Recovery")

full.p.smithii<-lme(A~treatment*Week,random=~1|Block/Plant,data=P.smithii,na.action = na.exclude)
Anova(full.p.smithii,type=3)
#everything significant

#drought full model
full.p.smithii.drought<-lme(A~treatment*Week,random=~1|Block/Plant,data=p.smithii.drought,na.action = na.exclude)
Anova(full.p.smithii.drought)
#everything significant

#recovery full model
full.p.smithii.recovery<-lme(A~treatment*Week,random=~1|Block/Plant,data=p.smithii.recovery,na.action = na.exclude)
Anova(full.p.smithii.recovery)
#signiciant interaction, treatment not signficant

#week1.pasm
pasm.week.1<-subset(P.smithii,Week=="1",na.rm=TRUE)
pasm.week1.lm.a<-lme(A~treatment,data=pasm.week.1,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week1.lm.a)
#n.s., but .054

#week3.pasm
pasm.week.3<-subset(P.smithii,Week=="3",na.rm=TRUE)
pasm.week3.lm.a<-lme(A~treatment,data=pasm.week.3,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week3.lm.a)
#extremely signiciant

#week5.pasm
pasm.week.5<-subset(P.smithii,Week=="5",na.rm=TRUE)
pasm.week5.lm.a<-lme(A~treatment,data=pasm.week.5,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week5.lm.a)
#signiciant 

#week7.pasm
pasm.week.7<-subset(P.smithii,Week=="7",na.rm=TRUE)
pasm.week7.lm.a<-lme(A~treatment,data=pasm.week.7,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week7.lm.a)
#significant

#week8.pasm
pasm.week.8<-subset(P.smithii,Week=="8",na.rm=TRUE)
pasm.week8.lm.a<-lme(A~treatment,data=pasm.week.8,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week8.lm.a)
#signiciant 

#week9.pasm
pasm.week.9<-subset(P.smithii,Week=="9",na.rm=TRUE)
pasm.week9.lm.a<-lme(A~treatment,data=pasm.week.9,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week9.lm.a)
#signiciant (PD higher than C)

#week10.pasm
pasm.week.10<-subset(P.smithii,Week=="10",na.rm=TRUE)
pasm.week10.lm.a<-lme(A~treatment,data=pasm.week.10,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week10.lm.a)
summary(pasm.week.10)
m<-aggregate(A~treatment,mean,data=pasm.week.10)
s<-aggregate(A~treatment,sd,data=pasm.week.10)
#n.s.

#week11.pasm
pasm.week.11<-subset(P.smithii,Week=="11",na.rm=TRUE)
pasm.week11.lm.a<-lme(A~treatment,data=pasm.week.11,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week11.lm.a)
#signiciant

#B.eriopoda
B.eriopoda<-subset(drad.phys,Species=="B.eriopoda")
B.eriopoda.drought<-subset(B.eriopoda,subset=="Drought")
B.eriopoda.recovery<-subset(B.eriopoda,subset.recovery=="Recovery")

full.B.eriopoda<-lme(A~treatment*Week,random=~1|Block/Plant,data=B.eriopoda,na.action = na.exclude)
Anova(full.a.gerardii,type=3)
#treatment signiciant, treatment*week not

#drought full model
full.B.eriopoda.drought<-lme(A~treatment*Week,random=~1|Block/Plant,data=B.eriopoda.drought,na.action = na.exclude)
#all signAnova(full.B.eriopoda.drought)

#recovery full model
full.B.eriopoda.recovery<-lme(A~treatment*Week,random=~1|Block/Plant,data=B.eriopoda.recovery,na.action = na.exclude)
Anova(full.B.eriopoda.recovery)

#week1.boer
boer.week.1<-subset(B.eriopoda,Week=="1",na.rm=TRUE)
boer.week1.lm.a<-lme(A~treatment,data=boer.week.1,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week1.lm.a)
#n.s.

#week3.boer
boer.week.3<-subset(B.eriopoda,Week=="1",na.rm=TRUE)
boer.week3.lm.a<-lme(A~treatment,data=boer.week.3,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week3.lm.a)
#n.s.

#week5.boer
boer.week.5<-subset(B.eriopoda,Week=="5",na.rm=TRUE)
boer.week5.lm.a<-lme(A~treatment,data=boer.week.5,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week5.lm.a)
#signiciant

#week7.boer
boer.week.7<-subset(B.eriopoda,Week=="7",na.rm=TRUE)
boer.week7.lm.a<-lme(A~treatment,data=boer.week.7,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week7.lm.a)
#signiciant

#week8.boer
boer.week.8<-subset(B.eriopoda,Week=="8",na.rm=TRUE)
boer.week8.lm.a<-lme(A~treatment,data=boer.week.8,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week8.lm.a)
#signiciant

#week9.boer
boer.week.9<-subset(B.eriopoda,Week=="9",na.rm=TRUE)
boer.week9.lm.a<-lme(A~treatment,data=boer.week.9,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week9.lm.a)
#n.s.

#week10.boer
boer.week.10<-subset(B.eriopoda,Week=="10",na.rm=TRUE)
boer.week10.lm.a<-lme(A~treatment,data=boer.week.10,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week10.lm.a)
#n.s.

#week11.boer
boer.week.11<-subset(B.eriopoda,Week=="11",na.rm=TRUE)
boer.week11.lm.a<-lme(A~treatment,data=boer.week.11,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week11.lm.a)

#check to see abot the relationship between flor and A
A.phips2.model.lm<-lm(A~phips2,data=drad.phys,na.action = na.exclude)
summary(A.phips2.model.lm)
#R-square=.61

A.phips2.model<-lme(A~phips2*Species,random=~1|Block/Plant,data=drad.phys,na.action = na.exclude)
Anova(A.phips2.model,type=3)

#flor
drad.phys<-read.csv(file.choose(),header=TRUE)
head(drad.phys)

full<-lme(phips2~Species*treatment*Week,random=~1|Block/Plant,data=drad.phys,na.action = na.exclude)
summary(full)

#type I
Anova(full)

#separatation by species due to species-level difference, no significant differences in species*treatment effect, though

#b.gracilis
b.gracilis.phi<-subset(drad.phys,Species=="B.gracilis")
head(b.gracilis)
summary(b.gracilis)

#full
full.b.gracilis.phi<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=b.gracilis.phi,na.action = na.exclude)
Anova(full.b.gracilis.phi,type=3)
#only signiciant effect of week

b.gracilis.drought.phi<-subset(b.gracilis.phi,subset=="Drought")

#drought
full.b.gracilis.drought<-lme(phips2 ~ treatment*Week,random=~1|Block/Plant,data=b.gracilis.drought.phi,na.action = na.exclude)
Anova(full.b.gracilis.drought,type=3)
#signiciant treatment*week interaction
anova(full.b.gracilis.drought)
#still signiciant with type 1


#recovery 

b.gracilis.recovery.phi<-subset(b.gracilis.phi,subset.recovery=="Recovery")

full.b.gracilis.recovery<-lme(phips2 ~ treatment*Week,random=~1|Block/Plant,data=b.gracilis.recovery.phi,na.action = na.exclude)
Anova(full.b.gracilis.recovery,type=3)
#signiciant treatment*week interaction, week alone not significant
anova(full.b.gracilis)
#still signiciant with type 1


#Week1bogr
bogr.week.1.phi<-subset(b.gracilis,Week=="1",na.rm=TRUE)
bogr.week1.lme.phi<-lme(phips2~treatment,random=~1|Block/Plant,data=bogr.week.1.phi,na.action = na.exclude)
anova(bogr.week1.lme.phi)
#n.s.

#Week3bogr
bogr.week.3<-subset(b.gracilis,Week=="3",na.rm=TRUE)
bogr.week3.lme.phi<-lme(phips2~treatment,random=~1|Block/Plant,data=bogr.week.3,na.action = na.exclude)
anova(bogr.week1.lme.phi)
#n.s.


#Week5bogr
bogr.week.5<-subset(b.gracilis,Week=="5",na.rm=TRUE)
bogr.week5.lm.phi<-lme(phips2~treatment,random=~1|Block/Plant,data=bogr.week.5,na.action = na.exclude)
anova(bogr.week5.lm.phi)
#n.s., p-value = .092 - weekly significant like net photosynthesis

#Week7bogr
bogr.week.7<-subset(b.gracilis,Week=="7",na.rm=TRUE)
bogr.week7.lm.phi<-lme(phips2~treatment,random=~1|Block/Plant,data=bogr.week.7,na.action = na.exclude)
anova(bogr.week7.lm.phi)
#*** significant, following a similar trend of net photosynthesis

#Week8bogr
bogr.week.8<-subset(b.gracilis,Week=="8",na.rm=TRUE)
bogr.week8.lm.phi<-lme(phips2~treatment,random=~1|Block/Plant,data=bogr.week.8,na.action = na.exclude)
anova(bogr.week8.lm.phi)
#n.s. - recovery within a week

#Week9bogr
bogr.week.9<-subset(b.gracilis,Week=="9",na.rm=TRUE)
bogr.week9.lm.phi<-lm(A~treatment,data=bogr.week.9,na.rm=TRUE)
anova(bogr.week9.lm.a)
#n.s.

#Week10bogr
bogr.week.10<-subset(b.gracilis,Week=="10",na.rm=TRUE)
bogr.week10.lm.phi<-lme(phips2~treatment,random=~1|Block/Plant,data=bogr.week.10,na.action = na.exclude)
anova(bogr.week10.lm.phi)
#signiciantly higher

#Week11bogr
bogr.week.11<-subset(b.gracilis,Week=="11",na.rm=TRUE)
bogr.week11.lm.phi<-lm(phips2~treatment,data=bogr.week.11,na.rm=TRUE)
anova(bogr.week11.lm.a)
#n.s.

#A.gerardii
A.gerardii<-subset(drad.phys,Species=="A.gerardii")

full.a.gerardii<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=A.gerardii,na.action = na.exclude)
Anova(full.a.gerardii,type=3)
#treatment effect, no treatment by week for full model

#drought
summary(A.gerardii)
a.gerardii.drought<-subset(A.gerardii,subset=="Drought")
full.a.gerardii.drought.phi<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=a.gerardii.drought,na.action = na.exclude)
Anova(full.a.gerardii.drought.phi,type=3)
#signiciant treatment*week interaction

#Recovery
a.gerardii.recovery<-subset(A.gerardii,subset.recovery=="Recovery")
full.a.gerardii.drought.phi<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=a.gerardii.recovery,na.action = na.exclude)
Anova(full.a.gerardii.drought.phi,type=3)
#signiciant treament*week

#week1.andro
andro.week.1.phi<-subset(A.gerardii,Week=="1",na.rm=TRUE)
andro.week1.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.1.a,na.action = na.exclude)
anova(andro.week1.lm.phi)
#n.s.

#week3.andro
andro.week.3.phi<-subset(A.gerardii,Week=="3",na.rm=TRUE)
andro.week3.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.3.phi,na.action = na.exclude)
anova(andro.week3.lm.phi)
#n.s

#week5.andro
andro.week.5.phi<-subset(A.gerardii,Week=="5",na.rm=TRUE)
andro.week5.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.5.a,na.action = na.exclude)
anova(andro.week5.lm.phi) 
#significant, similar trend as net photosynthesis

#week7.andro
andro.week.7.phi<-subset(A.gerardii,Week=="7",na.rm=TRUE)
andro.week7.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.7.a,na.action = na.exclude)
anova(andro.week7.lm.phi)
#significant

#week8.andro
andro.week.8.phi<-subset(A.gerardii,Week=="8",na.rm=TRUE)
andro.week8.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.8.phi,na.action = na.exclude)
anova(andro.week8.lm.phi)
#significant

#week9.andro
andro.week.9.phi<-subset(A.gerardii,Week=="9",na.rm=TRUE)
andro.week9.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.9.phi,na.action = na.exclude)
anova(andro.week9.lm.phi)
#n.s. similar trend as net photosynthesis -recovered two weeks post-drought

#week10.andro
andro.week.10.phi<-subset(A.gerardii,Week=="10",na.rm=TRUE)
andro.week10.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.10.phi,na.action = na.exclude)
anova(andro.week10.lm.phi)
#n.s.

#week11.andro
andro.week.11.phi<-subset(A.gerardii,Week=="11",na.rm=TRUE)
andro.week11.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=andro.week.11.phi,na.action = na.exclude)
anova(andro.week11.lm.phi)
#significant

#P.smithii
P.smithii<-subset(drad.phys,Species=="P.smithii")

full.p.smithii<-lme(A~treatment*Week,random=~1|Block/Plant,data=P.smithii,na.action = na.exclude)
Anova(full.p.smithii,type=3)
#everything significant

#drought
p.smithii.drought<-subset(P.smithii,subset=="Drought")

full.p.smithii.drought<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=p.smithii.drought,na.action = na.exclude)
Anova(full.p.smithii.drought,type=3)
#signiciant treatment*week interaction

#Recovery
p.smithii.recovery<-subset(P.smithii,subset.recovery=="Recovery")
full.p.smithii.recovery.phi<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=p.smithii.recovery,na.action = na.exclude)
Anova(full.p.smithii.recovery.phi,type=3)
#significant treatment*week interaction

#week1.pasm
pasm.week.1<-subset(P.smithii,Week=="1",na.rm=TRUE)
pasm.week1.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.1,na.action = na.exclude)
anova(pasm.week1.lm.phi)
#not significant

?MixMod

#week3.pasm
pasm.week.3<-subset(P.smithii,Week=="3",na.rm=TRUE)
pasm.week3.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.3,na.action = na.exclude)
anova(pasm.week3.lm.phi)
#not signficiant impacts ocurr later relative to gas exchange

#week5.pasm
pasm.week.5<-subset(P.smithii,Week=="5",na.rm=TRUE)
pasm.week5.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.5,na.action = na.exclude)
anova(pasm.week5.lm.phi)
#signficant

#week7.pasm
pasm.week.7<-subset(P.smithii,Week=="7",na.rm=TRUE)
pasm.week7.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.7,na.action = na.exclude)
anova(pasm.week7.lm.phi)
#signficant

#week8.pasm
pasm.week.8<-subset(P.smithii,Week=="8",na.rm=TRUE)
pasm.week8.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.8,na.action = na.exclude)
anova(pasm.week8.lm.phi)
#signficant

#week9.pasm
pasm.week.9<-subset(P.smithii,Week=="9",na.rm=TRUE)
pasm.week9.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.9,na.action = na.exclude)
anova(pasm.week9.lm.phi)
#not significant - similar recovery time trend as gas exchange - though the values are not higher

#week10.pasm
pasm.week.10<-subset(P.smithii,Week=="10",na.rm=TRUE)
pasm.week10.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.10,na.action = na.exclude)
anova(pasm.week10.lm.phi)
#not significant

#week11.pasm
pasm.week.11<-subset(P.smithii,Week=="11",na.rm=TRUE)
pasm.week11.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=pasm.week.11,na.action = na.exclude)
anova(pasm.week11.lm.phi)
#not signficant

#photosynthesis, and thus presumably gas exchange, in P.smithii more sensitive to drought the phipsII, and thus electron transport capacity. yet gas exchange also more sensitive to increases in soil moisture

#B.eriopoda
B.eriopoda<-subset(drad.phys,Species=="B.eriopoda")

full.b.eriopoda.phi<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=B.eriopoda,na.action = na.exclude)
Anova(full.b.eriopoda.phi,type=3)
#no sig treatment*week interaction for full experiment or for week, significant effect of treatment

#drought
b.eriopoda.drought<-subset(drad.phys,subset=="Drought")
full.b.eriopoda.drought.phi<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=b.eriopoda.drought,na.action = na.exclude)
Anova(full.b.eriopoda.drought.phi,type=3)
#significant treatment*week interaction for drought

#Recovery
b.eriopoda.recovery<-subset(drad.phys,subset.recovery=="Recovery")
full.b.eriopoda.recovery.phi<-lme(phips2 ~treatment*Week,random=~1|Block/Plant,data=b.eriopoda.recovery,na.action = na.exclude)
Anova(full.b.eriopoda.recovery.phi,type=3)
#significant treatment*week interaction

#week1.boer
boer.week.1<-subset(B.eriopoda,Week=="1",na.rm=TRUE)
boer.week1.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.1,na.action = na.exclude)
anova(boer.week1.lm.phi)
#n.s.

#week3.boer
boer.week.3<-subset(B.eriopoda,Week=="3",na.rm=TRUE)
boer.week3.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.3,na.action = na.exclude)
anova(boer.week3.lm.phi)
#n.s.

#week5.boer
boer.week.5<-subset(B.eriopoda,Week=="5",na.rm=TRUE)
boer.week5.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.5,na.action = na.exclude)
anova(boer.week5.lm.phi)
#significant - similar trend as net photosynthesis

#week7.boer
boer.week.7<-subset(B.eriopoda,Week=="7",na.rm=TRUE)
boer.week7.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.7,na.action = na.exclude)
anova(boer.week7.lm.phi)
#significant

#week8.boer
boer.week.8<-subset(B.eriopoda,Week=="8",na.rm=TRUE)
boer.week8.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.8,na.action = na.exclude)
anova(boer.week8.lm.phi)
#significant

#week9.boer
boer.week.9<-subset(B.eriopoda,Week=="9",na.rm=TRUE)
boer.week9.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.9,na.action = na.exclude)
anova(boer.week9.lm.phi)
#n.s. same time trend as net photosynthesis

#week10.boer
boer.week.10<-subset(B.eriopoda,Week=="10",na.rm=TRUE)
boer.week10.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.10,na.action = na.exclude)
anova(boer.week10.lm.phi)
#n.s.

#week11.boer
boer.week.11<-subset(B.eriopoda,Week=="11",na.rm=TRUE)
boer.week11.lm.phi<-lme(phips2 ~treatment,random=~1|Block/Plant,data=boer.week.11,na.action = na.exclude)
anova(boer.week11.lm.phi) 
#n.s.

#similar to P.sithii, phipsII generally less sensitive to drought, took longer for signficant reductions to ocurr
#in certain species

###waterpotential
drad.wp<-read.csv(file.choose(),header=TRUE)
head(drad.wp)
hist(drad.wp$mpa)
#full

full.wp<-lme(mpa~species*treatment*Week,random=~1|Block/plant,data=drad.wp,na.action = na.exclude)
Anova(full.wp,type=3)
#signiciant species differences, so analyze sperately 

#a.gerardii
andro.wp<-subset(drad.wp,species=="A.gerardii",na.rm=TRUE)
summary(andro.wp)
andro.wp.full<-lme(mpa~treatment*Time,random=~1|Block/plant,data=andro.wp,na.action = na.exclude)
Anova(andro.wp.full,type=3)
#signficiant treatment*time interactions


#week1.andro
andro.wp.week.1<-subset(andro.wp,Week=="1",na.rm=TRUE)
pasm.week1.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=andro.wp.week.1,na.rm=TRUE)
anova(pasm.week1.lme.wp)
#not significant

#week7.andro
andro.wp.week.7<-subset(andro.wp,Week=="7",na.rm=TRUE)
andro.wp.week.7.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=andro.wp.week.7,na.rm=TRUE)
anova(andro.wp.week.7.lme.wp)
#highly signiciant 

#week11.andro
andro.wp.week.11<-subset(andro.wp,Week=="11",na.rm=TRUE)
andro.wp.week.11.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=andro.wp.week.11,na.rm=TRUE)
anova(andro.wp.week.11.lme.wp)
#not significant, p=.16

##B.gracilis
bogr.wp<-subset(drad.wp,species=="B.gracilis",na.rm=TRUE)
summary(bogr.wp)
bogr.wp.full<-lme(mpa~treatment*Time,random=~1|Block/plant,data=andro.wp,na.action = na.exclude)
Anova(andro.wp.full,type=3)
#signiciant treatmnt*time interaction

?MixMod

#week1.bogr
bogr.wp.week.1<-subset(bogr.wp,Week=="1",na.rm=TRUE)
bogr.week1.lme.wp<-lm(mpa ~ treatment,random=~1|Block/plant,data=bogr.wp.week.1,na.rm=TRUE)
anova(bogr.week1.lme.wp)
#not significant, p=.64

#week7.bogr
bogr.wp.week.7<-subset(bogr.wp,Week=="7",na.rm=TRUE)
bogr.wp.week.7.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=bogr.wp.week.7,na.rm=TRUE)
anova(bogr.wp.week.7.lme.wp)
#highly signiciant, 7.011e-07 ***

#week11.bogr
bogr.wp.week.11<-subset(bogr.wp,Week=="11",na.rm=TRUE)
bogr.wp.week.11.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=bogr.wp.week.11,na.rm=TRUE)
anova(bogr.wp.week.11.lme.wp)
#not significant, p = .9101

##B.eriopoda
boer.wp<-subset(drad.wp,species=="B.eriopoda",na.rm=TRUE)
summary(boer.wp)
boer.wp.full<-lme(mpa~treatment*Time,random=~1|Block/plant,data=boer.wp,na.action = na.exclude)
Anova(boer.wp.full,type=3)
#significant treatment*time interaction

?MixMod

#week1.boer
boer.wp.week.1<-subset(boer.wp,Week=="1",na.rm=TRUE)
boer.week1.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=boer.wp.week.1,na.rm=TRUE)
anova(boer.week1.lme.wp)
#not significant, 0.6438

#week7.boer
boer.wp.week.7<-subset(boer.wp,Week=="7",na.rm=TRUE)
boer.wp.week.7.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=boer.wp.week.7,na.rm=TRUE)
anova(boer.wp.week.7.lme.wp)
#highly signiciant, 1.071e-07 ***

#week11.boer
boer.wp.week.11<-subset(boer.wp,Week=="11",na.rm=TRUE)
boer.wp.week.11.lme.wp<-lm(mpa ~treatment,random=~1|Block/plant,data=boer.wp.week.11,na.rm=TRUE)
anova(boer.wp.week.11.lme.wp)
#not significant, p = 0.4823

##P.smithii
pasm.wp<-subset(drad.wp,species=="P.smithii",na.rm=TRUE)
summary(pasm.wp)
pasm.wp.full<-lme(mpa~treatment*Time,random=~1|Block/plant,data=pasm.wp,na.action = na.exclude)
Anova(pasm.wp.full,type=3)
#Signiciant treatment*time interaction 

?MixMod

#week1.pasm
pasm.wp.week.1<-subset(pasm.wp,Week=="1",na.rm=TRUE)
pasm.week1.lme.wp<-lme(mpa ~ treatment,random=~1|Block/plant,data=pasm.wp.week.1,na.action = na.exclude)
anova(pasm.week1.lme.wp)
#not significant, 0.7126

#week7.pasm
pasm.wp.week.7<-subset(pasm.wp,Week=="7",na.rm=TRUE)
pasm.wp.week.7.lme.wp<-lm(mpa ~ treatment,random=~1|Block/plant,data=pasm.wp.week.7,na.action = na.exclude)
anova(pasm.wp.week.7.lme.wp)
#highly signiciant, 5.616e-11 ***

#week11.pasm
pasm.wp.week.11<-subset(pasm.wp,Week=="11",na.rm=TRUE)
pasm.wp.week.11.lme.wp<-lm(mpa ~ treatment,random=~1|Block/plant,data=pasm.wp.week.11,na.action = na.exclude)
anova(pasm.wp.week.11.lme.wp)
#not significant, p = 0.1517


#specific leaf area

drad.sla<-read.csv(file.choose(),header=TRUE)
head(drad.sla)
hist(drad.sla$SLA)

#full
full.sla<-lme(SLA~Species*Treatment*Time,random=~1|Plant,data=drad.sla,na.action = na.exclude)
Anova(full.wp,type=3)
#signiciant species-level differences, so analyze sperately 

#andro
andro.sla<-subset(drad.sla,Species=="A.gerardii",na.rm=TRUE)
summary(andro.sla)
andro.sla.full<-lme(SLA~Treatment*Time,random=~1|Block/Plant,data=andro.sla,na.action = na.exclude)
Anova(andro.sla.full,type=3)
#nothing signiciant

?MixMod

#week7.andro
andro.wp.week.7<-subset(andro.sla,Time=="Peak drought",na.rm=TRUE)
andro.sla.week.7.lme.wp<-lm(SLA ~Treatment,random=~1|Block/Plant,data=andro.wp.week.7,na.rm=TRUE)
anova(andro.sla.week.7.lme.wp)
#P= 0.3207, no significant

#week11.andro
andro.sla.week.11<-subset(andro.sla,Time=="Recovery",na.rm=TRUE)
andro.sla.week.11.lme<-lm(mpa ~treatment,random=~1|Block/plant,data=andro.wp.week.11,na.rm=TRUE)
anova(andro.sla.week.11.lme)
#not significant, P=0.1517

#B.gracilis
bogr.sla<-subset(drad.sla,Species=="B.gracilis",na.rm=TRUE)
summary(bogr.sla)
bogr.sla.full<-lme(SLA~Treatment*Time,random=~1|Plant,data=bogr.sla,na.action = na.exclude)
Anova(bogr.sla.full,type=3)
#Treatment*time signiciant

?MixMod

#week7.bogr
bogr.wp.week.7<-subset(bogr.sla,Time=="Peak drought",na.rm=TRUE)
bogr.sla.week.7.lme<-lm(SLA ~Treatment,random=~1|Block/Plant,data=bogr.wp.week.7,na.rm=TRUE)
anova(bogr.sla.week.7.lme)
#not signiciant p = .95

#week11.bogr
bogr.wp.week.11<-subset(bogr.sla,Time=="Recovery",na.rm=TRUE)
bogr.sla.week.11.lme<-lm(SLA ~Treatment,random=~1|Block/Plant,data=bogr.wp.week.11,na.rm=TRUE)
anova(bogr.sla.week.11.lme)
#highly signiciant (higher)

#B.eriopoda
boer.sla<-subset(drad.sla,Species=="B.eriopoda",na.rm=TRUE)
summary(boer.sla)
boer.sla.full<-lme(SLA~Treatment*Time,random=~1|Block/Plant,data=boer.sla,na.action = na.exclude)
Anova(boer.sla.full,type=3)
#treatmena weakly significant, treatment*time weakly significant

#week7.boer
boer.wp.week.7<-subset(boer.sla,Time=="Peak drought",na.rm=TRUE)
boer.sla.week.7.lme<-lm(SLA ~Treatment,random=~1|Block/Plant,data=boer.wp.week.7,na.rm=TRUE)
anova(boer.sla.week.7.lme)
# signiciant (lower)

#week11.boer
boer.wp.week.11<-subset(boer.sla,Time=="Recovery",na.rm=TRUE)
boer.sla.week.11.lme<-lm(SLA ~Treatment,random=~1|Block/Plant,data=boer.wp.week.11,na.rm=TRUE)
anova(boer.sla.week.11.lme)
#not signiciant

#P.smthii SLA
pasm.sla<-subset(drad.sla,Species=="P.smithii",na.rm=TRUE)
summary(pasm.sla)
pasm.sla.full<-lme(SLA~Treatment*Time,random=~1|Block/Plant,data=pasm.sla,na.action = na.exclude)
Anova(pasm.sla.full,type=3)
#treatment*time signiciant

#week7.pasm
pasm.wp.week.7<-subset(pasm.sla,Time=="Peak drought",na.rm=TRUE)
pasm.sla.week.7.lme<-lm(SLA ~Treatment,random=~1|Block/Plant,data=pasm.wp.week.7,na.rm=TRUE)
anova(pasm.sla.week.7.lme)
#not signiciant

# signiciant

#week11.pasm
pasm.wp.week.11<-subset(pasm.sla,Time=="Recovery",na.rm=TRUE)
pasm.sla.week.11.lme<-lm(SLA ~Treatment,random=~1|Block/Plant,data=pasm.wp.week.11,na.rm=TRUE)
anova(pasm.sla.week.11.lme)
#signiciant (higher)

#root:shoot

head(drad.biomass)
root.shoot.full<-subset(pasm.sla,Time=="drad.biomass",na.rm=TRUE)
root.shoot.full.lme<-lme(root.shoot~Treatment*Species,random=~1|Plant,data=drad.biomass,na.action = na.exclude)
Anova(root.shoot.full.lme)
#Significant Species differences so analyze separately


#NPP
#B.gracilis
drad.biomass<-read.csv(file.choose(),header=TRUE)
head(drad.biomass)
b.gracilis.biomass<-subset(drad.biomass,Species=="B.gracilis",na.rm=TRUE)
bogr.root.shoot.full.lme<-lme(root.shoot~Treatment,random=~1|Block/Plant,data=b.gracilis.biomass,na.action = na.exclude)
anova(bogr.root.shoot.full.lme)
#signiciant - post-deought lower than peak drought

#A.gerardii
andro.biomass<-subset(drad.biomass,Species=="A.gerardii",na.rm=TRUE)
andro.root.shoot.full.lme<-lme(root.shoot~Treatment,random=~1|Block/Plant,data=andro.biomass,na.action = na.exclude)
anova(andro.root.shoot.full.lme)
#significant - post-drought lower than peak drought

#B.eriopoda
boer.biomass<-subset(drad.biomass,Species=="B.eriopoda",na.rm=TRUE)
boer.root.shoot.full.lme<-lme(root.shoot~Treatment,random=~1|Block/Plant,data=boer.biomass,na.action = na.exclude)
anova(boer.root.shoot.full.lme)
#not signficant

#not significant
pasm.biomass<-subset(drad.biomass,Species=="P.smithii",na.rm=TRUE)
pasm.root.shoot.full.lme<-lme(root.shoot~Treatment,random=~1|Block/Plant,data=pasm.biomass,na.action = na.exclude)
anova(pasm.root.shoot.full.lme)
#moderatly signiciant

?position_dodge()
dodge <- position_dodge(.8)

#ggplot
summary(merge.biomass)
library(ggplot2)
head(controls.averaged.merged)
ggplot(merge.final.a,aes(Week,a.perc.x,fill=as.factor(Species),na.rm=TRUE)) +
  #scale_x_discrete(labels=c("1" = "Mesic prairie", "2"="Mixed-grass prairie",
                           # "3" = "Semi-arid steppe","4"="Desert grassland")) +
  #geom_vline(xintercept = 7:11,color="gray80")  +
  #stat_summary(geom="line",fun.y="mean",position=dodge) +
  stat_summary(geom="line",fun.y="mean",position=dodge) +
  #geom_line() +
  geom_errorbar(aes(ymin=a.perc.x-a.perc.y, ymax=a.perc.x + a.perc.y), width=.5,position=dodge) +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1,se=FALSE,color="red")
  #facet_wrap(~Region) +
  #stat_summary(geom="point",fun.y="mean",size=4,color="black",fill="white") +
  geom_hline(yintercept = 0) +
  stat_summary(geom="point",fun.y="mean",size=3.5,color="black",aes(fill=as.factor(Species)),shape=21,position=dodge) +
  scale_fill_manual(values=c('DR'='red','C'='blue'),name="") +
  #geom_errorbar(limits,width=0.1,position = dodge)
  #scale_fill_manual(values=c('B.gracilis'='black','A.gerardii'='white'),name="Region",
  #labels=c('B.gracilis'='Semi-arid steppe','A.gerardii'='Mesic prairie')) +
  #ylab("Area per leaf (cm^2") +
  #ylab(bquote('Area per leaf ('*cm^2*')')) +
  #scale_x_discrete(labels=c("1" = "Mesic prairie", "2"="Mixed-grass prairie",
                           # "3" = "Semi-arid steppe","4"="Desert grassland")) +
  ylab(bquote('Net photosynthesis ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) +
  facet_wrap(~plotid,labeller=hospital_labeller,nrow=2) +
  xlab("Week") +
  #coord_flip() +
  theme(
    axis.text = element_text(color='black',size=14),
    axis.title = element_text(color='black',size=18),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=20),
    legend.text = element_text(size=17),
    legend.position = c(.075,.1),
    legend.key = element_blank(),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

ggsave("drad_A_plot.pdf",width = 8, height = 6, units = c("in"))


limits <- aes(ymin=root.shoot.x-root.shoot.y, ymax=root.shoot.x  + root.shoot.y)

hospital_names <- list(
  '1'="Mesic prairie",
  '2'="Mixed grass prairie",
  '3'="Semi-arid steppe",
  '4'="Desert grassland"
)


hospital_labeller <- function(variable,value){
  return(hospital_names[value])
}


#graphing datasets
#net photosynthesis
head(drad.phys)
ag.mean.a<-aggregate(A~plotid + Week + treatment + Species, mean,data=drad.phys)
ag.ser.a<-aggregate(A~plotid + Week + treatment + Species, ser,data=drad.phys)
merge.stuff.a<-merge(ag.mean.a,ag.ser.a,by=c("Week","Species","treatment","plotid"))


#flor
ag.mean.phi<-aggregate(phips2~Week + Region + treatment + Species, mean,data=drad.phys)
ag.mean.ser<-aggregate(phips2~Week + Region + treatment + Species, ser,data=drad.phys)
merge.stuff.phi<-merge(ag.mean.phi,ag.mean.ser,by=c("Week","Region","Species","treatment"))

#wp
head(drad.wp)
ag.mean.sla<-aggregate(mpa~Time + Week + treatment + species, mean,data=drad.wp)
ag.sla.ser<-aggregate(mpa~Time + Week + treatment + species, ser,data=drad.wp)
merge.stuff.sla<-merge(ag.mean.sla,ag.sla.ser,by=c("Week","species","treatment"))

#wp
head(drad.wp)
ag.mean.sla<-aggregate(mpa~Time + Week + treatment + species, mean,data=drad.wp)
ag.sla.ser<-aggregate(mpa~Time + Week + treatment + species, ser,data=drad.wp)
merge.stuff.sla<-merge(ag.mean.sla,ag.sla.ser,by=c("Week","species","treatment"))



#creating dataframes for ggplot
#standard error function
ser <- function(x) {
  
  n <- sum(!is.na(x))
  se <- sd(x, na.rm = T)/sqrt(n)
  
  return(se)
}

#aggregation for modeling
head(drad.phys)
#net photosynthesis
ag.mean.A<-aggregate(A~Week + Region + treatment + Species, mean,data=drad.phys)
ag.sd.A<-aggregate(A~Week + Region +treatment + Species, sd,data=drad.phys)
merge.stuff.A<-merge(ag.mean.A,ag.sd.A,by=c("Week","Region","Species","treatment"))
write.csv(merge.stuff,file="clm.A.summary.csv")

#phipsII
ag.mean.phi<-aggregate(phips2~Week + Region + treatment + Species, mean,data=drad.phys)
ag.sd.phi<-aggregate(phips2~Week + Region +treatment + Species, sd,data=drad.phys)
merge.stuff.phi<-merge(ag.mean.phi,ag.sd.phi,by=c("Week","Region","Species","treatment"))
write.csv(merge.stuff.phi,file="clm.phi.csv")

#gsw
ag.mean.gsw<-aggregate(gsw~Week + Region + treatment + Species, mean,data=drad.phys)
ag.sd.gsw<-aggregate(gsw~Week + Region +treatment + Species, sd,data=drad.phys)
merge.stuff.gsw<-merge(ag.mean.gsw,ag.sd.gsw,by=c("Week","Region","Species","treatment"))
write.csv(merge.stuff.gsw,file="clm.gsw.csv")

#water potential
drad.wp<-read.csv(file.choose(),header=TRUE)
head(drad.wp)
ag.sd.wp<-aggregate(mpa~Week  + treatment + species, ser,data=drad.wp)
ag.mean.wp<-aggregate(mpa~Week + treatment + species, mean,data=drad.wp)
ag.sd.wp<-aggregate(mpa~Week  + treatment + species, sd,data=drad.wp)
merge.stuff.wp<-merge(ag.mean.wp,ag.sd.wp,by=c("Week","species","treatment"))
write.csv(merge.stuff.wp,file="clm.wp.csv")

#SLA
drad.sla<-read.csv(file.choose(),header=TRUE)
head(drad.sla)
ag.mean.sla<-aggregate(SLA~Time + Treatment + Species, mean,data=drad.sla)
ag.sd.sla<-aggregate(SLA~Time + Treatment + Species, sd,data=drad.sla)
merge.stuff.sla<-merge(ag.mean.sla,ag.sd.sla,by=c("Time","Species","Treatment"))
write.csv(merge.stuff.sla,file="clm.sla.csv")

#leaf area
drad.morph<-read.csv(file.choose(),header=TRUE)
head(drad.morph)
drought.area<-subset(drad.morph,treatment=="DR")
ag.drought.leaf.area.mean<-aggregate(average~Species + Week + treatment,mean,data=drought.area)
ag.drought.leaf.area.sd<-aggregate(average~Species + Week + treatment,sd,data=drought.area)

merge.stuff.area<-merge(ag.drought.leaf.area.mean,ag.drought.leaf.area.sd,by=c("Week","Species","treatment"))
write.csv(merge.stuff.area,file="clm.area.csv")

#stem number
ag.drought.stem.mean<-aggregate(tillers~Species + Week + treatment,mean,data=drought.area)
ag.drought.stem.sd<-aggregate(tillers~Species + Week + treatment,sd,data=drought.area)

merge.stuff.stem<-merge(ag.drought.stem.mean,ag.drought.stem.sd,by=c("Week","Species","treatment"))
write.csv(merge.stuff.stem,file="clm.stem.csv") 

#green.leaves
ag.drought.green.mean<-aggregate(green.leaves~Species + Week + treatment,mean,data=drought.area)
ag.drought.green.sd<-aggregate(green.leaves~Species + Week + treatment,sd,data=drought.area)

merge.stuff.green.leaves<-merge(ag.drought.green.mean,ag.drought.green.sd,by=c("Week","Species","treatment"))
write.csv(merge.stuff.green.leaves,file="clm.green.csv") 

#brown.leaves
head(drought.area)
ag.drought.brown.mean<-aggregate(brown.leaves~Species + Week + treatment,mean,data=drought.area)
ag.drought.brown.sd<-aggregate(brown.leaves~Species + Week + treatment,sd,data=drought.area)

merge.stuff.brown.leaves<-merge(ag.drought.brown.mean,ag.drought.brown.sd,by=c("Week","Species","treatment"))
write.csv(merge.stuff.brown.leaves,file="clm.brown.csv") 

#root:shoot
drad.biomass<-read.csv(file.choose(),header=TRUE)
head(drad.biomass)
ag.drought.root.shoot.mean<-aggregate(root.shoot~Species + Time ,mean,data=drad.biomass)
ag.drought.root.shoot.sd<-aggregate(root.shoot~Species + Time ,sd,data=drad.biomass)
ag.drought.root.shoot.se<-aggregate(root.shoot~Species + Time + ID,ser,data=drad.biomass)

merge.biomass<-merge(ag.drought.root.shoot.mean,ag.drought.root.shoot.sd,by=c("Time","Species"))
write.csv(merge.biomass,file="clm.root.shoot.csv") 

#for finding starting poin of model scenario
library(raster)
library(sp)
r <- getData("worldclim",var="bio",res=10)
r <- r[[c(1,12)]]
names(r) <- c("temp","Prec")
?getData

lats <- c( 34.4200000 ,34.5, 40.83 ,39.09, 39.09)
lons <- c(-106.5130000,-105.20000, -104.72 ,-97.75, -96.58 ) 

coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

df <- cbind.data.frame(coordinates(points),values)

head(df)
plot(r[[1]])
plot(points,add=T)

#mean annual precipitation midpoints
(246 + 375)/2 #midpoint is 310 mm
(584 + 892)/2 #midpoint is 738 mm
