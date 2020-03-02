
#linear mixed effects models

#load libraries
library(nlme)
library(car)
library(dplyr)
library(fBasics)
####GWC###########
drad.gwc<-read.csv('G:/My Drive/Other projects/doe-drought-recovery/DOE_2019/data/drad_gwc_for_analysis.csv',header=TRUE)
head(drad.gwc)
View(drad.gwc)
gwc.full<-lme(newgwc~Species*Treatment,random=~1|Pot,data=drad.gwc,na.action = na.exclude)
hist(resid(gwc.full)) #normal
anova(gwc.full)
Anova(gwc.full, contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#significant treatment*species interaction. Look at separately

#A.gerardii
andro.gwc<-subset(drad.gwc,Species=='A.gerardii')
summary(andro.gwc)
andro.gwc.lme<-lme(newgwc~Treatment*Day,random=~1|Pot,data=andro.gwc,na.action = na.exclude)
hist(resid(andro.gwc.lme)) #looks normal
Anova(andro.gwc.lme, contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#significnat treatment*time interaction

#look at ranges for drought versus control
control.andro.gwc <- subset(andro.gwc,Treatment=='C')
summary(control.andro.gwc)
#range = 49% to 37%
#mean = 45%

drought.andro.gwc <- subset(andro.gwc,Treatment=='DR')
summary(drought.andro.gwc)
#range = 52% to 22%
#mean = 37%

#b.gracilis
bogr.gwc<-subset(drad.gwc,Species=='B.bracilis') #mistake in excel file with name
summary(bogr.gwc)
bogr.gwc.lme<-lme(newgwc~Treatment*Day,random=~1|Pot,data=bogr.gwc,na.action = na.exclude)
hist(resid(bogr.gwc.lme)) #looks normal
Anova(bogr.gwc.lme, contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#significnat treatment*time interaction

#look at ranges for drought versus control
control.bogr.gwc <- subset(bogr.gwc,Treatment=='C')
summary(control.bogr.gwc)
#range = 50% to 27%
#mean = 43%

drought.bogr.gwc <- subset(bogr.gwc,Treatment=='DR')
summary(drought.bogr.gwc)
#range = 18% to 49%
#mean = 35%

#boer
boer.gwc<-subset(drad.gwc,Species=='B.eriopoda') #mistake in excel file with name
summary(boer.gwc)
boer.gwc.lme<-lme(newgwc~Treatment*Day,random=~1|Pot,data=boer.gwc,na.action = na.exclude)
hist(resid(boer.gwc.lme)) #looks normal
Anova(boer.gwc.lme, contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#significant treatment*time interaction

#look at ranges for drought versus control
control.boer.gwc <- subset(boer.gwc,Treatment=='C')
summary(control.boer.gwc)
#range = 52% to 35%
#mean = 44%

drought.boer.gwc <- subset(boer.gwc,Treatment=='DR')
summary(drought.boer.gwc)
#range = 25% to 49%
#mean = 37%

#pasm
pasm.gwc<-subset(drad.gwc,Species=='P.smithii') #mistake in excel file with name
summary(pasm.gwc)
pasm.gwc.lme<-lme(newgwc~Treatment*Day,random=~1|Pot,data=pasm.gwc,na.action = na.exclude)
hist(resid(pasm.gwc.lme)) #looks normal
Anova(pasm.gwc.lme, contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#significant treatment*time interaction

#look at ranges for drought versus control
control.pasm.gwc <- subset(pasm.gwc,Treatment=='C')
summary(control.pasm.gwc)
#range = 30% to 49%
#mean = 42%

drought.pasm.gwc <- subset(pasm.gwc,Treatment=='DR')
summary(drought.pasm.gwc)
#range = 23% to 51%
#mean = 36%



####photosynthesis#######
drad.phys<-read.csv('C:/Users/A02296270/Desktop/Side_projects/DOE/doe_greenhouse_experiment_data/DRAD_GAS_EXCHANGE.csv',header=TRUE)
head(drad.phys)
summary(drad.phys)

#look at distribtuion of data
hist(drad.phys$A)

#check for baseline differences among species

controls<-subset(drad.phys,treatment=='C')
A.controls<-lme(A~Species,random=~1|Block/Plant,data=controls,na.action = na.exclude)
plot(resid(A.controls)) #homegenity of variances met
dagoTest(resid(A.controls)) #normality of model residuals met
hist(resid(A.controls))
anova(A.controls) #significant baseline differences in species phsyiology, analyze each species separately

drad.phys$A.transformed <- sqrt((drad.phys$A + 1))

#two periods of focus drought and recovery
drought<-subset(drad.phys,subset=="Drought") # a little less normal, produce due to the drought reductions through time...
hist(drought$A)

recovery<-subset(drad.phys,subset.recovery=="Recovery") # normal
hist(recovery$A)
View(recovery)

#look at autocorrelation
full.recovery.A.AR<-lme(A~Species*treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1),data=recovery,na.action = na.exclude)
full.recovery.A<-lme(A~Species*treatment*Week,random=~1|Block/Plant,data=recovery,na.action = na.exclude)
AIC(full.recovery.A,full.recovery.A.AR) #really no difference again

#look at model assumptions
plot(full.recovery.A) #residuals versus fitted has fairly homegenous variance in the drought model
qqnorm(resid(full.recovery.A)) #some noise at the tails
qqline(resid(full.recovery.A))
shapiro.test(resid(full.recovery.A)) #doesn't pass the test

#transform
recovery$A.transformed <- sqrt((recovery$A + 1)) 

#look at model
full.recovery.A.transformed<-lme(A.transformed~Species*treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1), data=recovery,na.action = na.exclude)
plot(full.recovery.A.transformed) #residuals versus fitted has fairly homegenous variance in the drought model
qqnorm(resid(full.recovery.A.transformed)) #some noise at the tails
qqline(resid(full.recovery.A.transformed))
shapiro.test(resid(full.recovery.A.transformed)) #still bad, look into data
hist(resid(full.recovery.A.transformed))
View(resid(full.recovery.A.transformed))

#see if removal of the one clear outlier makes a difference...

recovery.2 <- recovery %>% filter(!(Plant=='bogr19'))
View(recovery.2)

full.recovery.A.transformed.2<-lme(A.transformed~Species*treatment*Week,random=~1|Block/Plant,correlation=corAR1(form=~1), data=recovery.2,na.action = na.exclude)
shapiro.test(resid(full.recovery.A.transformed.2))
#removal of the outlier helps data meet assumptions of normally distributed residuals. Still some skew, though that is to be expected.

#ok what does the model say?
Anova(full.recovery.A.transformed.2, contrasts=list(topic=contr.sum, sys=contr.sum), type=3)

#no main effect of species or interactions...species responded similarly during the recovery period...

#merge the drought and recovery subsets...?

#separatation by species

drad.phys.2<-drad.phys %>% filter(!(Plant=='bogr19'))
drad.phys.2$A.transformed <- sqrt((drad.phys.2$A + 1)) 

#######b.gracilis##
b.gracilis<-subset(drad.phys,Species=="B.gracilis")
b.gracilis.drought<-subset(drought,Species=="B.gracilis")
b.gracilis.recovery<-subset(recovery,Species=="B.gracilis")

#drought full model
full.b.gracilis.drought<-lme(A~treatment*Week,random=~1|Block/Plant,data=b.gracilis.drought,na.action = na.exclude)
Anova(full.b.gracilis.drought,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
plot(resid(full.b.gracilis.drought)) #homegenity of variances met
dagoTest(resid(full.b.gracilis.drought)) #normality of model residuals met
hist(resid(full.b.gracilis.drought)) #looks normal,little bit of an outlier...

#recovery full model
full.b.gracilis.recovery<-lme(A~treatment*Week,random=~1|Block/Plant,data=b.gracilis.recovery,na.action = na.exclude)
Anova(full.b.gracilis.recovery,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
plot(resid(full.b.gracilis.recovery)) #homegenity of variances met
dagoTest(resid(full.b.gracilis.recovery)) #normality of model residuals met
hist(resid(full.b.gracilis.recovery))

# again a significnat treatment by week, suggesting treatment impacts depended on the week.

#Week1bogr
bogr.week.1<-subset(b.gracilis,Week=="1",na.rm=TRUE)
bogr.week1.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.1,na.action = na.exclude)
plot(resid(bogr.week1.lm.a))
hist(resid(bogr.week1.lm.a))
anova(bogr.week1.lm.a)
#n.s.

#Week3bogr
bogr.week.3<-subset(b.gracilis,Week=="3",na.rm=TRUE)
hist(bogr.week.3$A)
bogr.week3.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.3,na.action = na.exclude)
plot(resid(bogr.week3.lm.a))
hist(resid(bogr.week3.lm.a))
anova(bogr.week3.lm.a)
#n.s.

#Week5bogr
bogr.week.5<-subset(b.gracilis,Week=="5",na.rm=TRUE)
bogr.week5.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.5,na.action = na.exclude)
anova(bogr.week5.lm.a)
plot(resid(bogr.week5.lm.a))
hist(resid(bogr.week5.lm.a))
#n.s.

#Week7bogr
bogr.week.7<-subset(b.gracilis,Week=="7",na.rm=TRUE)
bogr.week7.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.7,na.action = na.exclude)
anova(bogr.week7.lm.a)
plot(resid(bogr.week7.lm.a))
hist(resid(bogr.week7.lm.a))
#*** significant

mean.a.bogr.peak<-aggregate(A~treatment,mean,data=bogr.week.7)
sd.a.bogr.peak<-aggregate(A~treatment,sd,data=bogr.week.7)

#Week8bogr
bogr.week.8<-subset(b.gracilis,Week=="8",na.rm=TRUE)
bogr.week8.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=bogr.week.8,na.action = na.exclude) #transformed to better meet normality of errors
anova(bogr.week8.lm.a)
plot(resid(bogr.week8.lm.a))
hist(resid(bogr.week8.lm.a))
#n.s.

#Week9bogr
bogr.week.9<-subset(b.gracilis,Week=="9",na.rm=TRUE)
bogr.week9.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=bogr.week.9,na.action = na.exclude) #transformed to better meet normality of errors
anova(bogr.week9.lm.a)
plot(resid(bogr.week9.lm.a))
hist(resid(bogr.week9.lm.a))
#n.s.

#Week10bogr
bogr.week.10<-subset(b.gracilis,Week=="10",na.rm=TRUE)
bogr.week10.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=bogr.week.10,na.action = na.exclude) #transformed
anova(bogr.week10.lm.a)
plot(resid(bogr.week10.lm.a))
hist(resid(bogr.week10.lm.a))
#n.s.

#Week11bogr
bogr.week.11<-subset(b.gracilis,Week=="11",na.rm=TRUE)
bogr.week11.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=bogr.week.11,na.action = na.exclude)
anova(bogr.week11.lm.a)
plot(resid(bogr.week10.lm.a))
hist(resid(bogr.week10.lm.a))
#n.s.
#one week to recover at the leaf level

#####A.gerardii#
A.gerardii<-subset(drad.phys,Species=="A.gerardii")
A.gerardii.drought<-subset(A.gerardii,subset=="Drought")
A.gerardii.recovery<-subset(A.gerardii,subset.recovery=="Recovery")

mean.a.andro<-aggregate(A~treatment,mean,data=A.gerardii)
sd.a.andro<-aggregate(A~treatment,sd,data=A.gerardii)

#drought full model
full.A.gerardii.drought<-lme(A~treatment*Week,random=~1|Block/Plant,data=A.gerardii.drought,na.action = na.exclude)
Anova(full.A.gerardii.drought,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)

plot(resid(full.A.gerardii.drought)) #homegenity of variances met
dagoTest(resid(full.A.gerardii.drought)) #normality of model residuals met
hist(resid(full.A.gerardii.drought))

#treatment*week signiciant, sugegsting the influence drought depended on week, and more specifically
#that the impacts of the drought treatment increased with time

#recovery full model
full.A.gerardii.recovery<-lme(A~treatment*Week,random=~1|Block/Plant,data=A.gerardii.recovery,na.action = na.exclude)
Anova(full.A.gerardii.recovery,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#treatment*week significant, suggesting treatment differences dimished through time

plot(resid(full.A.gerardii.recovery)) #homegenity of variances met
dagoTest(resid(full.A.gerardii.recovery)) #normality of model residuals met
hist(resid(full.A.gerardii.recovery))

#week1.andro
andro.week.1.a<-subset(A.gerardii,Week=="1",na.rm=TRUE)
andro.week1.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=andro.week.1.a,na.action = na.exclude) #transformed to better meet normality assumption
anova(andro.week1.lm.a)
plot(resid(andro.week1.lm.a))
hist(resid(andro.week1.lm.a))
#n.s.

#week3.andro
andro.week.3.a<-subset(A.gerardii,Week=="3",na.rm=TRUE)
andro.week3.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=andro.week.3.a,na.action = na.exclude) #transformed
anova(andro.week3.lm.a)
plot(resid(andro.week3.lm.a))
hist(resid(andro.week3.lm.a))
#n.s.

#week5.andro
andro.week.5.a<-subset(A.gerardii,Week=="5",na.rm=TRUE)
andro.week5.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=andro.week.5.a,na.action = na.exclude) #transformed
anova(andro.week5.lm.a)
plot(resid(andro.week5.lm.a))
hist(resid(andro.week5.lm.a))
#**signiciant

#week7.andro
andro.week.7.a<-subset(A.gerardii,Week=="7",na.rm=TRUE)
andro.week7.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.7.a,na.action = na.exclude)
anova(andro.week7.lm.a)
plot(resid(andro.week7.lm.a))
hist(resid(andro.week7.lm.a)) #looks meh
#***signiciant

mean.a.andro.peak<-aggregate(A~treatment,mean,data=andro.week.7.a)
sd.a.andro.peak<-aggregate(A~treatment,sd,data=andro.week.7.a)

#week8.andro
andro.week.8.a<-subset(A.gerardii,Week=="8",na.rm=TRUE)
andro.week8.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=andro.week.8.a,na.action = na.exclude) #transformed
anova(andro.week8.lm.a)
plot(resid(andro.week8.lm.a))
hist(resid(andro.week8.lm.a))
#**signiciant

#week9.andro
andro.week.9.a<-subset(A.gerardii,Week=="9",na.rm=TRUE)
andro.week9.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=andro.week.9.a,na.action = na.exclude) #transformed
anova(andro.week9.lm.a)
plot(resid(andro.week9.lm.a))
hist(resid(andro.week9.lm.a)) #little funky
#n.s.

#week10.andro
andro.week.10.a<-subset(A.gerardii,Week=="10",na.rm=TRUE)
andro.week10.lm.a<-lme(A.transformed~treatment,random=~1|Block/Plant,data=andro.week.10.a,na.action = na.exclude) #transformed
anova(andro.week10.lm.a)
plot(resid(andro.week10.lm.a))
hist(resid(andro.week10.lm.a)) 
#n.s.

#week11.andro
andro.week.11.a<-subset(A.gerardii,Week=="11",na.rm=TRUE)
andro.week11.lm.a<-lme(A~treatment,random=~1|Block/Plant,data=andro.week.11.a,na.action = na.exclude)
anova(andro.week11.lm.a)
plot(resid(andro.week11.lm.a))
hist(resid(andro.week11.lm.a)) 
#n.s.

#about two weeks for andropogon to recover to levels of well-watered controls

#####P.smithii#
P.smithii<-subset(drad.phys,Species=="P.smithii")
p.smithii.drought<-subset(P.smithii,subset=="Drought")
p.smithii.recovery<-subset(P.smithii,subset.recovery=="Recovery")

mean.a.pasm<-aggregate(A~treatment,mean,data=P.smithii)
sd.a.pasm<-aggregate(A~treatment,sd,data=P.smithii)

#drought full model
full.p.smithii.drought<-lme(A~treatment*Week,random=~1|Block/Plant,data=p.smithii.drought,na.action = na.exclude)
Anova(full.p.smithii.drought,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
plot(resid(full.p.smithii.drought)) #homegenity of variances met
dagoTest(resid(full.p.smithii.drought)) #normality of model residuals met
hist(resid(full.p.smithii.drought))

#treatment*week interaction significant

#recovery full model
full.p.smithii.recovery<-lme(A.transformed~treatment*Week,random=~1|Block/Plant,data=p.smithii.recovery,na.action = na.exclude) #transformed
Anova(full.p.smithii.recovery, contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
plot(resid(full.p.smithii.recovery)) #homegenity of variances met
dagoTest(resid(full.p.smithii.recovery)) #normality of model residuals met
hist(resid(full.p.smithii.recovery))

#signiciant interaction; treatment differences depended on the week of the recovery

#week1.pasm
pasm.week.1<-subset(P.smithii,Week=="1",na.rm=TRUE)
pasm.week1.lm.a<-lme(A.transformed~treatment,data=pasm.week.1,random=~1|Block/Plant,na.action = na.exclude) #transformed
anova(pasm.week1.lm.a)
plot(resid(pasm.week1.lm.a)) #homegenity of variances met
hist(resid(pasm.week1.lm.a))

#n.s., but .055, worth mentioning that even at the very beginning of the drought, differences were emerging

#week3.pasm
pasm.week.3<-subset(P.smithii,Week=="3",na.rm=TRUE)
pasm.week3.lm.a<-lme(A.transformed~treatment,data=pasm.week.3,random=~1|Block/Plant,na.action = na.exclude) #transformed
anova(pasm.week3.lm.a)
plot(resid(pasm.week3.lm.a)) #homegenity of variances met
hist(resid(pasm.week3.lm.a))
#signiciant

#week5.pasm
pasm.week.5<-subset(P.smithii,Week=="5",na.rm=TRUE)
pasm.week5.lm.a<-lme(A~treatment,data=pasm.week.5,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week5.lm.a)
plot(resid(pasm.week5.lm.a)) #homegenity of variances met
hist(resid(pasm.week5.lm.a)) #looks normal
#signiciant 

#week7.pasm
pasm.week.7<-subset(P.smithii,Week=="7",na.rm=TRUE)
pasm.week7.lm.a<-lme(A~treatment,data=pasm.week.7,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week7.lm.a)
plot(resid(pasm.week7.lm.a)) #homegenity of variances met
hist(resid(pasm.week7.lm.a))
#significant

mean.a.pasm.peak<-aggregate(A~treatment,mean,data=pasm.week.7)
sd.a.pasm.peak<-aggregate(A~treatment,sd,data=pasm.week.7)

#week8.pasm
pasm.week.8<-subset(P.smithii,Week=="8",na.rm=TRUE)
pasm.week8.lm.a<-lme(A.transformed~treatment,data=pasm.week.8,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week8.lm.a)
plot(resid(pasm.week8.lm.a)) #homegenity of variances met
hist(resid(pasm.week8.lm.a)) #looks meh
#signiciant, though weakened 

#week9.pasm
pasm.week.9<-subset(P.smithii,Week=="9",na.rm=TRUE)
pasm.week9.lm.a<-lme(A~treatment,data=pasm.week.9,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week9.lm.a)
plot(resid(pasm.week9.lm.a)) #homegenity of variances met
hist(resid(pasm.week9.lm.a)) #quite skewed
#signiciant, but because of a switch (PD higher than C). previously droughted outperfermoed controls...

#week10.pasm
pasm.week.10<-subset(P.smithii,Week=="10",na.rm=TRUE)
summary(pasm.week.10)
pasm.week10.lm.a<-lme(A~treatment,data=pasm.week.10,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week10.lm.a)
plot(resid(pasm.week10.lm.a)) #homegenity of variances met
hist(resid(pasm.week10.lm.a))
#n.s. event he it looks like they should be


#week11.pasm
pasm.week.11<-subset(P.smithii,Week=="11",na.rm=TRUE)
pasm.week11.lm.a<-lme(A~treatment,data=pasm.week.11,random=~1|Block/Plant,na.action = na.exclude)
anova(pasm.week11.lm.a)
plot(resid(pasm.week11.lm.a)) #homegenity of variances met
hist(resid(pasm.week11.lm.a))
#significant

######B.eriopoda#
B.eriopoda<-subset(drad.phys,Species=="B.eriopoda")
B.eriopoda.drought<-subset(B.eriopoda,subset=="Drought")
B.eriopoda.recovery<-subset(B.eriopoda,subset.recovery=="Recovery")

mean.a.boer<-aggregate(A~treatment,mean,data=B.eriopoda)
sd.a.boer<-aggregate(A~treatment,sd,data=B.eriopoda)

#drought full model
full.B.eriopoda.drought<-lme(A.transformed~treatment*Week,random=~1|Block/Plant,data=B.eriopoda.drought,na.action = na.exclude)
Anova(full.B.eriopoda.drought,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
plot(resid(full.B.eriopoda.drought)) #homegenity of variances met
dagoTest(resid(full.B.eriopoda.drought)) #normality of model residuals met after transformation
hist(resid(full.B.eriopoda.drought))

#treatment by week interaction significant, impact of drought treatment increased through time

#recovery full model
full.B.eriopoda.recovery<-lme(A~treatment*Week,random=~1|Block/Plant,data=B.eriopoda.recovery,na.action = na.exclude)
Anova(full.B.eriopoda.recovery,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
plot(resid(full.B.eriopoda.recovery)) #homegenity of variances met
dagoTest(resid(full.B.eriopoda.recovery)) #normality of model residuals met after transformation
hist(resid(full.B.eriopoda.recovery))

#treatment by week interactions signficant, suggesting treatment differences lessened through time

#week1.boer
boer.week.1<-subset(B.eriopoda,Week=="1",na.rm=TRUE)
boer.week1.lm.a<-lme(A.transformed~treatment,data=boer.week.1,random=~1|Block/Plant,na.action = na.exclude) #transformed
anova(boer.week1.lm.a)
plot(resid(boer.week1.lm.a)) #homegenity of variances met
hist(resid(boer.week1.lm.a))
#n.s.

#week3.boer
boer.week.3<-subset(B.eriopoda,Week=="3",na.rm=TRUE)
boer.week3.lm.a<-lme(A~treatment,data=boer.week.3,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week3.lm.a)
plot(resid(boer.week3.lm.a)) #homegenity of variances met
hist(resid(boer.week3.lm.a))
#n.s.

#week5.boer
boer.week.5<-subset(B.eriopoda,Week=="5",na.rm=TRUE)
boer.week5.lm.a<-lme(A.transformed~treatment,data=boer.week.5,random=~1|Block/Plant,na.action = na.exclude) #transformed
anova(boer.week5.lm.a)
plot(resid(boer.week5.lm.a)) #homegenity of variances met
hist(resid(boer.week5.lm.a))
#significant

#week7.boer
boer.week.7<-subset(B.eriopoda,Week=="7",na.rm=TRUE)
boer.week7.lm.a<-lme(A~treatment,data=boer.week.7,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week7.lm.a)
plot(resid(boer.week7.lm.a)) #homegenity of variances met
hist(resid(boer.week7.lm.a))
#signiciant

mean.a.boer.peak<-aggregate(A~treatment,mean,data=boer.week.7)
sd.a.boer.peak<-aggregate(A~treatment,sd,data=boer.week.7)

#week8.boer
boer.week.8<-subset(B.eriopoda,Week=="8",na.rm=TRUE)
boer.week8.lm.a<-lme(A~treatment,data=boer.week.8,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week8.lm.a)
plot(resid(boer.week8.lm.a)) #homegenity of variances met
hist(resid(boer.week8.lm.a)) #looks meh
#signiciant

#week9.boer
boer.week.9<-subset(B.eriopoda,Week=="9",na.rm=TRUE)
boer.week9.lm.a<-lme(A~treatment,data=boer.week.9,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week9.lm.a)
plot(resid(boer.week9.lm.a)) #homegenity of variances met
hist(resid(boer.week9.lm.a)) 
#n.s.

#week10.boer
boer.week.10<-subset(B.eriopoda,Week=="10",na.rm=TRUE)
boer.week10.lm.a<-lme(A~treatment,data=boer.week.10,random=~1|Block/Plant,na.action = na.exclude)
anova(boer.week10.lm.a)
plot(resid(boer.week10.lm.a)) #homegenity of variances met
hist(resid(boer.week10.lm.a)) 
#n.s.

#week11.boer
boer.week.11<-subset(B.eriopoda,Week=="11",na.rm=TRUE)
boer.week11.lm.a<-lme(A.transformed~treatment,data=boer.week.11,random=~1|Block/Plant,na.action = na.exclude) #transformed
anova(boer.week11.lm.a)

plot(resid(boer.week11.lm.a)) #homegenity of variances met
hist(resid(boer.week11.lm.a)) 

#check to see abot the relationship between flor and A
A.phips2.model.lm<-lm(A~phips2,data=drad.phys.2,na.action = na.exclude)
summary(A.phips2.model.lm)
#R-square=.61
plot(A~phips2,data=drad.phys.2)

A.phips2.model<-lme(A~phips2*Species,random=~1|Block/Plant,data=drad.phys,na.action = na.exclude)
Anova(A.phips2.model,type=3)

###Light-adapted flourscence######

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

###leaf water potential#######
drad.wp<-read.csv('C:/Users/A02296270/Desktop/Side_projects/DOE/doe_greenhouse_experiment_data/DRAD_chlor_wp.csv',header=TRUE)
head(drad.wp)
hist(drad.wp$mpa)
#full

#separate by control to see if there are baseline differences
wp.control<-subset(drad.wp,treatment=='C')
species.wp<-lme(water.potential~species,random=~1|Block/plant,data=wp.control,na.action = na.exclude)
qqnorm(resid(species.wp)) #some noise at the tails
qqline(resid(species.wp))
dagoTest(resid(species.wp)) #normality of model residuals met
hist(resid(species.wp)) #looks good

anova(species.wp) #signiciant baseline differences among species, analyze separately

#####a.gerardii##
andro.wp<-subset(drad.wp,species=="A.gerardii",na.rm=TRUE)
summary(andro.wp) #full distrubtion of w.p. values for table
andro.wp.full<-lme(sqrt(water.potential)~treatment*Time,random=~1|Block/plant,data=andro.wp,na.action = na.exclude) #square root transformed
qqnorm(resid(andro.wp.full)) #some noise at the tails
qqline(resid(andro.wp.full))
dagoTest(resid(andro.wp.full)) #normality of model residuals barely met, perfromed tranformation to betetr meet htis asumption
hist(resid(andro.wp.full))

Anova(andro.wp.full,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#signficiant treatment*time interactions, implying the impact of the treatment depended on the timing

#get mean wp of control
mean.wp.andro<-aggregate(mpa~treatment,mean,data=andro.wp)
sd.wp.andro<-aggregate(mpa~treatment,sd,data=andro.wp)
#mean: -0.8, sd:0.27

#week1.andro
andro.wp.week.1<-subset(andro.wp,Week=="1",na.rm=TRUE)
andro.week1.lme.wp<-lme(sqrt(water.potential) ~ treatment,random=~1|Block/plant,data=andro.wp.week.1,na.action = na.exclude) #transformed
anova(andro.week1.lme.wp)
hist(resid(andro.week1.lme.wp)) #not the most normal distribution. still, groups mostly identical
#not significant

#week7.andro
andro.wp.week.7<-subset(andro.wp,Week=="7",na.rm=TRUE)
andro.wp.week.7.lme.wp<-lme(sqrt(water.potential) ~treatment,random=~1|Block/plant,data=andro.wp.week.7,na.action = na.exclude) #transformed
anova(andro.wp.week.7.lme.wp) #significant
hist(resid(andro.wp.week.7.lme.wp))
#highly signiciant 

#mean at peak
mean.wp.andro.peak<-aggregate(mpa~treatment,mean,data=andro.wp.week.7)
sd.wp.andro.peak<-aggregate(mpa~treatment,sd,data=andro.wp.week.7)

#week11.andro
andro.wp.week.11<-subset(andro.wp,Week=="11",na.rm=TRUE)
andro.wp.week.11.lme.wp<-lme(sqrt(water.potential) ~treatment,random=~1|Block/plant,data=andro.wp.week.11,na.action = na.exclude) #transformed
anova(andro.wp.week.11.lme.wp)
hist(resid(andro.wp.week.11.lme.wp)) #still a little skew, looks ok
#not significant, p=.16

##B.gracilis
bogr.wp<-subset(drad.wp,species=="B.gracilis",na.rm=TRUE)
summary(bogr.wp)
bogr.wp.full<-lme(sqrt(water.potential)~treatment*Time,random=~1|Block/plant,data=andro.wp,na.action = na.exclude) #transformed
qqnorm(resid(bogr.wp.full)) #some noise at the tails
qqline(resid(bogr.wp.full))
dagoTest(resid(bogr.wp.full)) #normality of model residuals barely met, perfromed tranformation to betetr meet htis asumption
hist(resid(bogr.wp.full))

Anova(bogr.wp.full,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#signiciant treatmnt*time interaction

#mean of control
mean.wp.bogr.control<-aggregate(mpa~treatment,mean,data=bogr.wp)
sd.wp.bogr.control<-aggregate(mpa~treatment,sd,data=bogr.wp)
#mean: -1.66, sd: 0.41

#week1.bogr
bogr.wp.week.1<-subset(bogr.wp,Week=="1",na.rm=TRUE)
bogr.week1.lme.wp<-lme(sqrt(water.potential) ~ treatment,random=~1|Block/plant,data=bogr.wp.week.1,na.action = na.exclude) #transformation applied
anova(bogr.week1.lme.wp)
hist(resid(bogr.week1.lme.wp))
#not significant, p=.64

#week7.bogr
bogr.wp.week.7<-subset(bogr.wp,Week=="7",na.rm=TRUE)
bogr.wp.week.7.lme.wp<-lme(sqrt(water.potential)~treatment,random=~1|Block/plant,data=bogr.wp.week.7,na.action = na.exclude)
anova(bogr.wp.week.7.lme.wp)
hist(resid(bogr.wp.week.7.lme.wp))
#highly signiciant

#week11.bogr
bogr.wp.week.11<-subset(bogr.wp,Week=="11",na.rm=TRUE)
bogr.wp.week.11.lme.wp<-lme(water.potential ~treatment,random=~1|Block/plant,data=bogr.wp.week.11,na.action = na.exclude)
anova(bogr.wp.week.11.lme.wp)
hist(resid(bogr.wp.week.11.lme.wp))
#not significant

##B.eriopoda#
boer.wp<-subset(drad.wp,species=="B.eriopoda",na.rm=TRUE)
summary(boer.wp)
boer.wp.full<-lme(water.potential~treatment*Time,random=~1|Block/plant,data=boer.wp,na.action = na.exclude)
Anova(boer.wp.full,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
qqnorm(resid(boer.wp.full)) #some noise at the tails
qqline(resid(boer.wp.full))
dagoTest(resid(boer.wp.full)) #normality of model residuals met
hist(resid(boer.wp.full))

#significant treatment*time interaction

#week1.boer
boer.wp.week.1<-subset(boer.wp,Week=="1",na.rm=TRUE)
boer.week1.lme.wp<-lme(sqrt(water.potential)~treatment,random=~1|Block/plant,data=boer.wp.week.1,na.action = na.exclude) #transformed
anova(boer.week1.lme.wp)
hist(resid(boer.week1.lme.wp)) #normal, little bit of skew
#not significant

#week7.boer
boer.wp.week.7<-subset(boer.wp,Week=="7",na.rm=TRUE)
boer.wp.week.7.lme.wp<-lme(sqrt(water.potential)~treatment,random=~1|Block/plant,data=boer.wp.week.7,na.action = na.exclude)
anova(boer.wp.week.7.lme.wp)
hist(resid(boer.wp.week.7.lme.wp)) 
#highly signiciant

#week11.boer
boer.wp.week.11<-subset(boer.wp,Week=="11",na.rm=TRUE)
boer.wp.week.11.lme.wp<-lme(sqrt(water.potential) ~treatment,random=~1|Block/plant,data=boer.wp.week.11,na.action = na.exclude)
anova(boer.wp.week.11.lme.wp)
hist(resid(boer.wp.week.11.lme.wp)) 
#not significant

##P.smithii
pasm.wp<-subset(drad.wp,species=="P.smithii",na.rm=TRUE)
summary(pasm.wp)
pasm.wp.full<-lme(sqrt(water.potential)~treatment*Time,random=~1|Block/plant,data=pasm.wp,na.action = na.exclude)
qqnorm(resid(pasm.wp.full)) #some noise at the tails
qqline(resid(pasm.wp.full))
dagoTest(resid(pasm.wp.full)) #normality of model residuals not met.
hist(resid(pasm.wp.full)) #Appears to be driver by one outlier.

View(resid(pasm.wp.full)) #pasm38 outlier. this was also written in my notes during experiment.
pasm.wp.2 <- pasm.wp %>% dplyr::filter(!(plant == 'pasm38')) #remove outlier plant
View(recovery.2)

#try again without outlier
pasm.wp.full.2<-lme(water.potential~treatment*Time,random=~1|Block/plant,data=pasm.wp.2,na.action = na.exclude)
qqnorm(resid(pasm.wp.full.2)) #some noise at the tails
qqline(resid(pasm.wp.full.2))
dagoTest(resid(pasm.wp.full.2)) #normality of model residuals not met.
hist(resid(pasm.wp.full.2)) #looks good

Anova(pasm.wp.full.2,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#significant treatment*time interaction

#week1.pasm
pasm.wp.week.1<-subset(pasm.wp.2,Week=="1",na.rm=TRUE)
pasm.week1.lme.wp<-lme(sqrt(water.potential) ~ treatment,random=~1|Block/plant,data=pasm.wp.week.1,na.action = na.exclude) #transformed
anova(pasm.week1.lme.wp)
hist(resid(pasm.week1.lme.wp)) #meh
#not significant

#week7.pasm
pasm.wp.week.7<-subset(pasm.wp,Week=="7",na.rm=TRUE)
pasm.wp.week.7.lme.wp<-lme(sqrt(water.potential) ~ treatment,random=~1|Block/plant,data=pasm.wp.week.7,na.action = na.exclude) #transformed
anova(pasm.wp.week.7.lme.wp)
hist(resid(pasm.wp.week.7.lme.wp)) #normal
#highly signiciant

#week11.pasm
pasm.wp.week.11<-subset(pasm.wp,Week=="11",na.rm=TRUE)
pasm.wp.week.11.lme.wp<-lme(sqrt(water.potential) ~ treatment,random=~1|Block/plant,data=pasm.wp.week.11,na.action = na.exclude) #transformed
anova(pasm.wp.week.11.lme.wp)
hist(resid(pasm.wp.week.11.lme.wp)) #normal
#not significant

#######specific leaf area#########

drad.sla<-read.csv('G:/My Drive/Other projects/doe-drought-recovery/DOE_2019/data/DRAD_SLA.csv',header=TRUE)
head(drad.sla)
hist(drad.sla$SLA)
View(drad.sla)

#look at baseline differences among species (controls)
sla.control<-subset(drad.sla,Treatment=='C')
full.sla.control<-lme(SLA~Species,random=~1|Block/Plant,data=sla.control,na.action = na.exclude)
plot(resid(full.sla.control)) #homegenity of variances met
dagoTest(resid(full.sla.control)) #normality of model residuals met
hist(resid(full.sla.control))

anova(full.sla.control)
#significant baseline differences among species

#andro
andro.sla<-subset(drad.sla,Species=="A.gerardii",na.rm=TRUE)
summary(andro.sla)
andro.sla.full<-lme(sqrt(SLA)~Treatment*Time,random=~1|Block/Plant,data=andro.sla,na.action = na.exclude) #transformed
plot(resid(andro.sla.full)) #homegenity of variances met
dagoTest(resid(andro.sla.full)) #normality of model residuals met after transformation
hist(resid(andro.sla.full))
Anova(andro.sla.full,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#nothing signiciant

#week7.andro
andro.sla.week.7<-subset(andro.sla,Time=="Peak drought",na.rm=TRUE)
andro.sla.week.7.lme.wp<-lme(SLA ~Treatment,random=~1|Block/Plant,data=andro.sla.week.7,na.action = na.exclude)
hist(resid(andro.sla.week.7.lme.wp)) #looks fairly normal
anova(andro.sla.week.7.lme.wp)
#no significant differences

#week11.andro
andro.sla.week.11<-subset(andro.sla,Time=="Recovery",na.rm=TRUE)
andro.sla.week.11.lme<-lme(SLA ~Treatment,random=~1|Block/plant,data=andro.sla.week.11,na.action = na.exclude)
hist(resid(andro.sla.week.11.lme)) #looks ok
anova(andro.sla.week.11.lme)
#not significant

#B.gracilis
bogr.sla<-subset(drad.sla,Species=="B.gracilis",na.rm=TRUE)
summary(bogr.sla)
bogr.sla.full<-lme(sqrt(SLA)~Treatment*Time,random=~1|Plant,data=bogr.sla,na.action = na.exclude)
plot(resid(bogr.sla.full)) #homegenity of variances met
dagoTest(resid(bogr.sla.full)) #normality of model residuals not met after transformation
hist(resid(bogr.sla.full)) #look at outlier

#see if removing outlier does anything
View(resid(bogr.sla.full)) #bogr 37 outlier. this was also written in my notes during experiment.
bogr.sla.2 <- bogr.sla %>% dplyr::filter(!(Plant == 'BOGR37')) #remove outlier plant

#try again
bogr.sla.full.2<-lme(sqrt(SLA)~Treatment*Time,random=~1|Plant,data=bogr.sla.2,na.action = na.exclude)
plot(resid(bogr.sla.full.2)) #homegenity of variances met
dagoTest(resid(bogr.sla.full.2)) #normality of model residuals met after removing the outlier
hist(resid(bogr.sla.full.2)) 

Anova(bogr.sla.full.2,type=3)
#Signiciant treatment* time interaction

#week7.bogr
bogr.wp.week.7<-subset(bogr.sla.2,Time=="Peak drought",na.rm=TRUE)
bogr.sla.week.7.lme<-lme(SLA ~Treatment,random=~1|Block/Plant,data=bogr.wp.week.7,na.action = na.exclude)
hist(resid(bogr.sla.week.7.lme)) #looks ok
anova(bogr.sla.week.7.lme)
#not signiciant

#week11.bogr
bogr.wp.week.11<-subset(bogr.sla.2,Time=="Recovery",na.rm=TRUE)
bogr.sla.week.11.lme<-lme(SLA~Treatment,random=~1|Block/Plant,data=bogr.wp.week.11,na.action = na.exclude) #transformed
hist(resid(bogr.sla.week.11.lme)) #looks ok
anova(bogr.sla.week.11.lme)
#highly signiciant (higher in previously droughted plants)

#B.eriopoda
boer.sla<-subset(drad.sla,Species=="B.eriopoda",na.rm=TRUE)
summary(boer.sla)
boer.sla.full<-lme(SLA~Treatment*Time,random=~1|Block/Plant,data=boer.sla,na.action = na.exclude)
plot(resid(boer.sla.full)) #homegenity of variances met
dagoTest(resid(boer.sla.full)) #normality of model residuals met
hist(resid(boer.sla.full)) #looks ok
Anova(boer.sla.full,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)

#treatmena weakly significant, treatment*time weakly significant (p=0.08)

#week7.boer
boer.wp.week.7<-subset(boer.sla,Time=="Peak drought",na.rm=TRUE)
boer.sla.week.7.lme<-lme(sqrt(SLA) ~Treatment,random=~1|Block/Plant,data=boer.wp.week.7,na.action = na.exclude)
hist(resid(boer.sla.week.7.lme))
anova(boer.sla.week.7.lme)
# signiciant (lower)

#week11.boer
boer.wp.week.11<-subset(boer.sla,Time=="Recovery",na.rm=TRUE)
boer.sla.week.11.lme<-lm(SLA ~Treatment,random=~1|Block/Plant,data=boer.wp.week.11,na.action = na.exclude)
hist(resid(boer.sla.week.11.lme)) #looks ok
anova(boer.sla.week.11.lme)
#not signiciant

#P.smthii 
pasm.sla<-subset(drad.sla,Species=="P.smithii",na.rm=TRUE)
summary(pasm.sla)
pasm.sla.full<-lme(SLA~Treatment*Time,random=~1|Block/Plant,data=pasm.sla,na.action = na.exclude)
plot(resid(pasm.sla.full)) #homegenity of variances not met
dagoTest(resid(pasm.sla.full)) #normality of model residuals not met
hist(resid(pasm.sla.full)) #clear outlier pasm 27

#remove the outlier
pasm.sla.2 <- pasm.sla %>% dplyr::filter(!(Plant == 'pasm27')) #remove outlier plant

#try again
pasm.sla.full.2<-lme(SLA~Treatment*Time,random=~1|Block/Plant,data=pasm.sla.2,na.action = na.exclude)
plot(resid(pasm.sla.full.2)) #homegenity of variances met
dagoTest(resid(pasm.sla.full.2)) #normality of model residuals met after removing the outlier
hist(resid(pasm.sla.full.2))

Anova(pasm.sla.full.2,contrasts=list(topic=contr.sum, sys=contr.sum), type=3)
#significant treatment*time interaction

#week7.pasm
pasm.wp.week.7<-subset(pasm.sla.2,Time=="Peak drought",na.rm=TRUE)
pasm.sla.week.7.lme<-lme(SLA ~Treatment,random=~1|Block/Plant,data=pasm.wp.week.7,na.action = na.exclude)
hist(resid(pasm.sla.week.7.lme)) #little right skewed but ok
anova(pasm.sla.week.7.lme) 
#significant

#week11.pasm
pasm.wp.week.11<-subset(pasm.sla.2,Time=="Recovery",na.rm=TRUE)
pasm.sla.week.11.lme<-lme(SLA ~Treatment,random=~1|Block/Plant,data=pasm.wp.week.11,na.action = na.exclude)
hist(resid(pasm.sla.week.11.lme)) #looks ok
anova(pasm.sla.week.11.lme)
#signiciant (higher)

#area per leaf#############
drad.morph<-read.csv('G:/My Drive/Other projects/doe-drought-recovery/DOE_2019/data/drad_morphology_for_analysis.csv')
head(drad.morph)
summary(drad.morph)
View(drad.morph)

leaf.ag.morph<-aggregate(average~Plant + Species + Block + Week + Treatment,mean,data=drad.morph)

#subset
#leaf.dr<-subset(leaf.ag.morph,treatment=='DR')

#andro
leaf.dr.andro <- subset(leaf.ag.morph,Species=='A.gerardii')
leaf.dr.7.11 <- subset(leaf.dr.andro, Week==c('11') | Week==c('7'))

#get rid of unclear data values of bogr and andro29
leaf.dr.7.11.2 <-leaf.dr.7.11 %>% dplyr::filter(!(Plant==c('BOGR29'))) %>%
  dplyr::filter(!(Plant==c('ANGE29')))

#week 7
andro.leaf.size.week.7<-subset(leaf.dr.7.11.2,Week=='7')
andro.leaf.size.7.lme<-lme(average~treatment,random=~1|Block/Plant,data=andro.leaf.size.week.7,na.action = na.exclude)
plot(resid(andro.leaf.size.7.lme))
qqnorm(resid(andro.leaf.size.7.lme)) #some noise at the tails
qqline(resid(andro.leaf.size.7.lme))
hist(resid(andro.leaf.size.7.lme)) # a little meh
anova(andro.leaf.size.7.lme)
#significantly different

#week 11
andro.leaf.size.week.11<-subset(leaf.dr.7.11.2,Week=='11')
andro.leaf.size.11.lme<-lme(average~treatment,random=~1|Block/Plant,data=andro.leaf.size.week.11,na.action = na.exclude)
plot(resid(andro.leaf.size.11.lme))
qqnorm(resid(andro.leaf.size.11.lme)) #looks good
qqline(resid(andro.leaf.size.11.lme))
hist(resid(andro.leaf.size.11.lme)) # a little meh
anova(andro.leaf.size.11.lme)
#not significant, weakly singificant P=0.07

#bogr
leaf.dr.bogr <- subset(leaf.ag.morph,Species=='B.gracilis')
leaf.dr.bogr.7.11 <- subset(leaf.dr.bogr, Week==c('11') | Week==c('7'))

#week 7
leaf.bogr.week.7<-subset(leaf.dr.bogr.7.11,Week=='7')
bogr.leaf.size.7.lme<-lme(average~treatment,random=~1|Block/Plant,data=leaf.bogr.week.7,na.action = na.exclude)
qqnorm(resid(bogr.leaf.size.7.lme)) 
qqline(resid(bogr.leaf.size.7.lme)) #some noise at top tails
hist(resid(bogr.leaf.size.7.lme)) # looks ok
anova(bogr.leaf.size.7.lme)
#signficiant reduction

#week 11
leaf.bogr.week.11<-subset(leaf.dr.bogr.7.11,Week=='11')
bogr.leaf.size.11.lme<-lme(sqrt(average)~treatment,random=~1|Block/Plant,data=leaf.bogr.week.11,na.action = na.exclude) #transformed
qqnorm(resid(bogr.leaf.size.11.lme)) 
qqline(resid(bogr.leaf.size.11.lme)) #some noise at top tails
hist(resid(bogr.leaf.size.11.lme)) # looks better after transformation
anova(bogr.leaf.size.11.lme)
#not signficiant

#boer
leaf.dr.boer <- subset(leaf.ag.morph,Species=='B.eriopoda')
leaf.dr.boer.7.11 <- subset(leaf.dr.boer, Week==c('11') | Week==c('7'))

#week 7
leaf.boer.week.7<-subset(leaf.dr.boer.7.11, Week=='7')
boer.leaf.size.7.lme<-lme(sqrt(average)~treatment,random=~1|Block/Plant,data=leaf.boer.week.7,na.action = na.exclude) #transformed
qqnorm(resid(boer.leaf.size.7.lme)) #some noise at the tails, transformation helps
qqline(resid(boer.leaf.size.7.lme))
hist(resid(boer.leaf.size.7.lme)) # looks good after transformation
anova(boer.leaf.size.7.lme)
#not significant

#week 11
leaf.boer.week.11<-subset(leaf.dr.boer.7.11, Week=='11')
boer.leaf.size.11.lme<-lme(average~treatment,random=~1|Block/Plant,data=leaf.boer.week.11,na.action = na.exclude)
qqnorm(resid(boer.leaf.size.11.lme)) #look ok
qqline(resid(boer.leaf.size.11.lme))
hist(resid(boer.leaf.size.11.lme)) # looks ok
 anova(boer.leaf.size.11.lme)
#sig. difference

#pasm
leaf.dr.pasm <- subset(leaf.ag.morph,Species=='P.smithii')
leaf.dr.pasm.7.11 <- subset(leaf.dr.pasm, Week==c('11') | Week==c('7'))

#week 7
leaf.pasm.week.7<-subset(leaf.dr.pasm.7.11, Week=='7')
pasm.leaf.size.7.lme<-lme(average~treatment,random=~1|Block/Plant,data=leaf.pasm.week.7,na.action = na.exclude) #transformed
qqnorm(resid(pasm.leaf.size.7.lme)) #some noise at the tails, transformation helps
qqline(resid(pasm.leaf.size.7.lme))
hist(resid(pasm.leaf.size.7.lme)) # looks good after transformation
anova(boer.leaf.size.7.lme)

#remove clear outlier: pasm10
leaf.dr.pasm.7.11.2 <- leaf.dr.pasm.7.11 %>% dplyr::filter(!(Plant=='PASM10'))
leaf.pasm.week.7.2<-subset(leaf.dr.pasm.7.11.2, Week=='7')
pasm.leaf.size.7.lme.2<-lme(sqrt(average)~treatment,random=~1|Block/Plant,data=leaf.pasm.week.7.2,na.action = na.exclude) #transformed
qqnorm(resid(pasm.leaf.size.7.lme.2)) #some noise at the tails, transformation helps
qqline(resid(pasm.leaf.size.7.lme.2))
hist(resid(pasm.leaf.size.7.lme.2)) # looks good 
anova(pasm.leaf.size.7.lme.2)
#signficiant 

leaf.pasm.week.11<-subset(leaf.dr.pasm.7.11.2, Week=='11')
pasm.leaf.size.11.lme<-lme(sqrt(average)~treatment,random=~1|Block/Plant,data=leaf.pasm.week.11,na.action = na.exclude) #transformed
qqnorm(resid(pasm.leaf.size.11.lme)) #some noise at the tails, transformation helps
qqline(resid(pasm.leaf.size.11.lme))
hist(resid(pasm.leaf.size.11.lme)) # looks good after transformation
anova(pasm.leaf.size.11.lme)
#not significant

pasm.leaf.size.lme<-lme(sqrt(average)~Week,random=~1|Block/Plant,data=leaf.dr.pasm.7.11,na.action = na.exclude) #transformed
qqnorm(resid(pasm.leaf.size.lme)) #some noise at the tails
qqline(resid(pasm.leaf.size.lme))
hist(resid(pasm.leaf.size.lme)) # looks better after transformation
anova(pasm.leaf.size.lme)
#significant increases

###stem number#######
stem.ag.morph<-aggregate(tillers~Plant + Species + Block + Week + treatment,mean,data=drad.morph)
View(drad.morph)

#andro
stem.dr.andro <- subset(stem.ag.morph,Species=='A.gerardii')
stem.dr.7.11 <- subset(stem.dr.andro, Week==c('11') | Week==c('7'))

#get rid of unclear data values of bogr and andro29
stem.dr.7.11.2 <-stem.dr.7.11 %>% 
  dplyr::filter(!(Plant==c('ANGE29')))

andro.stem.lme<-lme(sqrt(tillers)~Week,random=~1|Block/Plant,data=stem.dr.7.11.2,na.action = na.exclude) #transformed
qqnorm(resid(andro.stem.lme)) #some noise at the tails
qqline(resid(andro.stem.lme))
hist(resid(andro.stem.lme)) # better after transformation
anova(andro.stem.lme)
#not significantly different!

#b.gracilis
#bogr
stem.dr.bogr <- subset(stem.ag.morph,Species=='B.gracilis')
stem.dr.bogr.7.11 <- subset(stem.dr.bogr, Week==c('11') | Week==c('7'))

bogr.stem.lme<-lme(tillers~Week,random=~1|Block/Plant,data=stem.dr.bogr.7.11 ,na.action = na.exclude)
qqnorm(resid(bogr.stem.lme)) #some noise at the tails
qqline(resid(bogr.stem.lme))
hist(resid(bogr.stem.lme)) # looks ok
anova(bogr.stem.lme)
#significant increase

#boer
stem.dr.boer <- subset(stem.ag.morph,Species=='B.eriopoda')
stem.dr.boer.7.11 <- subset(stem.dr.boer, Week==c('11') | Week==c('7'))

boer.stem.lme<-lme(tillers~Week,random=~1|Block/Plant,data=stem.dr.boer.7.11 ,na.action = na.exclude)
qqnorm(resid(boer.stem.lme)) #some noise at the tails
qqline(resid(boer.stem.lme))
hist(resid(boer.stem.lme)) # looks ok
anova(boer.stem.lme)
#significant increase

#pasm
stem.dr.pasm <- subset(stem.ag.morph,Species=='P.smithii')
stem.dr.pasm.7.11 <- subset(stem.dr.pasm, Week==c('11') | Week==c('7'))

pasm.stem.lme<-lme(sqrt(tillers)~Week,random=~1|Block/Plant,data=stem.dr.pasm.7.11 ,na.action = na.exclude) #transformed
qqnorm(resid(pasm.stem.lme)) #some noise at the tails
qqline(resid(pasm.stem.lme))
hist(resid(pasm.stem.lme)) # looks ok after transformation
anova(pasm.stem.lme)
#signficiant increase

#######root:shoot##########
drad.biomass <-read.csv('G:/My Drive/Other projects/doe-drought-recovery/DOE_2019/data/DRAD_ANPP.csv')
head(drad.biomass)
root.shoot.full.lme<-lme(sqrt(root.shoot)~Species,random=~1|Block/Plant,data=drad.biomass,na.action = na.exclude) #transformed
qqnorm(resid(root.shoot.full.lme)) #some noise at the tails
qqline(resid(root.shoot.full.lme))
dagoTest(resid(root.shoot.full.lme)) #normality of model residuals met
hist(resid(root.shoot.full.lme))
anova(root.shoot.full.lme)
#Significant Species differences in total and in peak drought, so analyze separately

#look at peak drought values across species because it is the basis of comparison
biomass.drought<-subset(drad.biomass,Time=='Peak drought')
root.shoot.full.drought.lme<-lme(root.shoot~Species,random=~1|Block/Plant,data=biomass.drought,na.action = na.exclude)
qqnorm(resid(root.shoot.full.drought.lme)) #some noise at the tails
qqline(resid(root.shoot.full.drought.lme))
dagoTest(resid(root.shoot.full.drought.lme)) #normality of model residuals met
hist(resid(root.shoot.full.drought.lme))
anova(root.shoot.full.drought.lme)
#significant species differences at peak drought

#B.gracilis
b.gracilis.biomass<-subset(drad.biomass,Species=="B.gracilis",na.rm=TRUE)
bogr.root.shoot.full.lme<-lme(sqrt(root.shoot)~Treatment,random=~1|Block/Plant,data=b.gracilis.biomass,na.action = na.exclude) #transformed
hist(resid(bogr.root.shoot.full.lme))
anova(bogr.root.shoot.full.lme)
#signiciant - post-deought lower than peak drought

#A.gerardii
andro.biomass<-subset(drad.biomass,Species=="A.gerardii",na.rm=TRUE)
andro.root.shoot.full.lme<-lme(root.shoot~Treatment,random=~1|Block/Plant,data=andro.biomass,na.action = na.exclude)
hist(resid(andro.root.shoot.full.lme)) #looks ok
anova(andro.root.shoot.full.lme)
#significant - post-drought lower than peak drought

#B.eriopoda
boer.biomass<-subset(drad.biomass,Species=="B.eriopoda",na.rm=TRUE)
boer.root.shoot.full.lme<-lme(sqrt(root.shoot)~Treatment,random=~1|Block/Plant,data=boer.biomass,na.action = na.exclude) #transformed
hist(resid(boer.root.shoot.full.lme))
anova(boer.root.shoot.full.lme)
#not signficant

#not significant
pasm.biomass<-subset(drad.biomass,Species=="P.smithii",na.rm=TRUE)
pasm.root.shoot.full.lme<-lme(sqrt(root.shoot)~Treatment,random=~1|Block/Plant,data=pasm.biomass,na.action = na.exclude) #transformed
hist(resid(pasm.root.shoot.full.lme))
anova(pasm.root.shoot.full.lme)
#moderatly signiciant

?position_dodge()
dodge <- position_dodge(.8)
head(drad.phys)
drad.phys$wue<-drad.phys$A/drad.phys$E


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

######for finding starting poin of model scenario#############
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
