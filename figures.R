#manuscript figures
library(ggplot2)
#produce datasets for figures

#######functions######
ser <- function(x) {
  
  n <- sum(!is.na(x))
  se <- sd(x, na.rm = T)/sqrt(n)
  
  return(se)
}


hospital_names <- list(
  '1'="Mesic prairie",
  '2'="Mixed-grass prairie",
  '3'="Semi-arid steppe",
  '4'="Desert grassland"
)



hospital_labeller <- function(variable,value){
  return(hospital_names[value])
}

#############soil moisture#############

head(drad.gwc)
View(drad.gwc)
dodge.gwc <- position_dodge(.9)

ggplot(drad.gwc,aes(Day,newgwc*100,color=Treatment,na.rm=TRUE)) +
  ylab('% Soil water content') +
  facet_wrap(~plotid,labeller=hospital_labeller,nrow=2) +
  stat_summary(geom="line",fun.y="mean",size=1,aes(color=as.factor(Treatment)),position=dodge) +
  scale_colour_manual(values=c('DR'='black','C'='grey'),name="") +
  xlab("Week") +
  theme(
    axis.text = element_text(color='black',size=14),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=1),
    legend.text = element_text(size=8),
    legend.position = c(.08,.09),
    legend.key = element_blank(),
    panel.background = element_rect(fill=NA),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    #panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    strip.text = element_text(size=15),
    strip.background = element_rect(
      color="black", fill="white", size=0.75, linetype="solid"),
    axis.line.y = element_line(colour = "black"))

ggsave("drad_gwc_plot.pdf",width = 6, height = 5, units = c("in"))

#######leaf water potential##########
eff.sizes<-read.csv('G:/My Drive/Other projects/doe-drought-recovery/DOE_2019/data/drad_effect_sizes_2.csv')
eff.sizes.water.potential<-subset(eff.sizes,subgroup=='water.potential')
eff.sizes.water.potential$week.2<-as.numeric(as.character(eff.sizes.water.potential$Week))
dodge.eff <- position_dodge(0.7)

str(eff.sizes.water.potential)
  #make standard error limits
limits.eff<-aes(ymax = sup, ymin = inf)
?geom_errorbar
ggplot(eff.sizes.water.potential,aes(as.factor(week.2),effect,shape=as.factor(ID),na.rm=TRUE)) +
  scale_x_discrete(labels=c("1" = "Pre-drought", "7"="Peak drought",
  "11" = "End of recovery")) +
  geom_hline(yintercept = c(0),color="black",size=1)  +
  geom_hline(yintercept = c(-0.8),color="grey",size=1)  +
  geom_errorbar(aes(ymax = 0+sup, ymin = 0+inf),position=position_dodge(width=0.7),width=.4) +
  #stat_summary(geom="line",fun.y="mean",position=dodge) +
  stat_summary(geom="point",fun.y="mean",size=7,fill='white',aes(shape=as.factor(ID)),position=dodge.eff) +
  scale_shape_manual(values=c('1'=21,'2'=22,'3'=23,'4'=24),name="",
  labels=c('1'='Mesic prairie','2'='Mixed-grass prairie','3'='Semi-arid steppe','4'='Desert grassland')) +
  ylab('Leaf water potential effect size') +
  xlab('') +
  theme(
    axis.text = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=20),
    legend.text = element_text(size=10),
    legend.position = c(.2,.2),
    legend.key = element_blank(),
    panel.background = element_rect(fill=NA),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    #panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    #strip.text = element_text(size=15),
    #strip.background = element_rect(
      #color="black", fill="white", size=0.75, linetype="solid"),
    axis.line.y = element_line(colour = "black"))

ggsave("drad_wp_plot.pdf",width = 6, height = 5, units = c("in"))
####net photosynthesis#################

#produce dataframe
head(drad.phys)
#outliers removed:
ag.mean.a<-aggregate(A~plotid + Week + treatment + Species, mean,data=drad.phys)
ag.ser.a<-aggregate(A~plotid + Week + treatment + Species, ser,data=drad.phys)
merge.stuff.a<-merge(ag.mean.a,ag.ser.a,by=c("Week","Species","treatment"))

#jitter a little
dodge <- position_dodge(.5)

#make standard error limits
limits.a<-aes(ymax = A.x- A.y, ymin = A.x+A.y)

ggplot(merge.stuff.a,aes(Week,A.x,fill=treatment,na.rm=TRUE)) +
  #scale_x_discrete(labels=c("1" = "Mesic prairie", "2"="Mixed-grass prairie",
   #"3" = "Semi-arid steppe","4"="Desert grassland")) +
  geom_vline(xintercept = 7,color="gray80",size=3)  +
  stat_summary(geom="line",fun.y="mean",position=dodge) +
  geom_errorbar(limits.a,width=0.5,position = dodge) +
  stat_summary(geom="point",fun.y="mean",size=3.5,color="black",aes(fill=as.factor(treatment)),shape=21,position=dodge) +
  scale_fill_manual(values=c('DR'='black','C'='white'),name="") +
  #scale_fill_manual(values=c('B.gracilis'='black','A.gerardii'='white'),name="Region",
  #labels=c('B.gracilis'='Semi-arid steppe','A.gerardii'='Mesic prairie')) +
  #ylab("Area per leaf (cm^2") +
  #ylab(bquote('Area per leaf ('*cm^2*')')) +
  #scale_x_discrete(labels=c("1" = "Mesic prairie", "2"="Mixed-grass prairie",
  # "3" = "Semi-arid steppe","4"="Desert grassland")) +
  ylab(bquote('Net photosynthesis ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) +
  facet_wrap(~plotid.x,labeller=hospital_labeller,nrow=2) +
  xlab("Week") +
  #coord_flip() +
  theme(
    axis.text = element_text(color='black',size=14),
    axis.title = element_text(color='black',size=18),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=20),
    legend.text = element_text(size=17),
    legend.position = c(.1,.14),
    legend.key = element_blank(),
    panel.background = element_rect(fill=NA),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    #panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    strip.text = element_text(size=15),
    strip.background = element_rect(
      color="black", fill="white", size=0.75, linetype="solid"),
    axis.line.y = element_line(colour = "black"))

ggsave("drad_A_plot.pdf",width = 6, height = 5, units = c("in"))
