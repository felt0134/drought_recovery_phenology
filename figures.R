#manuscript figures


####net photosynthesis#################

?position_dodge()
dodge <- position_dodge(.8)
head(drad.phys)
drad.phys$wue<-drad.phys$A/drad.phys$E

library(ggplot2)
ggplot(drad.phys,aes(Region,wue,color=treatment,na.rm=TRUE)) +
  #scale_x_discrete(labels=c("1" = "Mesic prairie", "2"="Mixed-grass prairie",
  # "3" = "Semi-arid steppe","4"="Desert grassland")) +
  #geom_vline(xintercept = 7:11,color="gray80")  +
  #stat_summary(geom="line",fun.y="mean",position=dodge) +
  stat_summary(geom="bar",fun.y="mean",position=dodge) +
  #geom_line() +
  #geom_errorbar(aes(ymin=a.perc.x-a.perc.y, ymax=a.perc.x + a.perc.y), width=.5,position=dodge) +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1,se=FALSE,color="red")
  facet_wrap(~Region) +
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