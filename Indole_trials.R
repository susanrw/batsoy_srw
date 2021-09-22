# Data import and cleaning ------------------------------------------------


#read in all data
{ d1.1 <- read.csv(file="indole_4672_d.csv",head=TRUE)
d2.1 <- read.csv(file="indole_4614_d.csv",head=TRUE)
d3.1 <- read.csv(file="indole_4604_d.csv",head=TRUE)
d4.1 <- read.csv(file="indole_1676_d.csv",head=TRUE)
d5.1 <- read.csv(file="indole_0505_d.csv",head=TRUE)
c1.1 <- read.csv(file="indole_4655_c.csv",head=TRUE)
c2.1 <- read.csv(file="indole_2198_c.csv",head=TRUE)
c3.1 <- read.csv(file="indole_4608_c.csv",head=TRUE)
c4.1 <- read.csv(file="indole_2207_c.csv",head=TRUE)
c5.1 <- read.csv(file="indole_1708_c.csv",head=TRUE)
}

#addding plot column
{d1.1$plot <- NA
for(i in 1:length(d1.1$FILES)){
	if(d1.1$FILES[i]==1){d1.1$plot[i]="1"}
}
d2.1$plot <- NA
for(i in 1:length(d2.1$FILES)){
	if(d2.1$FILES[i]==1){d2.1$plot[i]="2"}
}
d3.1$plot <- NA
for(i in 1:length(d3.1$FILES)){
	if(d3.1$FILES[i]==1){d3.1$plot[i]="3"}
}
d4.1$plot <- NA
for(i in 1:length(d4.1$FILES)){
	if(d4.1$FILES[i]==1){d4.1$plot[i]="4"}
}
d5.1$plot <- NA
for(i in 1:length(d5.1$FILES)){
	if(d5.1$FILES[i]==1){d5.1$plot[i]="5"}
}
c1.1$plot <- NA
for(i in 1:length(c1.1$FILES)){
	if(c1.1$FILES[i]==1){c1.1$plot[i]="6"}
}
c2.1$plot <- NA
for(i in 1:length(c2.1$FILES)){
	if(c2.1$FILES[i]==1){c2.1$plot[i]="7"}
}
c3.1$plot <- NA
for(i in 1:length(c3.1$FILES)){
	if(c3.1$FILES[i]==1){c3.1$plot[i]="8"}
}
c4.1$plot <- NA
for(i in 1:length(c4.1$FILES)){
	if(c4.1$FILES[i]==1){c4.1$plot[i]="9"}
}
c5.1$plot <- NA
for(i in 1:length(c5.1$FILES)){
	if(c5.1$FILES[i]==1){c5.1$plot[i]="10"}
}}


#combine datasets
indole.d <- rbind(d1.1, d2.1, d3.1, d4.1, d5.1)
indole.c <- rbind(c1.1, c2.1, c3.1, c4.1, c5.1)


indole.d<-indole.d[indole.d$AUTO.ID. != "Noise", ]  
indole.c<-indole.c[indole.c$AUTO.ID. != "Noise", ]  

library(dplyr)

indole.d<-select(indole.d, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, plot)
indole.c<-select(indole.c, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, plot)

#{	indole.d$AUTO.ID.<-as.factor(indole.d$AUTO.ID.)
#	indole.d$DATE.12<-as.Date(indole.d$DATE.12,"%Y%m%d")
#	indole.d$HOUR<-as.numeric(indole.d$HOUR)
#	indole.d$DATE<-as.Date(indole.d$DATE,"%y%m%d")
#	indole.d$HOUR.12<-as.numeric(indole.d$HOUR.12)
#}
#
{#	indole.c$AUTO.ID.<-as.factor(indole.c$AUTO.ID.)
#	indole.c$DATE.12<-as.Date(indole.c$DATE.12,"%m/%d/%y")
#	indole.c$HOUR<-as.numeric(indole.c$HOUR)
#	indole.c$DATE<-as.Date(indole.c$DATE,"%m/%d/%y")
#	indole.c$HOUR.12<-as.numeric(indole.c$HOUR.12)
}

#creating treatment columns
indole.d$treatment <- NA
for(i in 1:length(indole.d$FILES)){
	if(indole.d$FILES[i]==1){indole.d$treatment[i]="Dispenser"}
}

indole.c$treatment <- NA
for(i in 1:length(indole.c$FILES)){
	if(indole.c$FILES[i]==1){indole.c$treatment[i]="Control"}
}

#combine them
all.indole <- rbind(indole.c, indole.d)

all.in.ag<-aggregate(FILES ~ DATE.12 + treatment + AUTO.ID. + plot, data=all.indole, FUN=sum)
all.in.ag$treatment<-as.factor(all.in.ag$treatment)

##Analyses----
library(ggplot2)
library(viridis)

indole.plot1<-ggplot(data=all.in.ag, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))
indole.plot1

indole.plot1+facet_wrap(~AUTO.ID.)

library(lme4)
indole.mod1<-lmer(FILES~treatment*AUTO.ID.+1|DATE.12, dat = all.in.ag)
summary(indole.mod1)
shapiro.test(resid(indole.mod1))

indole.mod2<-lm(FILES~treatment*AUTO.ID., dat=all.in.ag)
summary(indole.mod2)

##BIG BROWNS ONLY ----
bigbrown.indole <- all.in.ag[which(all.in.ag$AUTO.ID.== 'EPTFUS'),]

bigbrown.indole$FILES<-as.numeric(bigbrown.indole$FILES)
bigbrown.indole$treatment<-as.character(bigbrown.indole$treatment)
bigbrown.indole$plot<-as.factor(bigbrown.indole$plot)

#graph
ggplot(data=bigbrown.indole, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)",
		 title = "Big brown bats (EPTFUS)-Round 1")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")

#barplot
library(Rmisc)
bb_agse <- summarySE(bigbrown.indole, measurevar="FILES", groupvars=c("treatment"))
ggplot(bb_agse,aes(x=treatment,y=FILES, fill=treatment))+
	geom_bar(stat = "summary")+
	theme_classic()+
	labs(x=" ",y="Relative activity")+
	theme(text = element_text(size = 20), legend.position = "none")+
	geom_errorbar(aes(ymin=FILES-se, ymax=FILES+se), width=.1)+
	scale_x_discrete(limits=c("Dispenser", "Control"))+
	scale_fill_manual(values = c("#fde725ff", "#1f968bff"))


indole.mod.bb<-lmer(FILES~treatment + 1|plot, data = bigbrown.indole)
#warnings
summary(indole.mod.bb)
Anova(indole.mod.bb)#can't get this to run
shapiro.test(resid(indole.mod.bb))#not normal

##from Kate's class...
#we don't get a p-value, only a t value
#this is because understanding the df with random effects is hard
#my recommendation: estimate the random effects conservative (e.g. the largest number of df they could potentially take up)
#here, we have 18 subjects, plus the fixed effect Days (which is continous)
#therefore, our df = Numbers of obs (62) - (all the levels of fixed effects) - (all the levels of random effects)
#62 - 10 - 2
62 - 10 - 2
#50
t.value = 2.352
p.value = 2*pt(t.value, df = 50, lower=FALSE)
#draw from the t distribution to get the probability (p-value) at your calculated t-value
p.value
#a good rule of thumb is that if you have a lot of data, you want a t-value > 2
#p=0.0227

##Round 2 ----

#read in all data
{ c1.2 <- read.csv(file="indole_4672_c2.csv",head=TRUE)
c2.2 <- read.csv(file="indole_4614_c2.csv",head=TRUE)
c3.2 <- read.csv(file="indole_4604_c2.csv",head=TRUE)
c4.2 <- read.csv(file="indole_1676_c2.csv",head=TRUE)
c5.2 <- read.csv(file="indole_0505_c2.csv",head=TRUE)
d1.2 <- read.csv(file="indole_4655_d2.csv",head=TRUE)
d2.2 <- read.csv(file="indole_2198_d2.csv",head=TRUE)
d3.2 <- read.csv(file="indole_4608_d2.csv",head=TRUE)
d4.2 <- read.csv(file="indole_2207_d2.csv",head=TRUE)
d5.2 <- read.csv(file="indole_1708_d2.csv",head=TRUE)
}

#addding plot column
{d1.2$plot <- NA
	for(i in 1:length(d1.2$FILES)){
		if(d1.2$FILES[i]==1){d1.2$plot[i]="1"}
	}
	d2.2$plot <- NA
	for(i in 1:length(d2.2$FILES)){
		if(d2.2$FILES[i]==1){d2.2$plot[i]="2"}
	}
	d3.2$plot <- NA
	for(i in 1:length(d3.2$FILES)){
		if(d3.2$FILES[i]==1){d3.2$plot[i]="3"}
	}
	d4.2$plot <- NA
	for(i in 1:length(d4.2$FILES)){
		if(d4.2$FILES[i]==1){d4.2$plot[i]="4"}
	}
	d5.2$plot <- NA
	for(i in 1:length(d5.2$FILES)){
		if(d5.2$FILES[i]==1){d5.2$plot[i]="5"}
	}
	c1.2$plot <- NA
	for(i in 1:length(c1.2$FILES)){
		if(c1.2$FILES[i]==1){c1.2$plot[i]="6"}
	}
	c2.2$plot <- NA
	for(i in 1:length(c2.2$FILES)){
		if(c2.2$FILES[i]==1){c2.2$plot[i]="7"}
	}
	c3.2$plot <- NA
	for(i in 1:length(c3.2$FILES)){
		if(c3.2$FILES[i]==1){c3.2$plot[i]="8"}
	}
	c4.2$plot <- NA
	for(i in 1:length(c4.2$FILES)){
		if(c4.2$FILES[i]==1){c4.2$plot[i]="9"}
	}
	c5.2$plot <- NA
	for(i in 1:length(c5.2$FILES)){
		if(c5.2$FILES[i]==1){c5.2$plot[i]="10"}
	}}


#combine datasets
indole.d2 <- rbind(d1.2, d2.2, d3.2, d4.2, d5.2)
indole.c2 <- rbind(c1.2, c2.2, c3.2, c4.2, c5.2)


indole.d2<-indole.d2[indole.d2$AUTO.ID. != "Noise", ]  
indole.c2<-indole.c2[indole.c2$AUTO.ID. != "Noise", ]  

library(dplyr)

indole.d2<-select(indole.d2, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, plot)
indole.c2<-select(indole.c2, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, plot)

{	indole.d2$AUTO.ID.<-as.factor(indole.d2$AUTO.ID.)
	indole.d2$DATE.12<-as.Date(indole.d2$DATE.12,"%Y%m%d")
	indole.d2$HOUR<-as.numeric(indole.d2$HOUR)
	indole.d2$DATE<-as.Date(indole.d2$DATE,"%y%m%d")
	indole.d2$HOUR.12<-as.numeric(indole.d2$HOUR.12)
}

{	indole.c2$AUTO.ID.<-as.factor(indole.c2$AUTO.ID.)
	indole.c2$DATE.12<-as.Date(indole.c2$DATE.12,"%m/%d/%y")
	indole.c2$HOUR<-as.numeric(indole.c2$HOUR)
	indole.c2$DATE<-as.Date(indole.c2$DATE,"%m/%d/%y")
	indole.c2$HOUR.12<-as.numeric(indole.c2$HOUR.12)
}

#creating treatment columns
indole.d2$treatment <- NA
for(i in 1:length(indole.d2$FILES)){
	if(indole.d2$FILES[i]==1){indole.d2$treatment[i]="Dispenser"}
}

indole.c2$treatment <- NA
for(i in 1:length(indole.c2$FILES)){
	if(indole.c2$FILES[i]==1){indole.c2$treatment[i]="Control"}
}

#combine them
all.indole2 <- rbind(indole.c2, indole.d2)

all.in.ag2<-aggregate(FILES ~ DATE.12 + treatment + AUTO.ID. + plot, data=all.indole2, FUN=sum)
all.in.ag2$treatment<-as.factor(all.in.ag2$treatment)

##Analyses----
library(ggplot2)
library(viridis)

indole.plot1<-ggplot(data=all.in.ag2, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))
indole.plot1
indole.plot1+facet_wrap(~AUTO.ID.)

indole.all<-rbind (all.in.ag2, all.in.ag)

indole.plot10<-ggplot(data=indole.all, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="All indole trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))
indole.plot10
indole.plot10+facet_wrap(~AUTO.ID.)

#simple graph
ggplot(data=indole.all, aes(x=treatment, y=FILES))+ 
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="All indole trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

##BIG BROWNS ONLY ----
bigbrown.indole2 <- all.in.ag2[which(all.in.ag2$AUTO.ID.== 'EPTFUS'),]

bigbrown.indole2$FILES<-as.numeric(bigbrown.indole2$FILES)
bigbrown.indole2$treatment<-as.character(bigbrown.indole2$treatment)
bigbrown.indole2$plot<-as.factor(bigbrown.indole2$plot)

#graph
ggplot(data=bigbrown.indole2, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)",
		 title = "Big brown bats (EPTFUS)-Round 2")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")

bb.indole.all<-rbind (bigbrown.indole, bigbrown.indole2)

#graph
ggplot(data=bb.indole.all, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)",
		 title = "Big brown bats (EPTFUS)-all trials")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")

#simple graph
ggplot(data=bb.indole.all, aes(x=treatment, y=FILES))+ 
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)",
		 title = "Big brown bats (EPTFUS)-all trials")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")
