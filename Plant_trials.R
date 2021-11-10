#Load libraries
library(ggplot2)
library(lme4)
library(viridis)
library(car)
library(emmeans)
library(multcomp)
library(dplyr)
library(plyr)
library(tidyr)

# Data import and cleaning ------------------------------------------------

#SUMMARY DATA----
plant <- read.csv(file="plant_sum.csv",head=TRUE)

##Gathering data — compounds from col to rows
plant1<-plant %>% gather(sp, activity, EPTFUS:NOID)
plant1 <- plant1[order(plant1$activity),]
#only 12 zeros
hist(plant1$activity)
#but very right skewed


mod10<-glmer.nb(activity~treatment*sp+(1|trial), dat = plant1)
Anova(mod10)
#treatment chisq=9.95 p=0.0016, spp chisq=578.73 p<0.0001, interaxn chisq=2.86 p=0.97


###BIG DATA----
#read in all data
{ w1 <- read.csv(file="id_w1.csv",head=TRUE)
w2 <- read.csv(file="id_w2.csv",head=TRUE)
w3 <- read.csv(file="id_w3.csv",head=TRUE)
w4 <- read.csv(file="id_w4.csv",head=TRUE)
e1 <- read.csv(file="id_e1.csv",head=TRUE)
e2 <- read.csv(file="id_e2.csv",head=TRUE)
e3 <- read.csv(file="id_e3.csv",head=TRUE)
e4 <- read.csv(file="id_e4.csv",head=TRUE) }

#combine datasets
all.dat.w <- rbind(w1, w2, w3, w4)
all.dat.e <- rbind(e1, e2, e3, e4)


all.dat.w<-all.dat.w[all.dat.w$AUTO.ID. != "Noise", ]  
all.dat.e<-all.dat.e[all.dat.e$AUTO.ID. != "Noise", ]  

all.bat.w<-select(all.dat.w, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, Fmin)
all.bat.e<-select(all.dat.e, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, Fmin)

{	all.bat.w$AUTO.ID.<-as.factor(all.bat.w$AUTO.ID.)
	all.bat.w$DATE.12<-as.Date(all.bat.w$DATE.12,"%m/%d/%y")
	all.bat.w$HOUR<-as.numeric(all.bat.w$HOUR)
	all.bat.w$DATE<-as.Date(all.bat.w$DATE,"%m/%d/%y")
	all.bat.w$HOUR.12<-as.numeric(all.bat.w$HOUR.12)
}

{	all.bat.e$AUTO.ID.<-as.factor(all.bat.e$AUTO.ID.)
	all.bat.e$DATE.12<-as.Date(all.bat.e$DATE.12,"%m/%d/%y")
	all.bat.e$HOUR<-as.numeric(all.bat.e$HOUR)
	all.bat.e$DATE<-as.Date(all.bat.e$DATE,"%m/%d/%y")
	all.bat.e$HOUR.12<-as.numeric(all.bat.e$HOUR.12)
}

##EASTSIDE
###creating treatment column
all.bat.e$treatment <- NA
for(i in 1:length(all.bat.e$DATE.12)){
	if(all.bat.e$DATE.12[i]=="2021-07-17"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-18"){all.bat.e$treatment[i]="D"}
	if(all.bat.e$DATE.12[i]=="2021-07-19"){all.bat.e$treatment[i]="D"}
	if(all.bat.e$DATE.12[i]=="2021-07-20"){all.bat.e$treatment[i]="U"}
	if(all.bat.e$DATE.12[i]=="2021-07-21"){all.bat.e$treatment[i]="U"}
	if(all.bat.e$DATE.12[i]=="2021-07-22"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-23"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-24"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-25"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-26"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-27"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-28"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-29"){all.bat.e$treatment[i]="R"}
	if(all.bat.e$DATE.12[i]=="2021-07-30"){all.bat.e$treatment[i]="U"}
	if(all.bat.e$DATE.12[i]=="2021-07-31"){all.bat.e$treatment[i]="U"}
	if(all.bat.e$DATE.12[i]=="2021-08-01"){all.bat.e$treatment[i]="D"}
	if(all.bat.e$DATE.12[i]=="2021-08-02"){all.bat.e$treatment[i]="D"}
	if(all.bat.e$DATE.12[i]=="2021-08-03"){all.bat.e$treatment[i]="D"}
	if(all.bat.e$DATE.12[i]=="2021-08-04"){all.bat.e$treatment[i]="D"}
	
}

#creating column trial
all.bat.e$trial <- NA
for(i in 1:length(all.bat.e$DATE.12)){
	if(all.bat.e$DATE.12[i]=="2021-07-17"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-18"){all.bat.e$trial[i]="1"}
	if(all.bat.e$DATE.12[i]=="2021-07-19"){all.bat.e$trial[i]="1"}
	if(all.bat.e$DATE.12[i]=="2021-07-20"){all.bat.e$trial[i]="2"}
	if(all.bat.e$DATE.12[i]=="2021-07-21"){all.bat.e$trial[i]="2"}
	if(all.bat.e$DATE.12[i]=="2021-07-22"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-23"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-24"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-25"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-26"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-27"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-28"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-29"){all.bat.e$trial[i]="N/A"}
	if(all.bat.e$DATE.12[i]=="2021-07-30"){all.bat.e$trial[i]="3"}
	if(all.bat.e$DATE.12[i]=="2021-07-31"){all.bat.e$trial[i]="3"}
	if(all.bat.e$DATE.12[i]=="2021-08-01"){all.bat.e$trial[i]="4"}
	if(all.bat.e$DATE.12[i]=="2021-08-02"){all.bat.e$trial[i]="4"}
	if(all.bat.e$DATE.12[i]=="2021-08-03"){all.bat.e$trial[i]="5"}
	if(all.bat.e$DATE.12[i]=="2021-08-04"){all.bat.e$trial[i]="5"}
	
}

#creating side column
all.bat.e$side <- NA
for(i in 1:length(all.bat.e$FILES)){
	if(all.bat.e$FILES[i]==1){all.bat.e$side[i]="E"}
}

all.bat.e <- all.bat.e[order(all.bat.e$trial),]

#WESTSIDE
##creating treatment column
all.bat.w$treatment <- NA
for(i in 1:length(all.bat.w$DATE.12)){
	if(all.bat.w$DATE.12[i]=="2021-07-17"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-18"){all.bat.w$treatment[i]="U"}
	if(all.bat.w$DATE.12[i]=="2021-07-19"){all.bat.w$treatment[i]="U"}
	if(all.bat.w$DATE.12[i]=="2021-07-20"){all.bat.w$treatment[i]="D"}
	if(all.bat.w$DATE.12[i]=="2021-07-21"){all.bat.w$treatment[i]="D"}
	if(all.bat.w$DATE.12[i]=="2021-07-22"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-23"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-24"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-25"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-26"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-27"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-28"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-29"){all.bat.w$treatment[i]="R"}
	if(all.bat.w$DATE.12[i]=="2021-07-30"){all.bat.w$treatment[i]="D"}
	if(all.bat.w$DATE.12[i]=="2021-07-31"){all.bat.w$treatment[i]="D"}
	if(all.bat.w$DATE.12[i]=="2021-08-01"){all.bat.w$treatment[i]="U"}
	if(all.bat.w$DATE.12[i]=="2021-08-02"){all.bat.w$treatment[i]="U"}
	if(all.bat.w$DATE.12[i]=="2021-08-03"){all.bat.w$treatment[i]="U"}
	if(all.bat.w$DATE.12[i]=="2021-08-04"){all.bat.w$treatment[i]="U"}
	
}

##creating trial column
all.bat.w$trial <- NA
for(i in 1:length(all.bat.w$DATE.12)){
	if(all.bat.w$DATE.12[i]=="2021-07-17"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-18"){all.bat.w$trial[i]="1"}
	if(all.bat.w$DATE.12[i]=="2021-07-19"){all.bat.w$trial[i]="1"}
	if(all.bat.w$DATE.12[i]=="2021-07-20"){all.bat.w$trial[i]="2"}
	if(all.bat.w$DATE.12[i]=="2021-07-21"){all.bat.w$trial[i]="2"}
	if(all.bat.w$DATE.12[i]=="2021-07-22"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-23"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-24"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-25"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-26"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-27"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-28"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-29"){all.bat.w$trial[i]="N/A"}
	if(all.bat.w$DATE.12[i]=="2021-07-30"){all.bat.w$trial[i]="3"}
	if(all.bat.w$DATE.12[i]=="2021-07-31"){all.bat.w$trial[i]="3"}
	if(all.bat.w$DATE.12[i]=="2021-08-01"){all.bat.w$trial[i]="4"}
	if(all.bat.w$DATE.12[i]=="2021-08-02"){all.bat.w$trial[i]="4"}
	if(all.bat.w$DATE.12[i]=="2021-08-03"){all.bat.w$trial[i]="5"}
	if(all.bat.w$DATE.12[i]=="2021-08-04"){all.bat.w$trial[i]="5"}
	
}

#creating side column
all.bat.w$side <- NA
for(i in 1:length(all.bat.w$FILES)){
	if(all.bat.w$FILES[i]==1){all.bat.w$side[i]="W"}
}

#combine the two datasets, east and west sides
#all.bat=all data
all.bat <- rbind(all.bat.e, all.bat.w)
all.bat$treatment<-as.factor(all.bat$treatment)
all.bat$trial<-as.factor(all.bat$trial)
all.bat$FILES<-as.numeric(all.bat$FILES)

#create dataset with only experimental trials
#all.bat.exp=dataset with only nights where experiment was running
all.bat.exp <- all.bat[all.bat$trial != "N/A", ] 

##ANALYSES----

#All species----
#aggregating data by species
all.bat.exp.agg<-aggregate(FILES ~ treatment + side + trial + AUTO.ID., data=all.bat.exp, FUN=sum)
all.bat.exp.agg$treatment<-as.factor(all.bat.exp.agg$treatment)
all.bat.exp.agg$AUTO.ID.<-as.factor(all.bat.exp.agg$AUTO.ID.)
all.bat.exp.agg <- all.bat.exp.agg[order(all.bat.exp.agg$trial),]

mod1<-glmer.nb(FILES~treatment*AUTO.ID.+(1|trial), dat = all.bat.exp.agg)
summary(mod1)
Anova(mod1)
#treatment chisq=6.8884 p=0.009, spp chisq=456.1526 p<0.0001, interaxn chisq=2.00 p=0.99

#contrasts
f1<-emmeans(mod1,pairwise~AUTO.ID., type="response")
cld(f1$emmeans,  Letters ='abcdefghijklmn')

f2<-emmeans(mod1,pairwise~treatment, type="response")
cld(f2$emmeans,  Letters ='abcde')
#high activity of unidentified bat species. The second most active species were big browns and 
#free-tailed bats. The least active species was eastern reds.

treatment.tab <- ddply(all.bat.exp.agg, c("treatment"), summarise,
				  N    = length(FILES),
				  mean = mean(FILES),
				  sd   = sd(FILES),
				  se   = sd / sqrt(N))
treatment.tab
#(undamaged-damaged)/damaged
(83.52000-63.83673)/63.83673
#0.3083377
#Undamaged plots had 30.9% more activity compared to damaged plots


##GROUPING SPP WITH SIMILAR CALLS----
{all.bat.exp.agg3<-all.bat.exp.agg
all.bat.exp.agg3$AUTO.ID.=as.character(all.bat.exp.agg3$AUTO.ID.)
all.bat.exp.agg3$AUTO.ID.[all.bat.exp.agg3$AUTO.ID.=="LASBOR"]="LABO/LASE"
all.bat.exp.agg3$AUTO.ID.[all.bat.exp.agg3$AUTO.ID.=="LASSEM"]="LABO/LASE"
all.bat.exp.agg3$AUTO.ID.[all.bat.exp.agg3$AUTO.ID.=="EPTFUS"]="EPFU/LANO"
all.bat.exp.agg3$AUTO.ID.[all.bat.exp.agg3$AUTO.ID.=="LASNOC"]="EPFU/LANO"
}

group<-aggregate(FILES ~ treatment + AUTO.ID. + trial, dat=all.bat.exp.agg3, FUN=sum)

mod3<-glmer.nb(FILES~treatment*AUTO.ID.+(1|trial), dat = group)
Anova(mod3)
#treatment chisq=7.0682 p=0.007846, spp chisq=407.7986 p<0.0001, interaxn chisq=1.9812 p=0.960871

#contrasts
f30<-emmeans(mod3,pairwise~AUTO.ID., type="response")
cld(f30$emmeans,  Letters ='abcde')

f31<-emmeans(mod3,pairwise~treatment, type="response")
cld(f31$emmeans,  Letters ='abcde')

group.tab <- ddply(group, c("treatment"), summarise,
					N    = length(FILES),
					mean = mean(FILES),
					sd   = sd(FILES),
					se   = sd / sqrt(N))
group.tab
#(undamaged-damaged)/damaged
(104.4-78.2)/78.2
#0.3350384
#Undamaged plots averaged 33.5% more activity compared to damaged plots

group.tab2 <- ddply(group, c("AUTO.ID."), summarise,
					 N    = length(FILES),
					 mean = mean(FILES),
					 sd   = sd(FILES),
					 se   = sd / sqrt(N))
group.tab2


##GROUPING BY PHONIC GROUPS----
{all.bat.exp.agg2<-all.bat.exp.agg
all.bat.exp.agg2$AUTO.ID.=as.character(all.bat.exp.agg2$AUTO.ID.)
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="MYOLUC"]="High"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="NYCHUM"]="Mid"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="PERSUB"]="Mid"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="LASBOR"]="Mid"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="LASSEM"]="Mid"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="EPTFUS"]="Low"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="LASNOC"]="Low"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="LASCIN"]="Low"
all.bat.exp.agg2$AUTO.ID.[all.bat.exp.agg2$AUTO.ID.=="TADBRA"]="Low"
}

phonic<-aggregate(FILES ~ treatment + AUTO.ID. + trial, dat=all.bat.exp.agg2, FUN=sum)

mod2<-glmer.nb(FILES~treatment*AUTO.ID.+(1|trial), dat = phonic)
Anova(mod2)
#treatment chisq=5.0552 p=0.02455, spp chisq=275.5260 p<0.0001, interaxn chisq=1.3061 p=0.72769

#contrasts
f10<-emmeans(mod2,pairwise~AUTO.ID., type="response")
cld(f10$emmeans,  Letters ='abcde')

f20<-emmeans(mod2,pairwise~treatment, type="response")
cld(f2$emmeans,  Letters ='abcde')


phonic.tab <- ddply(phonic, c("treatment"), summarise,
					N    = length(FILES),
					mean = mean(FILES),
					sd   = sd(FILES),
					se   = sd / sqrt(N))
phonic.tab
#(undamaged-damaged)/damaged
(208.8-156.4)/156.4
#0.3350384
#Undamaged plots averaged 33.5% more activity compared to damaged plots

phonic.tab2 <- ddply(phonic, c("AUTO.ID."), summarise,
					 N    = length(FILES),
					 mean = mean(FILES),
					 sd   = sd(FILES),
					 se   = sd / sqrt(N))
phonic.tab2
#(undamaged-damaged)/damaged
(208.8-156.4)/156.4
#0.3350384
#Undamaged plots averaged 33.5% more activity compared to damaged plots

##GRAPHS----
#ALL SPECIES GRAPHS----
#interactive graph, all species
ggplot(data=all.bat.exp.agg, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))+
	stat_summary(geom = 'text', label = c("a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,800))

#simple graph, all species
ggplot(data=all.bat.exp.agg, aes(x=treatment, y=FILES))+ 
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))

#interactive graph, all species, separated by species
ggplot(data=all.bat.exp.agg, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title = "Plant trials")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	facet_wrap(~AUTO.ID.)

#interactive graph, all species, separated by trial 
ggplot(data=all.bat.exp.agg, aes(x=trial, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title = "Plant trials")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	facet_wrap(~treatment)

#species graph, all species
ggplot(data=all.bat.exp.agg, aes(x=AUTO.ID., y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), axis.text.x = element_text(angle=20, hjust=0.9, size=12))+
	stat_summary(geom = 'text', label = c("cd","a","ab","b", "c", "ab", "d","b", "ab","cd"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,800))

#TREATMENT graph, all species
ggplot(data=all.bat.exp.agg, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))+
	stat_summary(geom = 'text', label = c("a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,800))


#GROUPED SPECIES GRAPHS----
#INTERACTIVE GRAPH
ggplot(data=group, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Species"))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))+
	stat_summary(geom = 'text', label = c("a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	ylim(NA,800)

#TREATMENT GRAPH
ggplot(data=group, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))+
	stat_summary(geom = 'text', label = c("a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	ylim(NA,800)

#GROUP GRAPH
ggplot(data=group, aes(x=AUTO.ID., y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), axis.text.x = element_text(angle=20, hjust=0.9, size=12))+
	stat_summary(geom = 'text', label = c("bc","b","a","a", "c", "a", "a","bc"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,800))

#simple TREAT graph
ggplot(data=group, aes(x=treatment, y=FILES))+ 
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))


##PHONIC GROUP GRAPHS----
#INTERACTIVE GRAPH
ggplot(data=phonic, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Phonic group"))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))+
	stat_summary(geom = 'text', label = c("a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	ylim(NA,1000)

#TREATMENT GRAPH
ggplot(data=phonic, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))+
	stat_summary(geom = 'text', label = c("a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	ylim(NA,1000)

#PHONIC GROUP GRAPH
ggplot(data=phonic, aes(x=AUTO.ID., y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	stat_summary(geom = 'text', label = c("a","b","c","c"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,1000))+
	scale_x_discrete(limits=c("Low", "Mid", "High", "NoID"))

#simple TREAT graph
ggplot(data=phonic, aes(x=treatment, y=FILES))+ 
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))

##BIG BROWNS ONLY ----
bigbrown <- all.bat.exp.agg[which(all.bat.exp.agg$AUTO.ID.== 'EPTFUS'),]

mod.bb<-glmer.nb(FILES~treatment + (1|trial), data = bigbrown)
summary(mod.bb)

ggplot(data=bigbrown, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)",
		 title = "Big browns, plant trials")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")

#barplot
library(Rmisc)
bat_agse <- summarySE(bigbrown, measurevar="FILES", groupvars=c("treatment"))
ggplot(bat_agse,aes(x=treatment,y=FILES, fill=treatment))+
	geom_bar(stat = "summary")+
	theme_classic()+
	labs(x=" ",y="Relative activity")+
	theme(text = element_text(size = 20), legend.position = "none")+
	geom_errorbar(aes(ymin=FILES-se, ymax=FILES+se), width=.1)+
	scale_fill_manual(values = c("#1f968bff","#fde725ff"))
