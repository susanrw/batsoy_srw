#Load libraries
library(ggplot2)
library(lme4)
library(viridis)
library(car)
library(emmeans)
library(multcomp)
library(dplyr)

# Data import and cleaning ------------------------------------------------

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
all.dat.e <- rbind(e1, e3, e3, e4)


all.dat.w<-all.dat.w[all.dat.w$AUTO.ID. != "Noise", ]  
all.dat.e<-all.dat.e[all.dat.e$AUTO.ID. != "Noise", ]  



all.bat.w<-select(all.dat.w, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES)
all.bat.e<-select(all.dat.e, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES)

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

##ANALYSES----
#combine the two datasets, east and west sides
#all.bat=all data
all.bat <- rbind(all.bat.e, all.bat.w)
all.bat$treatment<-as.factor(all.bat$treatment)
all.bat$trial<-as.factor(all.bat$trial)
all.bat$FILES<-as.numeric(all.bat$FILES)

#create dataset with only experimental trials
#all.bat.exp=dataset with only nights where experiment was running
all.bat.exp <- all.bat[all.bat$trial != "N/A", ] 

#mod1<-glmer(FILES~treatment*AUTO.ID.+1|trial, dat = all.bat.exp)
#summary(mod1)
#shapiro.test(resid(mod1))
#printCoefmat(coef(summary(mod1)),digits=2)

ggplot(data=all.bat.exp, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

all.bat.exp.agg<-aggregate(FILES ~ DATE.12 + treatment + side + trial + AUTO.ID., data=all.bat.exp, FUN=sum)
all.bat.exp.agg$treatment<-as.factor(all.bat.exp.agg$treatment)

ggplot(data=all.bat.exp.agg, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))

#simple graph
ggplot(data=all.bat.exp.agg, aes(x=treatment, y=FILES))+ 
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Plant trials")+
	theme(text = element_text(size=16))+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))

mod2<-glmer.nb(FILES~treatment*AUTO.ID.+1|trial, dat = all.bat.exp.agg)
summary(mod2)
Anova(mod2)


all.bat.exp.agg$AUTO.ID.<-as.factor(all.bat.exp.agg$AUTO.ID.)
mod3<-glmer.nb(FILES~treatment+AUTO.ID.+(1|trial), dat = all.bat.exp.agg)
summary(mod3)
shapiro.test(resid(mod3))#p-value = 0.0002487
Anova(mod3)

#contrasts
f1<-emmeans(mod3,pairwise~AUTO.ID., type="response")
cld(f1$emmeans,  Letters ='abcde')

f2<-emmeans(mod3,pairwise~treatment, type="response")
cld(f2$emmeans,  Letters ='abcde')

mod4<-glmer.nb(FILES ~ treatment + (1|trial), dat = all.bat.exp.agg)
summary(mod4)
Anova(mod4)
shapiro.test(resid(mod4))

summary(glht(mod3, linfct=mcp(AUTO.ID.="Tukey")))

mod5<-glm(FILES~treatment+AUTO.ID.+trial, dat=all.bat.exp.agg)
summary(mod5)
summary(glht(mod5, linfct=mcp(treatment="Tukey")))

#create cld
library(emmeans)
library(multcomp)
#with interactive model, treatment
f1<-emmeans(mod5,pairwise~AUTO.ID., type="response")
cld(f1$emmeans,  Letters ='abcde')

f2<-emmeans(mod5,pairwise~treatment, type="response")
cld(f2$emmeans,  Letters ='abcde')

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
	
##BIG BROWNS ONLY ----
bigbrown <- all.bat.exp.agg[which(all.bat.exp.agg$AUTO.ID.== 'EPTFUS'),]

mod.bb<-lmer(FILES~treatment + 1|trial, data = bigbrown)
summary(mod.bb)

mod.bb1<-lm(FILES~treatment, data = bigbrown)
summary(mod.bb1)

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


##EASTERN REDS ONLY ----
reds <- all.bat.exp.agg[which(all.bat.exp.agg$AUTO.ID.== 'LASBOR'),]

mod.red<-lm(FILES~treatment, data = reds)
summary(mod.red)

ggplot(data=reds, aes(x=treatment, y=FILES))+
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)",
		 title = "Eastern red bats (LASBOR)")+
	theme(text = element_text(size=15))+
	scale_color_viridis(discrete = T, option = "D")

##OTHER STUFF----
bat1 <- read.csv(file="Trial 1.csv",head=TRUE)

library(lme4)

bat1<-bat1[1:899,]

bat1 <- bat1[order(bat1$trial),]

{
bat1$AUTO.ID.<-as.factor(bat1$AUTO.ID.)
bat1$DATE.12<-as.factor(bat1$DATE.12)
bat1$Treatment<-as.factor(bat1$Treatment)
bat1$Side<-as.factor(bat1$Side)
bat1$HOUR<-as.numeric(bat1$HOUR)
bat1$Pass<-as.numeric(bat1$Pass) }

bat2<-aggregate(Pass ~ AUTO.ID. + DATE.12 + Treatment + Side + HOUR, data=bat1, FUN=sum)

bat2$Treatment <- factor(bat2$Treatment, levels=c("Reference", "Damaged", "Undamaged"))

mod1<-lmer(Pass~Treatment+AUTO.ID.+1|Side, dat = bat2)
summary(mod1)
shapiro.test(resid(mod1))

hist(bat2$Pass)
shapiro.test(bat2$Pass)

#squareroot transformation
bat2$activity_sqrt<-sqrt(bat2$Pass)
shapiro.test(bat2$activity_sqrt)#didn't help
hist(bat2$activity_sqrt)

#log transformation
bat2$activity_log<-log(bat2$Pass)
shapiro.test(bat2$activity_log)#didn't help
hist(bat2$activity_log)

#cuberoot transformation
bat2$activity_cube<-(bat2$Pass)^(1/3)
shapiro.test(bat2$activity_cube)#didn't help
hist(bat2$activity_cube)

mod2<-glm(Pass~Treatment+AUTO.ID.+HOUR+DATE.12+Side, dat = bat2)
summary(mod2)
shapiro.test(mod2$residuals)

mod3<-glm(Pass~Treatment+AUTO.ID., dat=bat2)
summary(mod3)
shapiro.test(mod3$residuals)

library(multcomp)
summary(glht(mod3, linfct=mcp(Treatment="Tukey")))
summary(glht(mod3, linfct=mcp(AUTO.ID.="Tukey")))
#sig more big browns that LANO, LASE, MYLU, NYHU

library(ggplot2)
library(viridis)
ggplot(data=bat2, aes(x=Treatment, y=Pass))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

#removing reference treatment
bat2 <- bat2[order(bat2$Treatment),]
bat3<-bat2[-c(1:52),]

mod4<-glm(Pass~Treatment+AUTO.ID., dat=bat3)
summary(mod4)

library(multcomp)
summary(glht(mod4, linfct=mcp(AUTO.ID.="Tukey")))
#sig more big browns than LASE, MYLU, NYHU

ggplot(data=bat3, aes(x=Treatment, y=Pass))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

library(dplyr)
bat4<-aggregate(Pass ~ DATE.12 + Treatment + Side, data=bat3, FUN=sum)

bat4 %>%
	ggplot(aes(Treatment,Pass)) +
	geom_point(aes(fill=Treatment),size=3) +
	geom_line(aes(group = DATE.12))

bat5<-aggregate(Pass ~ Treatment + AUTO.ID., data=bat3, FUN=sum)

bat5 %>%
	ggplot(aes(Treatment,Pass,color=AUTO.ID.)) +
	geom_point(aes(fill=Treatment),size=3) +
	geom_line(aes(group = AUTO.ID.))

t123 <- read.csv(file="Trials1_5.csv",head=TRUE)

t123$Trial<-as.character(t123$Trial)

t123 %>%
	ggplot(aes(Treatment,Pass)) +
	geom_point(aes(color=Side),size=3) +
	geom_line(aes(group = DATE.12))+
	theme_classic()

t123 %>%
	ggplot(aes(Treatment,Pass)) +
	labs(x="",
		 y="Activity (no. detections)")+
	geom_boxplot()+
	geom_point(aes(color=Trial),size=3)+
	theme_classic()+
	theme(text = element_text(size=18))
	

t123 %>%
	ggplot(aes(Treatment,Pass)) +
	geom_point(aes(color=Trial),size=3) +
	geom_line(aes(group = DATE.12))+
	theme_classic()+
	labs(x="",
		 y="Activity (no. detections)")+
	theme(text = element_text(size=18))

base <- read.csv(file="baseline_activity.csv",head=TRUE)

base$date<-as.Date.character(base$DATE.12,"%Y%m%d")
base$exp<-as.character(base$exp)

base %>%
	ggplot(aes(x=date, 
			   y=Pass,
			   color=Side
			   ))+
	geom_point(aes(shape=exp), size=3)+
	geom_line()+
	labs(x="",
		 y="Total activity (no. detections)",
		 color="Side",
		 shape="Experiment")+
	theme_classic()+
	theme(text = element_text(size=15), legend.title = element_text(size = 12), 
		  legend.text = element_text(size = 10), axis.text.x = element_text(angle=20, hjust=1))+
	scale_x_date(date_breaks = "3 days", date_labels = "%b %d")
	

base %>%
	ggplot(aes(x=date, 
			   y=Pass,
			   color=Side
	))+
	geom_point()+
	geom_line()+
	labs(x="",
		 y="Total activity (no. detections)",
		 color="Side")+
	theme_classic()+
	theme(text = element_text(size=15), legend.title = element_text(size = 12), 
		  legend.text = element_text(size = 10), axis.text.x = element_text(angle=20, hjust=1))+
	scale_x_date(date_breaks = "3 days", date_labels = "%b %d")	

##
