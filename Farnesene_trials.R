# Libraries ----
library(dplyr)
library(ggplot2)
library(viridis)

# Data import and cleaning ------------------------------------------------


#read in all data, round 1
{ df1.1 <- read.csv(file="farn_4672_d.csv",head=TRUE)
df2.1 <- read.csv(file="farn_4614_d.csv",head=TRUE)
df3.1 <- read.csv(file="farn_2198_d.csv",head=TRUE)
df4.1 <- read.csv(file="farn_4608_d.csv",head=TRUE)
df5.1 <- read.csv(file="farn_1708_d.csv",head=TRUE)
cf1.1 <- read.csv(file="farn_4655_c.csv",head=TRUE)
cf2.1 <- read.csv(file="farn_4604_c.csv",head=TRUE)
cf3.1 <- read.csv(file="farn_1676_c.csv",head=TRUE)
cf4.1 <- read.csv(file="farn_2207_c.csv",head=TRUE)
cf5.1 <- read.csv(file="farn_0505_c.csv",head=TRUE)
}

#round 2
{ cf1.2 <- read.csv(file="farn_4672_c.csv",head=TRUE)
	cf2.2 <- read.csv(file="farn_4614_c.csv",head=TRUE)
	cf3.2 <- read.csv(file="farn_2198_c.csv",head=TRUE)
	cf4.2 <- read.csv(file="farn_4608_c.csv",head=TRUE)
	cf5.2 <- read.csv(file="farn_1708_c.csv",head=TRUE)
	df1.2 <- read.csv(file="farn_4655_d.csv",head=TRUE)
	df2.2 <- read.csv(file="farn_4604_d.csv",head=TRUE)
	df3.2 <- read.csv(file="farn_1676_d.csv",head=TRUE)
	df4.2 <- read.csv(file="farn_2207_d.csv",head=TRUE)
	df5.2 <- read.csv(file="farn_0505_d.csv",head=TRUE)
}

#addding plot column
{df1.1$plot <- NA
	for(i in 1:length(df1.1$FILES)){
		if(df1.1$FILES[i]==1){df1.1$plot[i]="1"}
	}
	df2.1$plot <- NA
	for(i in 1:length(df2.1$FILES)){
		if(df2.1$FILES[i]==1){df2.1$plot[i]="2"}
	}
	df3.1$plot <- NA
	for(i in 1:length(df3.1$FILES)){
		if(df3.1$FILES[i]==1){df3.1$plot[i]="3"}
	}
	df4.1$plot <- NA
	for(i in 1:length(df4.1$FILES)){
		if(df4.1$FILES[i]==1){df4.1$plot[i]="4"}
	}
	df5.1$plot <- NA
	for(i in 1:length(df5.1$FILES)){
		if(df5.1$FILES[i]==1){df5.1$plot[i]="5"}
	}
	cf1.1$plot <- NA
	for(i in 1:length(cf1.1$FILES)){
		if(cf1.1$FILES[i]==1){cf1.1$plot[i]="6"}
	}
	cf2.1$plot <- NA
	for(i in 1:length(cf2.1$FILES)){
		if(cf2.1$FILES[i]==1){cf2.1$plot[i]="7"}
	}
	cf3.1$plot <- NA
	for(i in 1:length(cf3.1$FILES)){
		if(cf3.1$FILES[i]==1){cf3.1$plot[i]="8"}
	}
	cf4.1$plot <- NA
	for(i in 1:length(cf4.1$FILES)){
		if(cf4.1$FILES[i]==1){cf4.1$plot[i]="9"}
	}
	cf5.1$plot <- NA
	for(i in 1:length(cf5.1$FILES)){
		if(cf5.1$FILES[i]==1){cf5.1$plot[i]="10"}
	}}

{df1.2$plot <- NA
	for(i in 1:length(df1.2$FILES)){
		if(df1.2$FILES[i]==1){df1.2$plot[i]="1"}
	}
	df2.2$plot <- NA
	for(i in 1:length(df2.2$FILES)){
		if(df2.2$FILES[i]==1){df2.2$plot[i]="2"}
	}
	df3.2$plot <- NA
	for(i in 1:length(df3.2$FILES)){
		if(df3.2$FILES[i]==1){df3.2$plot[i]="3"}
	}
	df4.2$plot <- NA
	for(i in 1:length(df4.2$FILES)){
		if(df4.2$FILES[i]==1){df4.2$plot[i]="4"}
	}
	df5.2$plot <- NA
	for(i in 1:length(df5.2$FILES)){
		if(df5.2$FILES[i]==1){df5.2$plot[i]="5"}
	}
	cf1.2$plot <- NA
	for(i in 1:length(cf1.2$FILES)){
		if(cf1.2$FILES[i]==1){cf1.2$plot[i]="6"}
	}
	cf2.2$plot <- NA
	for(i in 1:length(cf2.2$FILES)){
		if(cf2.2$FILES[i]==1){cf2.2$plot[i]="7"}
	}
	cf3.2$plot <- NA
	for(i in 1:length(cf3.2$FILES)){
		if(cf3.2$FILES[i]==1){cf3.2$plot[i]="8"}
	}
	cf4.2$plot <- NA
	for(i in 1:length(cf4.2$FILES)){
		if(cf4.2$FILES[i]==1){cf4.2$plot[i]="9"}
	}
	cf5.2$plot <- NA
	for(i in 1:length(cf5.2$FILES)){
		if(cf5.2$FILES[i]==1){cf5.2$plot[i]="10"}
	}}


#combine datasets
farn.d <- rbind(df1.1, df2.1, df3.1, df4.1, df5.1)
farn.c <- rbind(cf1.1, cf2.1, cf3.1, cf4.1, cf5.1)

farn.d<-farn.d[farn.d$AUTO.ID. != "Noise", ]  
farn.c<-farn.c[farn.c$AUTO.ID. != "Noise", ]  

farn.d<-select(farn.d, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, plot)
farn.c<-select(farn.c, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, plot)

{	farn.d$AUTO.ID.<-as.factor(farn.d$AUTO.ID.)
	farn.d$DATE.12<-as.Date(farn.d$DATE.12,"%Y%m%d")
	farn.d$HOUR<-as.numeric(farn.d$HOUR)
	farn.d$DATE<-as.Date(farn.d$DATE,"%y%m%d")
	farn.d$HOUR.12<-as.numeric(farn.d$HOUR.12)
}

{	farn.c$AUTO.ID.<-as.factor(farn.c$AUTO.ID.)
	farn.c$DATE.12<-as.Date(farn.c$DATE.12,"%m/%d/%y")
	farn.c$HOUR<-as.numeric(farn.c$HOUR)
	farn.c$DATE<-as.Date(farn.c$DATE,"%m/%d/%y")
	farn.c$HOUR.12<-as.numeric(farn.c$HOUR.12)
}

#creating treatment columns
farn.d$treatment <- NA
for(i in 1:length(farn.d$FILES)){
	if(farn.d$FILES[i]==1){farn.d$treatment[i]="Dispenser"}
}

farn.c$treatment <- NA
for(i in 1:length(farn.c$FILES)){
	if(farn.c$FILES[i]==1){farn.c$treatment[i]="Control"}
}

#combine them
all.farn <- rbind(farn.d, farn.c)

all.farn.ag<-aggregate(FILES ~ DATE.12 + treatment + AUTO.ID. + plot, data=all.farn, FUN=sum)
all.farn.ag$treatment<-as.factor(all.farn.ag$treatment)

##Analyses----

farn.plot1<-ggplot(data=all.farn.ag, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title = "Farnesene trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))
farn.plot1
farn.plot1+facet_wrap(~AUTO.ID.)

#simple graph
ggplot(data=all.farn.ag, aes(x=treatment, y=FILES))+ 
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)", title="Farnesene trials")+
	theme(text = element_text(size=16), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

##Big browns----
bb.farn <- all.farn.ag[which(all.farn.ag$AUTO.ID.== 'EPTFUS'),]

bb.farn$FILES<-as.numeric(bb.farn$FILES)
bb.farn$treatment<-as.character(bb.farn$treatment)
bb.farn$plot<-as.factor(bb.farn$plot)

#graph
ggplot(data=bb.farn, aes(x=treatment, y=FILES))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity (no. nightly recordings)",
		 title = "Big brown bats, Farnesene")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")

#barplot
library(Rmisc)
bb_farn <- summarySE(bb.farn, measurevar="FILES", groupvars=c("treatment"))
ggplot(bb_farn,aes(x=treatment,y=FILES, fill=treatment))+
	geom_bar(stat = "summary")+
	theme_classic()+
	labs(x=" ",y="Relative activity")+
	theme(text = element_text(size = 20), legend.position = "none")+
	geom_errorbar(aes(ymin=FILES-se, ymax=FILES+se), width=.1)+
	scale_fill_manual(values = c("#fde725ff", "#1f968bff"))

farn.mod.bb<-glmer(FILES~treatment + 1|plot, data = bb.farn)
summary(farn.mod.bb)
Anova(farn.mod.bb)#can't get this to run
shapiro.test(resid(farn.mod.bb))#not normal

summary(glm(FILES~treatment, data = bb.farn))
