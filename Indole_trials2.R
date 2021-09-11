#read in all data
{ c1.2 <- read.csv(file="indole_4672_c.csv",head=TRUE)
c2.2 <- read.csv(file="indole_4614_c.csv",head=TRUE)
c3.2 <- read.csv(file="indole_4604_c.csv",head=TRUE)
c4.2 <- read.csv(file="indole_1676_c.csv",head=TRUE)
c5.2 <- read.csv(file="indole_0505_c.csv",head=TRUE)
d1.2 <- read.csv(file="indole_4655_d.csv",head=TRUE)
d2.2 <- read.csv(file="indole_2198_d.csv",head=TRUE)
d3.2 <- read.csv(file="indole_4608_d.csv",head=TRUE)
d4.2 <- read.csv(file="indole_2207_d.csv",head=TRUE)
d5.2 <- read.csv(file="indole_1708_d.csv",head=TRUE)
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
		 title = "Big brown bats (EPTFUS)")+
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
		 title = "Big brown bats (EPTFUS)")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")
