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

#Clarksville----
cv <- read.csv(file="Cville_summary.csv",head=TRUE)

##Gathering data — compounds from col to rows
cv1<-cv %>% gather(sp, activity, EPTFUS:NOID)
cv1$sp<-as.factor(cv1$sp)
levels(cv1$sp)
cv1<-aggregate(activity ~ sp + jdate + site, dat=cv1, FUN=sum)

##GROUPING SPECIES
{cv2<-cv1
cv2$sp=as.character(cv2$sp)
cv2$sp[cv2$sp=="EPTFUS"]="EPFU/LANO"
cv2$sp[cv2$sp=="LASNOC"]="EPFU/LANO"
cv2$sp[cv2$sp=="LASBOR"]="LABO/LASE"
cv2$sp[cv2$sp=="LASSEM"]="LABO/LASE"
cv2$sp[cv2$sp=="LASCIN"]="Other bat spp."
cv2$sp[cv2$sp=="MYOLUC"]="Other bat spp."
cv2$sp[cv2$sp=="PERSUB"]="Other bat spp."
cv2$sp[cv2$sp=="NOID"]="Other bat spp."
cv2$sp[cv2$sp=="NYCHUM"]="Other bat spp."
}

cv3<-aggregate(activity ~ sp + jdate, dat=cv2, FUN=sum)
cv3$log.act<-log(cv3$activity)
#one night had 0 recordings of "other" so couldn't do the transformation
cv3$log.act[which(!is.finite(cv3$log.act))] <- 0

#Cville plot
plot.cv<-cv3 %>%
	ggplot(aes(x=jdate, 
			   y=activity,
			   color=sp))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()
plot.cv
plot.cv+facet_wrap(~sp)

plot.cv.log<-cv3 %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (log)",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()
plot.cv.log

cv1 %>%
	ggplot(aes(x=jdate, 
			   y=activity,
			   color=sp))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()

cv3 %>%
	ggplot(aes(x=jdate, 
			   y=activity))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 title = "Clarksville")+
	theme_classic()

#Wye----
wy <- read.csv(file="Wye_summary.csv",head=TRUE)

##Gathering data — compounds from col to rows
wy1<-wy %>% gather(sp, activity, EPTFUS:NOID)
wy1$sp<-as.factor(wy1$sp)
levels(wy1$sp)
wy1<-aggregate(activity ~ sp + jdate, dat=wy1, FUN=sum)

##GROUPING SPECIES
{wy2<-wy1
	wy2$sp=as.character(wy2$sp)
	wy2$sp[wy2$sp=="EPTFUS"]="EPFU/LANO"
	wy2$sp[wy2$sp=="LASNOC"]="EPFU/LANO"
	wy2$sp[wy2$sp=="LASBOR"]="LABO/LASE"
	wy2$sp[wy2$sp=="LASSEM"]="LABO/LASE"
	wy2$sp[wy2$sp=="LASCIN"]="Other bat spp."
	wy2$sp[wy2$sp=="MYOLUC"]="Other bat spp."
	wy2$sp[wy2$sp=="PERSUB"]="Other bat spp."
	wy2$sp[wy2$sp=="NOID"]="Other bat spp."
	wy2$sp[wy2$sp=="NYCHUM"]="Other bat spp."
}

wy3<-aggregate(activity ~ sp + jdate, dat=wy2, FUN=sum)
wy3$log.act<-log(wy3$activity)
#one night had 0 recordings of "other" so couldn't do the transformation
wy3$log.act[which(!is.finite(wy3$log.act))] <- 0

#wyille plot
plot.wy<-wy3 %>%
	ggplot(aes(x=jdate, 
			   y=activity,
			   color=sp))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 color="Species",
		 title = "Wye")+
	theme_classic()
plot.wy
plot.wy+facet_wrap(~sp)

plot.wy.log<-wy3 %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (log)",
		 color="Species",
		 title = "Wye")+
	theme_classic()
plot.wy.log

wy1 %>%
	ggplot(aes(x=jdate, 
			   y=activity,
			   color=sp))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 color="Species",
		 title = "Wye")+
	theme_classic()

wy3 %>%
	ggplot(aes(x=jdate, 
			   y=activity))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 title = "Wye")+
	theme_classic()

##Chew data
chew <- read.csv(file="beanDIP_chew.csv",head=TRUE)
chew$chew_pct<-as.numeric(chew$chew_pct)
chew1<-aggregate(chew_pct ~ site + sampling.round, dat=chew, FUN=mean)

chew1 %>%
	ggplot(aes(x=sampling.round, 
			   y=chew_pct))+
	geom_point()+
	geom_smooth()+
	labs(x="Sampling round",
		 y="Average chew damage")+
	theme_classic()+
	facet_wrap(~site)

##Abundance data
abun <- read.csv(file="beanDIP_ab_B.csv",head=TRUE)
abun$chew_pct<-as.numeric(chew$chew_pct)
abun1<-aggregate(insect.ab ~ site + round, dat=abun, FUN=mean)

abun1 %>%
	ggplot(aes(x=round, 
			   y=insect.ab))+
	geom_point()+
	geom_smooth()+
	labs(x="Sampling round",
		 y="Average insect abundance")+
	theme_classic()+
	facet_wrap(~site)

##Temperature data
temp.wye <- read.csv(file="beanDIP_wye_temp.csv",head=TRUE)

temp.wye<- temp.wye %>% filter(jdate > 170)
temp.wye<- temp.wye %>% filter(jdate < 270)

##Gathering data — compounds from col to rows
temp.wye1<-temp.wye %>% gather(temp, C, max.temp.c:min.temp.c)

temp.wye1 %>%
	ggplot(aes(x=jdate, 
			   y=C,
			   color=temp))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Max temperature (C)",
		 title="Wye")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

