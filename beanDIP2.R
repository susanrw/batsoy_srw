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
cv[, 3:11][is.na(cv[, 3:11])] <- 0

##Gathering data — compounds from col to rows
cv1<-cv %>% gather(sp, activity, EPTFUS:NOID)
cv1$sp<-as.factor(cv1$sp)
levels(cv1$sp)
cv1<-aggregate(activity ~ sp + jdate + site, dat=cv1, FUN=sum)
cv1$activity[is.na(cv1$activity)] <- 0
#log transformation
cv1$log.act<-log(cv1$activity)
#0 activity species/nights 
cv1$log.act[which(!is.finite(cv1$log.act))] <- 0

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
#0 activity species/nights 
cv3$log.act[which(!is.finite(cv3$log.act))] <- 0

#Cville plot, species groups, sum, raw numbers
plot.cv.all<-cv3 %>%
	ggplot(aes(x=jdate, 
			   y=activity))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()
plot.cv.all
#split by species
plot.cv.all+aes(color=sp)
#faceted by species
plot.cv.all+aes(color=sp)+facet_wrap(~sp)


#Cville plot, species groups, log-transformed 
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
#split by species
plot.cv.log+aes(color=sp)

#faceted by species
plot.cv.log+aes(color=sp)+facet_wrap(~sp)

#Cville, no species grouping, raw
plot.cv.1<-cv1 %>%
	ggplot(aes(x=jdate, 
			   y=activity))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()
plot.cv.1
#split by species
plot.cv.1+aes(color=sp)
#faceted by species
plot.cv.1+aes(color=sp)+facet_wrap(~sp)

#Cville, no species grouping, log-transformed
plot.cv.1.log<-cv1 %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()
plot.cv.1.log
#split by species
plot.cv.1.log+aes(color=sp)
#faceted by species
plot.cv.1.log+aes(color=sp)+facet_wrap(~sp)

#filtering for insect/plant sampling dates at Cville
#jdates=189,209,229,271


#Wye----
wy <- read.csv(file="Wye_summary.csv",head=TRUE)
wy[, 3:11][is.na(wy[, 3:11])] <- 0

##Gathering data — compounds from col to rows
wy1<-wy %>% gather(sp, activity, EPTFUS:NOID)
wy1$sp<-as.factor(wy1$sp)
levels(wy1$sp)
wy1<-aggregate(activity ~ sp + jdate, dat=wy1, FUN=sum)
#log transformation
wy1$log.act<-log(wy1$activity)
#0 activity species/nights 
wy1$log.act[which(!is.finite(wy1$log.act))] <- 0

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
wy3$log.act[which(!is.finite(wy3$log.act))] <- 0

#Wye plot, species groups, sum, raw numbers
plot.wy.all<-wy3 %>%
	ggplot(aes(x=jdate, 
			   y=activity))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Wye")+
	theme_classic()
plot.wy.all
#split by species
plot.wy.all+aes(color=sp)
#faceted by species
plot.wy.all+aes(color=sp)+facet_wrap(~sp)


#Wye plot, species groups, log-transformed 
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
#split by species
plot.wy.log+aes(color=sp)

#faceted by species
plot.cv.log+aes(color=sp)+facet_wrap(~sp)

#Wye, no species grouping, raw
plot.wy.1<-wy1 %>%
	ggplot(aes(x=jdate, 
			   y=activity))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Wye")+
	theme_classic()
plot.wy.1
#split by species
plot.wy.1+aes(color=sp)
#faceted by species
plot.wy.1+aes(color=sp)+facet_wrap(~sp)

#Wye, no species grouping, log-transformed
plot.wy.1.log<-wy1 %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Wye")+
	theme_classic()
plot.wy.1.log
#split by species
plot.wy.1.log+aes(color=sp)
#faceted by species
plot.wy.1.log+aes(color=sp)+facet_wrap(~sp)

##Chew data
chew <- read.csv(file="beanDIP_chew.csv",head=TRUE)
chew<-na.omit(chew)
chew$chew_pct<-as.numeric(chew$chew_pct)
chew$jdate<-as.numeric(chew$jdate)

chew1<-aggregate(chew_pct ~ site + sampling.round + jdate, dat=chew, FUN=mean)

chew1 %>%
	ggplot(aes(x=sampling.round, 
			   y=chew_pct))+
	geom_point()+
	geom_smooth()+
	labs(x="Sampling round",
		 y="Average chew damage")+
	theme_classic()+
	facet_wrap(~site)

chew1 %>%
	ggplot(aes(x=jdate, 
			   y=chew_pct))+
	geom_point()+
	geom_smooth()+
	labs(x="jdate",
		 y="Average chew damage")+
	theme_classic()+
	facet_wrap(~site)

##Abundance data
insect <- read.csv(file="beanDIP_insects.csv",head=TRUE)
insect$calc.ab<-as.numeric(insect$calc.ab)
insect<-na.omit(insect)#240 to 222 obs
insect$calc.ab<-as.numeric(insect$calc.ab)
insect$insect.rich<-as.numeric(insect$insect.rich)
library(plotrix)
insect1<-aggregate(insect.rich ~ site + round +jdate, dat=insect, FUN = mean)

library(Rmisc)
sum.ab<-summarySE(insect, measurevar="calc.ab", groupvars=c("site", "round", "jdate"))
sum.ab %>%
	ggplot(aes(x=jdate, 
			   y=calc.ab,
			   color=site))+
	geom_errorbar(aes(ymin=calc.ab-se, ymax=calc.ab+se), width=.1)+
	geom_line()+
	labs(x="Sampling round",
		 y="Average insect abundance")+
	theme_classic()+
	facet_wrap(~site)

sum.rich<-summarySE(insect, measurevar="insect.rich", groupvars=c("site", "round", "jdate"))
sum.rich %>%
	ggplot(aes(x=jdate, 
			   y=insect.rich))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin=insect.rich-se, ymax=insect.rich+se), width=.1)+
	labs(x="Sampling round",
		 y="Average insect richness")+
	theme_classic()+
	facet_wrap(~site)

insect$no.chew<-as.numeric(insect$no.chew)
insect$no.p_s<-as.numeric(insect$no.p_s)
insect$no.n_e<-as.numeric(insect$no.n_e)
insect$no.other<-as.numeric(insect$no.other)
insect$prop.herb<-(insect$no.chew+insect$no.p_s)/(insect$calc.ab)
insect<-na.omit(insect)#222 to 220

herb.prop.sum<-summarySE(insect, measurevar="prop.herb", groupvars=c("site", "round", "jdate"))
herb.prop.sum %>%
	ggplot(aes(x=jdate, 
			   y=prop.herb))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin=prop.herb-se, ymax=prop.herb+se), width=.1)+
	labs(x="Sampling round",
		 y="Average insect richness")+
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

