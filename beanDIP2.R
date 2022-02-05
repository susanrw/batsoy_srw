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
library(lubridate)

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
cv2$sp[cv2$sp=="NOID"]="No ID"
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
#jdates=189,209,229,271(but last bat sampling date is 269)
cv.bat.insect<-filter(cv1, jdate == "189" | jdate == "209"| jdate == "229"| jdate == "269")

#Cville, no species grouping, log-transformed
plot.cv.1.log.insect<-cv.bat.insect %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()
plot.cv.1.log.insect
#split by species
plot.cv.1.log.insect+aes(color=sp)
#faceted by species
plot.cv.1.log.insect+aes(color=sp)+facet_wrap(~sp)

cv.bat.sum<-summarySE(cv.bat.insect, measurevar="log.act", groupvars=c("jdate"))
cv.bat.sum %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_errorbar(aes(ymin=log.act-se, ymax=log.act+se), width=.1)+
	geom_line()+
	labs(x="Date",
		 y="Average bat activity",
		 title = "Clarksville")+
	theme_classic()

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

#filtering for insect/plant sampling dates at Wye
#jdates=187,208,228,265
wy.bat.insect<-filter(wy1, jdate == "187" | jdate == "208"| jdate == "228"| jdate == "265")

#Wye, no species grouping, log-transformed
plot.wy.1.log.insect<-wy.bat.insect %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity",
		 color="Species",
		 title = "Wye")+
	theme_classic()
plot.wy.1.log.insect
#split by species
plot.wy.1.log.insect+aes(color=sp)
#faceted by species
plot.wy.1.log.insect+aes(color=sp)+facet_wrap(~sp)

wy.bat.sum<-summarySE(wy.bat.insect, measurevar="log.act", groupvars=c("jdate"))
wy.bat.sum %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_errorbar(aes(ymin=log.act-se, ymax=log.act+se), width=.1)+
	geom_line()+
	labs(x="Date",
		 y="Average bat activity (log)",
		 title="Wye")+
	theme_classic()

##Chew data----
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

sum.chew<-summarySE(chew, measurevar="chew_pct", groupvars=c("site", "sampling.round", "jdate"))
sum.chew %>%
	ggplot(aes(x=jdate, 
			   y=chew_pct))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin=chew_pct-se, ymax=chew_pct+se), width=.1)+
	labs(x="Date (Julian)",
		 y="Chew damage (%)")+
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
			   y=calc.ab))+
	geom_errorbar(aes(ymin=calc.ab-se, ymax=calc.ab+se), width=.1)+
	geom_line()+
	labs(x="Date (Julian)",
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
	labs(x="Date (Julian)",
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
	labs(x="Date (Julian)",
		 y="Proportion herbivores")+
	theme_classic()+
	facet_wrap(~site)


##Temperature/RH hourly data----
#Wye
trh.wye <- read.csv(file="Wye_temp_rh.csv",head=TRUE)
trh.wye$jdate<-NA
trh.wye$jdate<-yday(trh.wye$date)

trh.wye<- trh.wye %>% filter(jdate > 170)
trh.wye<- trh.wye %>% filter(jdate < 270)

trh.wye$jdate<-as.numeric(trh.wye$jdate)
trh.wye$Temp.F<-as.numeric(trh.wye$Temp.F)

#hourly temp graph
trh.wye %>%
	ggplot(aes(x=time,
			   y=Temp.F))+
	geom_smooth()+
	geom_point(aes(color=jdate))+
	theme_classic()+
	labs(x="Time",
		 y="Temperature (F)",
		 title = "Wye",
		 color="Date (Julian)")+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#hourly rh graph
trh.wye %>%
	ggplot(aes(x=time,
			   y=Rh.pct))+
	geom_smooth()+
	geom_point(aes(color=jdate))+
	theme_classic()+
	labs(x="Time",
		 y="Relative humidity (%)",
		 title = "Wye",
		 color="Date (Julian)")+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

trh.wye.temp<-summarySE(trh.wye, measurevar="Temp.F", groupvars=c("jdate"))
trh.wye.rh<-summarySE(trh.wye, measurevar="Rh.pct", groupvars=c("jdate"))
trh.wye.temp %>%
	ggplot(aes(x=jdate, 
			   y=Temp.F))+
	geom_errorbar(aes(ymin=Temp.F-se, ymax=Temp.F+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Average Temp (F)",
		 title="Wye")+
	theme_classic()

trh.wye.rh %>%
	ggplot(aes(x=jdate, 
			   y=Rh.pct))+
	geom_errorbar(aes(ymin=Rh.pct-se, ymax=Rh.pct+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Relative humidity (%)",
		 title="Wye")+
	theme_classic()

#Cville
trh.cv <- read.csv(file="Cv_temp_rh.csv",head=TRUE)
trh.cv$date<-as.Date(trh.cv$date,)
trh.cv$jdate<-NA
trh.cv$jdate<-yday(trh.cv$date)

trh.cv<- trh.cv %>% filter(jdate > 170)
trh.cv<- trh.cv %>% filter(jdate < 270)

trh.cv$jdate<-as.numeric(trh.cv$jdate)
trh.cv$Temp.F<-as.numeric(trh.cv$Temp.F)

#hourly temp graph
trh.cv %>%
	ggplot(aes(x=time,
			   y=Temp.F))+
	geom_smooth()+
	geom_point(aes(color=jdate))+
	theme_classic()+
	labs(x="Time",
		 y="Temperature (F)",
		 title = "Clarksville",
		 color="Date (Julian)")+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#hourly rh graph
trh.cv %>%
	ggplot(aes(x=time,
			   y=RH.pct))+
	geom_smooth()+
	geom_point(aes(color=jdate))+
	theme_classic()+
	labs(x="Time",
		 y="Relative humidity (%)",
		 title = "Clarksville",
		 color="Date (Julian)")+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#summarizing data
trh.cv.temp<-summarySE(trh.cv, measurevar="Temp.F", groupvars=c("jdate"))
trh.cv.rh<-summarySE(trh.cv, measurevar="RH.pct", groupvars=c("jdate"))
trh.cv.temp %>%
	ggplot(aes(x=jdate, 
			   y=Temp.F))+
	geom_errorbar(aes(ymin=Temp.F-se, ymax=Temp.F+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Average Temp (F)",
		 title="Clarksville")+
	theme_classic()

trh.cv.rh %>%
	ggplot(aes(x=jdate, 
			   y=RH.pct))+
	geom_errorbar(aes(ymin=RH.pct-se, ymax=RH.pct+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Relative humidity (%)",
		 title="Clarksville")+
	theme_classic()


 ##Temp/precip daily data----
#wye
tp.wye <- read.csv(file="beanDIP_wye_temp_precip.csv",head=TRUE)

tp.wye<- tp.wye %>% filter(jdate > 170)
tp.wye<- tp.wye %>% filter(jdate < 270)
tp.wye$mean.temp<-NA
tp.wye$mean.temp<-rowMeans(tp.wye[,c('min.temp.c', 'max.temp.c')], na.rm=TRUE)

##Gathering data — compounds from col to rows
tp.wye1<-tp.wye %>% gather(temp, C, max.temp.c:min.temp.c)

#temp graph (high low)
tp.wye1 %>%
	ggplot(aes(x=jdate, 
			   y=C,
			   color=temp))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Temperature (C)",
		 title="Wye")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#temp graph (mean)
tp.wye1 %>%
	ggplot(aes(x=jdate, 
			   y=mean.temp))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Average Temperature (C)",
		 title="Wye")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#temp graph
tp.wy1 %>%
	ggplot(aes(x=jdate, 
			   y=mean.temp.C))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Mean Temperature (C)",
		 title="Clarksville")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#precip graph
tp.wye1 %>%
	ggplot(aes(x=jdate, 
			   y=precip))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Precipitation (in)",
		 title="Wye")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#Clarksville
tp.cv <- read.csv(file="Cv_temp_precip.csv",head=TRUE)

tp.cv<- tp.cv %>% filter(jdate > 170)
tp.cv<- tp.cv %>% filter(jdate < 270)

##Gathering data — compounds from col to rows
tp.cv1<-tp.cv %>% gather(temp, C, c(low.C,high.C))

#temp graph (high low)
tp.cv1 %>%
	ggplot(aes(x=jdate, 
			   y=C,
			   color=temp))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Temperature (C)",
		 title="Clarksville")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#temp graph
tp.cv1 %>%
	ggplot(aes(x=jdate, 
			   y=mean.temp.C))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Mean Temperature (C)",
		 title="Clarksville")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#precip graph
tp.cv1 %>%
	ggplot(aes(x=jdate, 
			   y=rain))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Precipitation (in)",
		 title="Clarksville")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#wind graph, avg
tp.cv %>%
	ggplot(aes(x=jdate, 
			   y=avg.wind.speed))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Average wind speed (mph)",
		 title="Clarksville")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

#wind graph, high
tp.cv %>%
	ggplot(aes(x=jdate, 
			   y=high.1))+
	geom_point()+
	geom_smooth()+
	labs(x="Julian Date",
		 y="Wind speed high (mph)",
		 title="Clarksville")+
	theme_classic()+
	scale_x_continuous(limits = c(170, 270))

##correlation matrix----
library(corrplot)

#filtering for insect/plant sampling dates at Wye over 3 days
#jdates=187,208,228,265
wy.bat.insect<-filter(wy1, jdate == "187" | jdate == "208"| jdate == "228"| jdate == "265")

cor1<-all.dat100[,-c(1,2,4:9,11:13)]#cat vars

L1 <- cor(samp1)#correlation matrix
corrplot(L1, method = "circle")
corrplot(L1, method = "number")
#low colinearity