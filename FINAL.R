#Load libraries
{library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(car)
library(emmeans)
library(plyr)
library(multcomp)
library(lubridate)
library(stats)
library(corrplot)
library(MuMIn)
library(stats)
library(data.table)
library(plotrix)
library(gridExtra)
library(ggpubr)
library(multcompView)
}



#Note: because you need data from Q2 to answer Q1, they are in that order
#Q2a, Q2b, Q1, Q3
#all plots at end of script

##Q2a: Naturally occurring soybean HIPVS (damaged vs. undamaged plants)----

# Data import and cleaning
plant <- read.csv(file="Maynard_etal_plant_sum.csv",head=TRUE)
plant[, 3:11][is.na(plant[, 3:11])] <- 0

##Gathering data — compounds from col to rows
plant1<-plant %>% gather(sp, activity, EPTFUS:NOID)
plant1$sp<-as.factor(plant1$sp)
levels(plant1$sp)

#nightly data split by species
plant1<-aggregate(activity ~ treatment + sp + trial + jdate, dat=plant1, FUN=sum)

#nightly data for all species
plant10<-aggregate(activity ~ treatment + trial + jdate, dat=plant1, FUN=sum)


#Test for overdispersion function
overdisp_fun <- function(model) {
	rdf <- df.residual(model)
	rp <- residuals(model,type="pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

test<-glmer(activity~treatment+(1|trial), dat = plant10, family=poisson)
summary(test)
overdisp_fun(test)#overdispersed
#poisson distribution model=overdispersed with non-normal residuals,
#so fitting data to negative binomial 

#negative binomial model
mod.plant<-glmer.nb(activity~treatment+(1|trial), dat = plant10)
Anova(mod.plant)
#chisq=2.4452, p=0.1179; Wald chisquare tests

plant10.tab <- ddply(plant10, c("treatment"), summarise,
					 N    = length(activity),
					 mean = mean(activity),
					 sd   = sd(activity),
					 se   = sd / sqrt(N))
plant10.tab

##Q2b: sythentic soybean HIPVs (indole and farnesene)----
# Indole data import and cleaning

indole <- read.csv(file="Maynard_etal_indole_sum.csv",head=TRUE)
indole[, 3:11][is.na(indole[, 3:11])] <- 0
colnames(indole)[which(colnames(indole)=="ï..jdate")] <- "jdate"

##Gathering data — compounds from col to rows
indole1<-indole %>% gather(sp, activity, EPTFUS:NOID)
indole1$sp<-as.factor(indole1$sp)
levels(indole1$sp)

#aggregating data, species level
indole1<-aggregate(activity ~ treatment + sp + jdate + site, dat=indole1, FUN=sum)

#data without species-level
indole10<-aggregate(activity ~ treatment + jdate + site, dat=indole1, FUN=sum)

#creating log-transformed value for graphs
indole10$log.act<-log(indole10$activity)
#indole10$log.act[which(!is.finite(indole10$log.act))] <- 0 #no zeros, so unnecessary

test.in<-glmer(activity~treatment+(1|site), dat = indole10,family=poisson)
summary(test.in)
shapiro.test(resid(test.in))#non-normal
overdisp_fun(test.in)#overdispersed

#fitting data to glm with neg binom dist
mod.in<-glmer.nb(activity~treatment+(1|site), dat = indole10)
summary(mod.in)
Anova(mod.in)
#treatment chi=2.2635 p=0.1325, wald chisquared test

#table
in10.tab <- ddply(indole10, c("treatment"), summarise,
				  N    = length(activity),
				  mean = mean(activity),
				  sd   = sd(activity),
				  se   = sd / sqrt(N))
in10.tab
#control= 328.2 +/- 56.5
#treatment= 321.0 +/- 36.9


# Farnesene data import and cleaning----


farn <- read.csv(file="Maynard_etal_farnesene_sum.csv",head=TRUE)
farn[, 3:11][is.na(farn[, 3:11])] <- 0
colnames(farn)[which(colnames(farn)=="ï..jdate")] <- "jdate"

##Gathering data — compounds from col to rows
farn1<-farn %>% gather(sp, activity, EPTFUS:NOID)
farn1$sp<-as.factor(farn1$sp)
levels(farn1$sp)

#aggregated by species
farn1<-aggregate(activity ~ treatment + sp + jdate + site, dat=farn1, FUN=sum)

#w/o species-level
farn10<-aggregate(activity ~ treatment + jdate + site, dat=farn1, FUN=sum)

#creating log-transformed value for graphs
farn10$log.act<-log(farn10$activity)
#farn10$log.act[which(!is.finite(farn10$log.act))] <- 0 #no zeros

test.farn<-glmer(activity~treatment+(1|site), dat = farn10,family=poisson)
summary(test.farn)
shapiro.test(resid(test.farn))#non-normal
overdisp_fun(test.farn)#overdispersed

mod.farn<-glmer.nb(activity~treatment+(1|site), dat = farn10)
Anova(mod.farn)
#treatment chi=0.398 p=0.5281

#table
farn10.tab <- ddply(farn10, c("treatment"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
farn10.tab
#control 318.1 +/- 58.7
#dispenser 284.6 +/- 58.1

###Q1: bat species----
#COMBINING INDOLE AND FARNESENE TRIALS-
#importing extra/non-trial data
indole0 <- read.csv(file="Maynard_indole2_all.csv",head=TRUE)
indole0[, 3:11][is.na(indole0[, 3:11])] <- 0

##Gathering data — compounds from col to rows
indole01<-indole0 %>% gather(sp, activity, EPTFUS:NOID)
indole01$sp<-as.factor(indole01$sp)
levels(indole01$sp)
indole01<-aggregate(activity ~ sp + jdate + site, dat=indole01, FUN=sum)

#data without treatment
farn01<-aggregate(activity ~ sp + jdate + site, dat=farn1, FUN=sum)
indole001<-aggregate(activity ~ sp + jdate + site, dat=indole1, FUN=sum)

dis.all <- rbind(indole001, farn01, indole01)

##grouping and renaming species
{dis.all<-dis.all
dis.all$sp=as.character(dis.all$sp)
dis.all$sp[dis.all$sp=="EPTFUS"]="EPFU/LANO"
dis.all$sp[dis.all$sp=="LASNOC"]="EPFU/LANO"
dis.all$sp[dis.all$sp=="LASBOR"]="LABO/LASE"
dis.all$sp[dis.all$sp=="LASSEM"]="LABO/LASE"
dis.all$sp[dis.all$sp=="NOID"]="No ID"
dis.all$sp[dis.all$sp=="MYOLUC"]="MYLU"
dis.all$sp[dis.all$sp=="LASCIN"]="LACI"
dis.all$sp[dis.all$sp=="NYCHUM"]="NYHU"
dis.all$sp[dis.all$sp=="PERSUB"]="PESU"
dis.all$sp[dis.all$sp=="MYOSOD"]="No ID"
}
#only one MYSO, so grouping with No ID. Could do Myotis spp?

dis.all1<-aggregate(activity ~  sp + jdate + site, dat=dis.all, FUN=sum)

q1<-glmer.nb(activity~sp+(1|site), data = dis.all1)
Anova(q1)
#chisq=2678.4, p<0.001, Wald chisquare tests
summary(q1)

#contrasts
q1c<-emmeans(q1,pairwise~sp, type="response")
cld(q1c$emmeans,  Letters ='abcdefg')
#MYLU=a, NYHU=b, PESU=c, LABO/LASE=d, LACI=d, NOID=e, EPFU/LANO=f

q1.tab <- ddply(dis.all1, c("sp"), summarise,
				N    = length(activity),
				mean = mean(activity),
				sd   = sd(activity),
				se   = sd / sqrt(N))
q1.tab

###Q3: weather----

#I didn't write a forloop, so there are just a lot of lines

#read in all data
{ind1 <- read.csv(file="indole1_id_1708_c.csv",head=TRUE)
ind2 <- read.csv(file="indole1_id_2198_c.csv",head=TRUE)
ind3 <- read.csv(file="indole1_id_2207_c.csv",head=TRUE)
ind4 <- read.csv(file="indole1_id_4655_c.csv",head=TRUE)
ind5 <- read.csv(file="indole1_id_4608_c.csv",head=TRUE)
ind6 <- read.csv(file="indole2_id_c_0505.csv",head=TRUE)
ind7 <- read.csv(file="indole3_id_0505_c.csv",head=TRUE)
ind8 <- read.csv(file="indole2_id_c_1676.csv",head=TRUE)
ind9 <- read.csv(file="indole3_id_1676_c.csv",head=TRUE)
ind10 <- read.csv(file="indole2_id_c_4604.csv",head=TRUE)
ind11 <- read.csv(file="indole3_id_4604_c.csv",head=TRUE)
ind12 <- read.csv(file="indole2_id_c_4614.csv",head=TRUE)
ind13 <- read.csv(file="indole3_id_4614_c.csv",head=TRUE)
ind14 <- read.csv(file="indole2_id_c_4672.csv",head=TRUE)
ind15 <- read.csv(file="indole3_id_4672_c.csv",head=TRUE)
far1 <- read.csv(file="farn1_id_0505_c.csv",head=TRUE)
far2 <- read.csv(file="farn1_id_1676_c.csv",head=TRUE)
far3 <- read.csv(file="farn1_id_2207_c.csv",head=TRUE)
far4 <- read.csv(file="farn1_id_4604_c.csv",head=TRUE)
far5 <- read.csv(file="farn1_id_4655_c.csv",head=TRUE)
far6 <- read.csv(file="farn2_id_1708_c.csv",head=TRUE)
far7 <- read.csv(file="farn2_id_2198_c.csv",head=TRUE)
far8 <- read.csv(file="farn2_id_4608_c.csv",head=TRUE)
far9 <- read.csv(file="farn2_id_4614_c.csv",head=TRUE)
far10 <- read.csv(file="farn2_id_4672_c.csv",head=TRUE)
f1_1708d <- read.csv(file="farn1_id_1708_d.csv",head=TRUE)
f1_2198d <- read.csv(file="farn1_id_2198_d.csv",head=TRUE)
f1_4608d <- read.csv(file="farn1_id_4608_d.csv",head=TRUE)
f1_4614d <- read.csv(file="farn1_id_4614_d.csv",head=TRUE)
f1_4672d <- read.csv(file="farn1_id_4672_d.csv",head=TRUE)
f2_0505d <- read.csv(file="farn2_id_0505_d.csv",head=TRUE)
f2_1676d <- read.csv(file="farn2_id_1676_d.csv",head=TRUE)
f2_2207d <- read.csv(file="farn2_id_2207_d.csv",head=TRUE)
f2_4604d <- read.csv(file="farn2_id_4604_d.csv",head=TRUE)
f2_4655d <- read.csv(file="farn2_id_4655_d.csv",head=TRUE)
i1_0505d <- read.csv(file="indole1_id_0505_d.csv",head=TRUE)
i1_1676d <- read.csv(file="indole1_id_1676_d.csv",head=TRUE)
i1_4604d <- read.csv(file="indole1_id_4604_d.csv",head=TRUE)
i1_4614d <- read.csv(file="indole1_id_4614_d.csv",head=TRUE)
i1_4672d <- read.csv(file="indole1_id_4672_d.csv",head=TRUE)
i2_1708d <- read.csv(file="indole2_id_d_1708.csv",head=TRUE)
i2_2198d <- read.csv(file="indole2_id_d_2198.csv",head=TRUE)
i2_2207d <- read.csv(file="indole2_id_d_2207.csv",head=TRUE)
i2_4608d <- read.csv(file="indole2_id_d_4608.csv",head=TRUE)
i3_1708d <- read.csv(file="indole3_id_1708_d.csv",head=TRUE)
i3_2198d <- read.csv(file="indole3_id_2198_d.csv",head=TRUE)
i3_2207d <- read.csv(file="indole3_id_2207_d.csv",head=TRUE)
i3_4608d <- read.csv(file="indole3_id_4608_d.csv",head=TRUE)
i3_4655d <- read.csv(file="indole3_id_4655_d.csv",head=TRUE)
}

#creating plot columns
{ind1$site<-NA
	ind2$site<-NA
	ind3$site<-NA
	ind4$site<-NA
	ind5$site<-NA
	ind6$site<-NA
	ind7$site<-NA
	ind8$site<-NA
	ind9$site<-NA
	ind10$site<-NA
	ind11$site<-NA
	ind12$site<-NA
	ind13$site<-NA
	ind14$site<-NA
	ind15$site<-NA
	far1$site<-NA
	far2$site<-NA
	far3$site<-NA
	far4$site<-NA
	far5$site<-NA
	far6$site<-NA
	far7$site<-NA
	far8$site<-NA
	far9$site<-NA
	far10$site<-NA
	f1_1708d$site<-NA
	f1_2198d$site<-NA
	f1_4608d$site<-NA
	f1_4614d$site<-NA
	f1_4672d$site<-NA
	f2_0505d$site<-NA
	f2_1676d$site<-NA
	f2_2207d$site<-NA
	f2_4604d$site<-NA
	f2_4655d$site<-NA
	i1_0505d$site<-NA
	i1_1676d$site<-NA
	i1_4604d$site<-NA
	i1_4614d$site<-NA
	i1_4672d$site<-NA
	i2_1708d$site<-NA
	i2_2198d$site<-NA
	i2_2207d$site<-NA
	i2_4608d$site<-NA
	i3_1708d$site<-NA
	i3_2198d$site<-NA
	i3_2207d$site<-NA
	i3_4608d$site<-NA
	i3_4655d$site<-NA
}

#populating plot columns
{ind1$site<-"1708"
	ind2$site<-'2198'
	ind3$site<-'2207'
	ind4$site<-'4655'
	ind5$site<-'4608'
	ind6$site<-'0505'
	ind7$site<-'1676'
	ind8$site<-'4604'
	ind9$site<-'4614'
	ind10$site<-'4672'
	ind11$site<-'0505'
	ind12$site<-'1676'
	ind13$site<-'4604'
	ind14$site<-'4614'
	ind15$site<-'4672'
	far1$site<-'0505'
	far2$site<-'1676'
	far3$site<-'2207'
	far4$site<-'4604'
	far5$site<-'4655'
	far6$site<-'1708'
	far7$site<-'2198'
	far8$site<-'4608'
	far9$site<-'4614'
	far10$site<-'4672'
	f1_1708d$site<-'1708'
	f1_2198d$site<-'2198'
	f1_4608d$site<-'4608'
	f1_4614d$site<-'4614'
	f1_4672d$site<-'4672'
	f2_0505d$site<-'0505'
	f2_1676d$site<-'1676'
	f2_2207d$site<-'2207'
	f2_4604d$site<-'4604'
	f2_4655d$site<-'4655'
	i1_0505d$site<-'0505'
	i1_1676d$site<-'1676'
	i1_4604d$site<-'4604'
	i1_4614d$site<-'4614'
	i1_4672d$site<-'4672'
	i2_1708d$site<-'1708'
	i2_2198d$site<-'2198'
	i2_2207d$site<-'2207'
	i2_4608d$site<-'4608'
	i3_1708d$site<-'1708'
	i3_2198d$site<-'2198'
	i3_2207d$site<-'2207'
	i3_4608d$site<-'4608'
	i3_4655d$site<-'4655'
}

#combine datasets
bat.hour <- rbind(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8,ind9,ind10,
				  ind11,ind12,ind13,ind14,ind15,far1,far2,far3,far4,far5,
				  far6,far7,far8,far9,far10, f1_1708d, f1_2198d, f1_4608d, f1_4614d,
				  f1_4672d, f2_0505d,f2_1676d,f2_2207d,f2_4604d,f2_4655d,i1_0505d,
				  i1_1676d,i1_4604d, i1_4614d,i1_4672d,i2_1708d,i2_2198d,i2_2207d,i2_4608d,
				  i3_1708d,i3_2198d,i3_2207d,i3_4608d,i3_4655d)

#remove noise files
bat.hour<-bat.hour[bat.hour$AUTO.ID. != "Noise", ]  

#select columns (can't have MASS loaded when you do this or will give error)
bat.hour<-bat.hour %>%
	dplyr::select(TIME, HOUR, DATE, AUTO.ID., FILES, site)

#insert jdate
bat.hour$jdate<-NA
bat.hour$jdate<-yday(bat.hour$DATE)

#renaming columns
colnames(bat.hour)[2] <- "hour"
colnames(bat.hour)[4] <- "sp"
colnames(bat.hour)[5] <- "activity"

#aggregate hourly bat data with all species summed
bat.hour.all<-aggregate(activity ~ jdate + hour+site, dat=bat.hour, FUN=sum)
#and averaged across all sites
bat.hour.all1<-aggregate(activity ~ jdate + hour, dat=bat.hour.all, FUN=mean)

#creating autoregressive term
bat.hour.all1<-bat.hour.all1[order(bat.hour.all1$jdate, bat.hour.all1$hour),]
acf(bat.hour.all1$activity, type = "correlation")
bat.hour.all1$act2<-lag(bat.hour.all1$activity, k=1)

#add 0s to the beginning of each night to rep no activity and not pre-sunrise data
bat.hour.all1<-bat.hour.all1[order(bat.hour.all1$hour, bat.hour.all1$jdate),]
#jdate 251, 257-262, 264-267 had calls at 1800 
#select rows for those dates
bat.hour.all1$act2[c(232,245, 249:254,256:259)]<-0

##SERC met data----
aug <- read.csv(file="SERC_TOWER_aug2021.csv",head=TRUE)
sep <- read.csv(file="SERC_TOWER_sep2021.csv",head=TRUE)
jun <- read.csv(file="SERC_TOWER_june2021.csv",head=TRUE)
jul <- read.csv(file="SERC_TOWER_july2021.csv",head=TRUE)

met<-rbind(aug,sep,jun,jul)
colnames(met)[which(colnames(met)=="ï..date")] <- "date"

met$date<-as.Date(met$date,)
met$jdate<-NA
met$jdate<-yday(met$date)

#aggregate data so it's hourly
#most variables averaged
met.hour.avg<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,
							  delta.air,Air_Pressure_pascal,
							  Air_Temperature_C)~hour + jdate, dat=met, FUN=mean)
#rain variables summed
met.hour.sum<-aggregate(cbind(Rain_Accumulation_mm,Rain_Duration_s)~hour + jdate, dat=met, FUN=sum)

#combine the two
met.hour<-cbind(met.hour.avg,met.hour.sum)
#remove duplicate columns, hour & jdate 
met.hour<-met.hour[,-c(8,9)]

##creating change in average hourly air pressure
#ordering data
met.hour<-met.hour[order(met.hour$jdate, met.hour$hour),]
#creating hourly change in air pressure var
met.hour$air2<-lag(met.hour$Air_Pressure_pascal, k=1)
met.hour$delta.air2=(met.hour$Air_Pressure_pascal)-(met.hour$air2)

#jdate 238-252, 255-270 (no detectors out nights of 253&254)
#met1 <- met.hour%>% filter( between(jdate, 238, 252))
#met2 <- met.hour%>% filter( between(jdate, 255, 270))
#met3<-rbind(met1,met2)

#filtering for hours bats were active, recorders active (6p-7a)
met4<-met.hour%>%filter(between(hour, 0,6))
met5<-met.hour%>%filter(between(hour, 19,23))
met6<-rbind(met4,met5)


#creating log-transformed variable for rain
met6$rain.log<-log((met6$Rain_Duration_s)+0.01)

met6[is.na(met6)]<-0

#creating binary rain variable by the hour
met6$rain_binary<-NA
for(i in 1:length(met6$Rain_Duration_s)){
	if(met6$Rain_Duration_s[i]==0){met6$rain_binary[i]="0"}
	if(met6$Rain_Duration_s[i]>0){met6$rain_binary[i]="1"}
}
met6$rain_binary<-as.numeric(met6$rain_binary)

#creating consecutive rain value
setDT(met6)[, rain_con_hours := cumsum(rain_binary), by = rleid(rain_binary == 0)]
met6$rain_con_hours<-as.numeric(met6$rain_con_hours)

##CHECKING FOR COLINEARITY----
met7<-met6[,c(3:14)]

M1 <- cor(met7)#correlation matrix
corrplot(M1, method = "circle")
corrplot(M1, method = "number")
#need to choose between rain and wind variables
#wind avg, consecutive rain

bat.met.hour<-merge(bat.hour.all1, met6, by=c("jdate","hour"))

#creating nonlinear wind var
bat.met.hour$wind.avg2 <- (as.numeric(bat.met.hour$Wind_speed_avg_m.s))^2


library(MASS)
#bat.met.hour[is.na(bat.met.hour)]<-0
mod1<-glm(activity~Air_Pressure_pascal+ Air_Temperature_C + 
		  	delta.air2 + act2 + Wind_speed_avg_m.s + wind.avg2, dat = bat.met.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod1)

#dredging and model averaging
d1<-dredge(mod1)
davg1<-model.avg(d1, subset=delta<2)
summary(davg1)

exp(-0.4972505)#intercept, 0.61
exp(0.4972505)#or intercept= -1.64??
exp(0.0209956)#ar term slope, 1.02
exp(0.0902396)#temperature slope, 1.09
exp(0.8317915)#linear wind slope, 2.30
exp(0.0001986)#delta air slope, 1.00


#removing zeros from consecutive rain hours data
rain.bat.hour<-bat.met.hour[bat.met.hour$rain_con_hours != "0", ]  

mod2<-glm(activity~rain_con_hours, dat = rain.bat.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod2)
exp(0.01357)#slope=-1.01
exp(3.49)#intercept=32.8
Anova(mod2, test.statistic = "F")
#F=0,34, p=0.56

plot(rain.bat.hour$activity~rain.bat.hour$rain_con_hours)+
	abline(glm((rain.bat.hour$activity~rain.bat.hour$rain_con_hours)))
summary(glm((rain.bat.hour$activity~rain.bat.hour$rain_con_hours)))

#creating model with top variables to make prediction plots
mod3<-glm(activity~Air_Temperature_C + act2 + Wind_speed_avg_m.s + wind.avg2, dat = bat.met.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod3)

bat.met.hour$yhat<-predict(mod3)
predplot1<-ggplot(bat.met.hour)+
	geom_point(aes(x=Air_Temperature_C, y=activity))+
	geom_line(aes(x=Air_Temperature_C, y=yhat) ,color="red", size=1)
predplot1

predplot2<-ggplot(bat.met.hour)+
	geom_point(aes(x=wind.avg2, y=activity))+
	geom_point(aes(x=wind.avg2, y=yhat), color="red", size=2)+
	geom_line(aes(x=wind.avg2, y=yhat) ,color="red", size=1)
predplot2

predplot3<-ggplot(bat.met.hour)+
	geom_point(aes(x=Wind_speed_avg_m.s, y=activity))+
	geom_point(aes(x=Wind_speed_avg_m.s, y=yhat), color="red", size=2)+
	geom_line(aes(x=Wind_speed_avg_m.s, y=yhat) ,color="red", size=1)
predplot3

predplot5<-ggplot(bat.met.hour)+
	geom_point(aes(x=delta.air2, y=activity))+
	geom_point(aes(x=delta.air2, y=yhat), color="red", size=2)+
	geom_line(aes(x=delta.air2, y=yhat) ,color="red", size=1)
predplot5

rain.bat.hour$yhat<-predict(mod2)
predplot4<-ggplot(rain.bat.hour)+
	geom_point(aes(x=rain_con_hours, y=activity))+
	geom_point(aes(x=rain_con_hours, y=yhat), color="red", size=2)+
	geom_line(aes(x=rain_con_hours, y=yhat) ,color="red", size=1)
predplot4


##As an alternative to the above, you could use a GAM

#this thread is a helpful discussion of GAM vs using a polynomial term
#https://stats.stackexchange.com/questions/166796/how-does-one-perform-multiple-non-linear-regression

library(mgcv)
m.gam <-gam(activity~Air_Pressure_pascal+ Air_Temperature_C + 
		   	delta.air2 + act2 + s(Wind_speed_avg_m.s, bs="ts"), dat = bat.met.hour,
		   family = Gamma(link=log),na.action = "na.fail")
summary(m.gam)

#dredging and model averaging
d5<-dredge(m.gam3)
davg5<-model.avg(d5, subset=delta<3)
summary(davg5)

#This also runs if you include consecutive rain hours
#(I was not sure why you had this in a separate model above??)
m.gam2 <-gam(activity~Air_Pressure_pascal+ Air_Temperature_C + 
			delta.air2 + rain_con_hours + act2 + s(Wind_speed_avg_m.s, bs='ts'), 
			dat = bat.met.hour, family = Gamma(link=log),na.action = "na.fail")
summary(m.gam2)


#or you can specifically set the number of twists/turns in the spline
#function with k=X
m.gam3 <-gam(activity~Air_Pressure_pascal+ Air_Temperature_C + 
			 	delta.air2 + rain_con_hours + act2 + 
			 	s(Wind_speed_avg_m.s, bs='ts', k=3), 
			 dat = bat.met.hour, family = Gamma(link=log),na.action = "na.fail")
summary(m.gam3)

#this checks that k is not too small. P is large so that suggests this is good
gam.check(m.gam3)

#most threads seems to suggest that larger k is better, but with model where
#k is unconstrained you get a lot of twists and turns that I see no reason to 
#expect biologically. So I like the lower k, and I think it can be justified
#https://stats.stackexchange.com/questions/463405/purposely-restricting-k-in-mgcvs-gam

#also the model fit is not significantly better with larger k
anova(m.gam2, m.gam3, test="Chisq")

#The nice thing here is that you get one overall p-value for wind speed
#as a non-linear predictor variable, so it removes the complication
#of interpreting the linear terms and the polynomial term

#also I think the geom_smooth function in ggplot
#is doing the same thing as the "s" term in this model, so you can just
#use that for plots and it would be in line with the model results

#if you go with this approach, see plots I would suggest at bottom of code



#calculating means, ranges, and SEs of variables

#temp
mean(bat.met.hour$Air_Temperature_C)
std.error(bat.met.hour$Air_Temperature_C)
range(bat.met.hour$Air_Temperature_C)

#avg wind speed
mean(bat.met.hour$Wind_speed_avg_m.s)
std.error(bat.met.hour$Wind_speed_avg_m.s)
range(bat.met.hour$Wind_speed_avg_m.s)

#rain duration
mean(bat.met.hour$Rain_Duration_s)
#rained an average of 7.2 sec 
std.error(bat.met.hour$Rain_Duration_s)

#consecutive rain hours
mean(bat.met.hour$rain_con_hours)
range(bat.met.hour$rain_con_hours)
#longest stetch of rain=18 h
std.error(bat.met.hour$rain_con_hours)
plot(bat.met.hour$activity~bat.met.hour$rain_con_hours)

#air pressure
mean(bat.met.hour$Air_Pressure_pascal)
std.error(bat.met.hour$Air_Pressure_pascal)
range(bat.met.hour$Air_Pressure_pascal)

#change in air pressure
mean(bat.met.hour$delta.air2)
std.error(bat.met.hour$delta.air2)


#PLOTS----

#Q1: species
#creating log-transformed variable
dis.all1$log.act<-log(dis.all1$activity)
dis.all1$log.act[which(!is.finite(dis.all1$log.act))]<-0

#log-transformed species plot
q1.plot<-ggplot(data=dis.all1, aes(x=sp, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.2, height = .05), alpha=0.4, size=3, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Bat activity (log)")+
	theme(text = element_text(size=16), legend.position = "none", 
		  axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=.8, size=12))+
	stat_summary(geom = 'text', label = c("f","d","b","d","a","e","c"),
				 fun = max, vjust = -0.8, size=5, fontface="bold")+
	scale_y_continuous(limits = c(-0.1,8.5))+
	scale_x_discrete(limits=c("Big brown/silver-haired bat","Eastern red/Seminole bat", 
							  "Evening bat","Hoary bat",
							  "Little brown bat", "Tricolored bat", "No ID"),
					 labels=c(expression(paste("Big brown/ \n silver-haired bat")), 
					 		 expression(paste("Eastern red/ \n Seminole bat")),
					 		 "Evening bat","Hoary bat",
					 		 "Little brown bat", "Tricolored bat", "No ID"))+
	scale_color_manual(values = c("#450757", "#443885", "#2d678e","#178f8b", "#2ab977", "#fee800","#8fd839"))
q1.plot


#EXPORT PLOT
tiff('Q1.tiff', units="in", width=6, height=4, res=400)
q1.plot
dev.off()

dis.all1$sp1 = factor(dis.all1$sp, levels=c("Big brown/silver-haired bat","Eastern red/Seminole bat", 
											"Hoary bat","Little brown bat",
											"Evening bat", "Tricolored bat", "No ID"))
q1.sp.plot<-ggplot(data=dis.all1, aes(x=jdate, y=log.act))+ 
	geom_point(alpha=0.4, size=2.5, aes(color=sp1))+
	theme_classic()+
	geom_smooth(method = "gam", color="black")+
	labs(x="Julian date", y="Bat activity (log)")+
	theme(text = element_text(size=18), 
		  axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=.9),
		  legend.text = element_text(size = 10),
		  legend.title = element_text(size = 12,face = "bold"))+
	scale_color_manual(values = c("#450757", "#443885", "#2d678e","#178f8b", "#2ab977", "#8fd839","#fee800"),
					   limits=c("Big brown/silver-haired bat","Eastern red/Seminole bat", 
					   		 "Hoary bat","Little brown bat",
					   		 "Evening bat", "Tricolored bat", "No ID"))+
	guides(color=guide_legend(title="Species"))
q1.sp.plot

#EXPORT PLOT
tiff('Q1_time.tiff', units="in", width=7, height=4, res=400)
q1.sp.plot
dev.off()

q1.wrap<-q1.sp.plot+facet_wrap(~sp1, ncol = 2)+theme(legend.position = "none")
q1.wrap

#EXPORT PLOT
tiff('Q1_time_sp.tiff', units="in", width=7, height=6, res=400)
q1.wrap
dev.off()

#Q2a: plants 
plant.plot<-ggplot(data=plant10, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA, width=.5, lwd=1)+ 
	geom_point(position=position_jitter(width = 0.1), alpha=0.4, size=3, color="#810f7c")+
	theme_classic()+
	labs(x=" ", y="Bat activity (nightly passes)")+
	theme(text = element_text(size=16), axis.text.x = element_text(size = 15))+
	scale_x_discrete(limits=c("U", "D"),
					 labels=c(expression(atop("Undamaged", paste("plants"))), 
					 		 expression(atop("Damaged", paste("plants")))))
plant.plot

#EXPORT PLOT
tiff('plant.tiff', units="in", width=5, height=5, res=400)
plant.plot
dev.off()

#Q2b: dispensers 
#indole
indole10$treatment[indole10$treatment=="Dispenser"]="Indole"

indole.plot<-ggplot(data=indole10, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA, width=.5, lwd=1)+
	geom_point(position=position_jitter(width = 0.1), alpha=0.4, size=3, color="#810f7c")+
	theme_classic()+
	labs(x=" ", y="")+
	theme(text = element_text(size=16), axis.text.x = element_text(size = 15))+
	scale_x_discrete(labels=c(expression(atop("Control", paste("dispenser"))), 
					 		 expression(atop("Indole", paste("dispenser")))))
indole.plot

#EXPORT PLOT
#tiff('indole.tiff', units="in", width=6, height=5, res=400)
#indole.plot
#dev.off()

#farnesene
farn10$treatment[farn10$treatment=="Dispenser"]="Farnesene"

farn.plot<-ggplot(data=farn10, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA, width=.5, lwd=1)+
	geom_point(position=position_jitter(width = 0.1), alpha=0.4, size=3, color="#810f7c")+
	theme_classic()+
	labs(x=" ", y="")+
	theme(text = element_text(size=16), axis.text.x = element_text(size = 15))+
	scale_x_discrete(labels=c(expression(atop("Control", paste("dispenser"))), 
							  expression(atop("Farnesene", paste("dispenser")))))
farn.plot

#EXPORT PLOT
#tiff('farnesene.tiff', units="in", width=6, height=5, res=400)
#farn.plot
#dev.off()


#tiff('Dispersers.tiff', units="in", width=8, height=4, res=300)
#ggarrange(plant.plot, indole.plot, farn.plot, 
		  #labels = c("a", "b"),heights = c(2, 2),
		  #ncol = 2, nrow = 1)
#dev.off()

tiff('HIPV.tiff', units="in", width=10, height=4, res=300)
ggarrange(plant.plot, indole.plot, farn.plot, 
		  labels = c("a", "b", "c"),heights = c(2, 2,2),
		  ncol = 3, nrow = 1,
		  vjust=1.2)
dev.off()

summary(davg1)
exp(-0.4972505)#intercept, 0.61
exp(0.4972505)#or intercept= -1.64??
exp(0.0209956)#ar term slope, 1.02
exp(0.0902396)#temperature slope, 1.09
exp(0.8317915)#linear wind slope, 2.30
exp(0.0001986)#delta air slope, 1.00

#Q3
#ar term
ar.plot<-bat.met.hour%>%
	ggplot(aes(x=act2, 
			   y=activity))+
	geom_point(alpha=0.4, size=2,color="#810f7c")+
	theme_classic()+
	labs(x="Autoregressive term",
		 y="Bat activity (avg hourly passes)")+
	theme(text = element_text(size = 12))+
	scale_y_continuous(limits = c(0,180))+ 
	geom_abline(slope=1.021218, intercept=-1.644194, color="black",
				size=1.5)
ar.plot

#EXPORT PLOT
tiff('autoregressive.tiff', units="in", width=4.25, height=2.9, res=400)
ar.plot
dev.off()

#temperature, linear
bat.met.hour%>%
	ggplot(aes(x=Air_Temperature_C, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	theme_classic()+
	labs(x="Average air temperature (ºC)",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))+
	geom_smooth(method = "glm", color="black")

#temperature, controlling the slope
temp.plot<-bat.met.hour%>%
	ggplot(aes(x=Air_Temperature_C, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	theme_classic()+
	labs(x="Average air temperature (ºC)",
		 y="Bat activity (avg hourly passes)")+
	theme(text = element_text(size = 15),
		  axis.title.y = element_text(size=13))+ 
	geom_abline(slope=1.09, intercept=-1.644194, color="black",
				size=1.5)+
	stat_smooth(aes(ymin = after_stat(y - 2 * se), ymax = after_stat(y + 2 * se)), 
				geom = "ribbon",  fill = "grey60", alpha = .4, method = "glm")
	
temp.plot
#geom_smooth(method = "glm", color="black")+

#EXPORT PLOT
tiff('temp.tiff', units="in", width=5, height=3, res=400)
temp.plot
dev.off()

#wind non-linear
wind2.plot<-bat.met.hour%>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", formula = y ~ x + I(x^2), color="black", se=F,
				fullrange = T, size=1.5)+
	theme_classic()+
	labs(x="Average wind speed (m/s)",
		 y="")+
	theme(text = element_text(size = 15))
wind2.plot

#EXPORT PLOT
tiff('wind.tiff', units="in", width=5, height=3, res=400)
wind2.plot
dev.off()

tiff('Abiotic.tiff', units="in", width=8, height=3.5, res=300)
ggarrange(temp.plot, wind2.plot, 
labels = c("a", "b"),heights = c(2, 2),
ncol = 2, nrow = 1)
dev.off()


#air pressure
delta.air.plot<-bat.met.hour%>%
	ggplot(aes(x=delta.air2, 
			   y=activity))+
	geom_point(alpha=0.4, size=2,color="#810f7c")+
	theme_classic()+
	labs(x="Average change in air pressure (Pa)",
		 y="Bat activity (avg hourly passes)")+
	theme(text = element_text(size = 12))
delta.air.plot

#EXPORT PLOT
tiff('delta_air.tiff', units="in", width=4.25, height=2.9, res=400)
delta.air.plot
dev.off()

exp(0.01357)#slope=-1.01
exp(3.49)#intercept=32.8
#consecutive rain hours

rain.plot<-rain.bat.hour%>%
	ggplot(aes(x=rain_con_hours, 
			   y=activity))+
	geom_point(alpha=0.4, size=2,color="#810f7c")+
	theme_classic()+
	labs(x="Consecutive rain (hours)",
		 y="Bat activity (avg hourly passes)")+
	theme(text = element_text(size = 12))
rain.plot

#EXPORT PLOT
tiff('rain.tiff', units="in", width=4.25, height=2.9, res=400)
rain.plot
dev.off()


##These are my suggested plots for Q3, if you go with the GAM

#temperature, linear
bat.met.hour%>%
	ggplot(aes(x=Air_Temperature_C, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	theme_classic()+
	labs(x="Average air temperature (ºC)",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))+
	geom_smooth(method = "glm", color="black")

#wind non-linear
bat.met.hour%>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1.5, color="black")+
	theme_classic()+
	labs(x="Average wind speed (m/s)",
		 y="")+
	theme(text = element_text(size = 15))


#OR, if you keep the polynomial, I would use this one for wind, it is the 
#same as what you have above, but with the SE

#wind non-linear
bat.met.hour%>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", formula = y ~ x + I(x^2), color="black", se=T,
				fullrange = T, size=1.5)+
	theme_classic()+
	labs(x="Average wind speed (m/s)",
		 y="")+
	theme(text = element_text(size = 15))


ciVal <- 0.5
myMax = max(c(bat.met.hour$Air_Temperature_C,bat.met.hour$activity))
myMin = min(c(bat.met.hour$Air_Temperature_C,bat.met.hour$activity))

