
library(dplyr)
library(lubridate)
library(BBmisc)
library(glmmTMB)

# Hourly bat data import and cleaning ------------------------------------------------


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

#select columns (#can't have MASS loaded when you do this)
bat.hour<-select(bat.hour, TIME, HOUR, DATE, AUTO.ID., FILES, site)

#bat.hour$DATE.12 <- format(as.Date(bat.hour$DATE.12, format = "%m/%d/%y"), "%m-%d-%y")

#insert jdate
bat.hour$jdate<-NA
bat.hour$jdate<-yday(bat.hour$DATE)


#renaming columns
colnames(bat.hour)[2] <- "hour"
colnames(bat.hour)[4] <- "sp"
colnames(bat.hour)[5] <- "activity"


#aggregate so hourly bat data is daily
bat.day<-aggregate(activity ~ jdate, dat=bat.hour, FUN=sum)

#aggregate hourly bat data with all species summed
bat.hour.all<-aggregate(activity ~ jdate + hour+site, dat=bat.hour, FUN=sum)
#and averaged across all sites
bat.hour.all1<-aggregate(activity ~ jdate + hour, dat=bat.hour.all, FUN=mean)

#figure out where the zeros are
#write.csv(bat.hour.all1, "fix3.csv")
#no zeros
#258,259,262 had 7a calls (2,2,1)
#251, 257-262, 264-267 had calls at 1800 


#bat.hour.all1<-rbind(bat.hour.all1, zeros)

bat.hour.all1<-bat.hour.all1[order(bat.hour.all1$jdate, bat.hour.all1$hour),]

library(stats)
acf(bat.hour.all1$activity, type = "correlation")

bat.hour.all1$act2<-lag(bat.hour.all1$activity, k=1)

#add NAs to the beginning of each night
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

met$date<-as.Date(met$date,)
met$jdate<-NA
met$jdate<-yday(met$date)

#control field trials 238-252, 255-270 (no detectors out nights of 253&254)
met1 <- met%>% filter( between(jdate, 238, 252))
met2 <- met%>% filter( between(jdate, 255, 270))
met3<-rbind(met1,met2)

#filtering for hours bats were active, recorders active (6p-7a)
met4<-met3%>%filter(between(hour, 0,6))
met5<-met3%>%filter(between(hour, 19,23))

met6<-rbind(met4,met5)

#aggregate data so it's hourly
met.hour<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,
						  Rain_Accumulation_mm,Rain_Duration_s,
						  delta.air,Air_Pressure_pascal,
						  Air_Temperature_C, Relative_Humidity_pct)~hour + jdate, dat=met6, FUN=mean)
#create log-transformed rain var 

#ordering data
met.hour<-met.hour[order(met.hour$jdate, met.hour$hour),]

#creating hourly change in air pressure var
met.hour$air2<-lag(met.hour$Air_Pressure_pascal, k=1)
met.hour$delta.air2<-met.hour$Air_Pressure_pascal-met.hour$air2

#creating quad term for avg wind and rh
#met.hour$wind.avg2 <- (as.numeric(met.hour$Wind_speed_avg_m.s))^2
#met.hour$rh2 <- (as.numeric(met.hour$Relative_Humidity_pct))^2

met.hour$rain.log<-log((met.hour$Rain_Duration_s)+0.01)

met.hour[is.na(met.hour)]<-0

##CHECKING FOR COLINEARITY----
library(corrplot)
met1<-met.hour[,c(5:13)]

M1 <- cor(met1)#correlation matrix
corrplot(M1, method = "circle")
corrplot(M1, method = "number")
#need to choose between rain and wind variables
#wind avg, rain duration

bat.met.hour<-merge(bat.hour.all1, met.hour, by=c("hour","jdate"))

#write.csv(bat.met.hour, "bat.met.hour.csv")

bat.met.hour$wind.avg2 <- (as.numeric(bat.met.hour$Wind_speed_avg_m.s))^2

library(MASS)
library(car)
#bat.met.hour[is.na(bat.met.hour)]<-0
mod1<-glm(activity~Wind_speed_avg_m.s+rain.log+Air_Pressure_pascal+ Air_Temperature_C + 
		  	delta.air + act2 + Relative_Humidity_pct, dat = bat.met.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod1)#wind, temp, AR
Anova(mod1)
shapiro.test(resid(mod1))#not normal

library(MuMIn)
d1<-dredge(mod1)
davg1<-model.avg(d1, subset=delta<4)
summary(davg1)

summary(lm(bat.met.hour$activity~bat.met.hour$Air_Temperature_C))
plot(bat.met.hour$activity~bat.met.hour$Air_Temperature_C)
summary(lm(bat.met.hour$activity~bat.met.hour$Wind_speed_avg_m.s))

#use estimate and SE to fit linear 

library(dglm)
fit <- dglm(bat.met.hour$activity~1, family=Gamma(link="log"), mustart=mean(bat.met.hour$activity))
summary(fit)

mu <- exp(3.265833)
shape <- exp(0.1464342)
scale <- mu/shape
rate <- 1/scale
c(shape, scale, rate)

library(ggplot2)
#graphs
bat.met.hour%>%
	ggplot(aes(x=act2, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	theme_classic()+
	labs(x="AR term",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))+
	geom_smooth(method = "glm", color="black")+
	scale_y_continuous(limits = c(0,180))
	#stat_function(fun=dgamma, args=list(shape=1.16, rate=1))

bat.met.hour%>%
	ggplot(aes(x=Air_Temperature_C, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", color="black")+
	theme_classic()+
	labs(x="Average air temperature (ºC)",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))

bat.met.hour%>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", color="black")+
	theme_classic()+
	labs(x="Average wind speed (m/s)",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))

bat.met.hour%>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", formula = y ~ x + I(x^2), color="black")+
	theme_classic()+
	labs(x="Wind speed average (m/s)",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))

bat.met.hour%>%
	ggplot(aes(x=delta.air, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", method.args = list(family = "Gamma"),se = F, colour = "black", 
				size = 0.8)+
	theme_classic()+
	labs(x="Average variation in hourly air pressure",
		 y="Bat activity (average hourly passes)")

bat.met.hour%>%
	ggplot(aes(x=rain.log, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", method.args = list(family = "Gamma"),se = F, colour = "black", 
				size = 0.8)+
	theme_classic()+
	labs(x="Average rain duraction (log)",
		 y="Bat activity (average hourly passes)")

bat.met.hour%>%
	ggplot(aes(x=Air_Pressure_pascal, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", method.args = list(family = "Gamma"),se = F, colour = "black", 
				size = 0.8)+
	theme_classic()+
	labs(x="Average hourly air pressure (Pascal)",
		 y="Bat activity (average hourly passes)")

bat.met.hour%>%
	ggplot(aes(x=Relative_Humidity_pct, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", colour = "black")+
	theme_classic()+
	labs(x="Average relative humidity (%)",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))

###other stuff----


## normalize met vars-hourly
met.hour$n.Wind_speed_max_m.s <- normalize(met.hour$Wind_speed_max_m.s, 
										   method = "range", range = c(0,1))
met.hour$n.Wind_speed_avg_m.s <- normalize(met.hour$Wind_speed_avg_m.s, 
										   method = "range", range = c(0,1))
met.hour$n.Rain_Accumulation_mm <- normalize(met.hour$Rain_Accumulation_mm, 
										   method = "range", range = c(0,1))
met.hour$n.Rain_Duration_s <- normalize(met.hour$Rain_Duration_s, 
										   method = "range", range = c(0,1))
met.hour$n.delta.air <- normalize(met.hour$delta.air, 
										   method = "range", range = c(0,1))
met.hour$n.Air_Pressure_pascal <- normalize(met.hour$Air_Pressure_pascal, 
										   method = "range", range = c(0,1))
met.hour$n.Air_Temperature_C <- normalize(met.hour$Air_Temperature_C, 
											method = "range", range = c(0,1))

#aggregate data so it's daily
met.day1<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Air_Temperature_C,Relative_Humidity_pct)~jdate, dat=met6, FUN=mean)
met.day2<-aggregate(cbind(Rain_Accumulation_mm, Rain_Duration_s)~jdate, dat=met6, FUN=sum)

met.day<-cbind(met.day1,met.day2)
#remove second jdate col
met.day<-met.day[,-6]

met.day$Rain_Accumulation_mm<-as.numeric(met.day$Rain_Accumulation_mm)


## normalize met vars-daily
met.day$n.Wind_speed_max_m.s <- normalize(met.day$Wind_speed_max_m.s, 
										   method = "range", range = c(0,1))
met.day$n.Wind_speed_avg_m.s <- normalize(met.day$Wind_speed_avg_m.s, 
										   method = "range", range = c(0,1))
met.day$n.Rain_Accumulation_mm <- normalize(met.day$Rain_Accumulation_mm, 
											 method = "range", range = c(0,1))
met.day$n.Rain_Duration_s <- normalize(met.day$Rain_Duration_s, 
										method = "range", range = c(0,1))
met.day$n.Air_Temperature_C <- normalize(met.day$Air_Temperature_C, 
										  method = "range", range = c(0,1))
met.day$n.rh_pct <- normalize(met.day$Relative_Humidity_pct, 
										 method = "range", range = c(0,1))
met.day$n.Rain_Accumulation_mm<-as.numeric(met.day$n.Rain_Accumulation_mm)




#combine nightly bat and met data
bat.met.night<-merge(met.day, bat.day, by=c("jdate"))
bat.met.night$n.wind.max2 = (as.numeric(bat.met.night$n.Wind_speed_max_m.s))^2

mod.night<-glmer.nb(activity~n.Rain_Accumulation_mm+n.Air_Temperature_C+
						n.wind.max2+(1|site), dat = bat.met.night)
Anova(mod.night)
summary(mod.night)

#combine species
#bat.met.hour.all<-aggregate(activity~jdate+site+hour+Wind_speed_max_m.s+
								#Rain_Duration_s+delta.air+n.Wind_speed_max_m.s+
							#	n.Rain_Duration_s+n.delta.air, data=bat.met.hour, FUN=sum)

zero1 <- read.csv(file="hour_zeros_spsum_indiv.csv",head=TRUE)
zero2 <- read.csv(file="hour_zeros_spsum.csv",head=TRUE)

bat.hour.all.zeros<-rbind(bat.hour.all, zero1,zero2)

bat.met.hour.zeros<-merge(bat.hour.all, met.hour, by=c("hour","jdate"))
bat.met.hour.zeros$log.act<-log(bat.met.hour.zeros$activity)


#creating new variables for hour
bat.met.hour.zeros$n.hour<-NA
{bat.met.hour.zeros<-bat.met.hour.zeros
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="0"]="0.07"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="1"]="0.08"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="2"]="0.09"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="3"]="0.1"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="4"]="0.11"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="5"]="0.12"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="6"]="0.13"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="7"]="0.14"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="18"]="0.01"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="19"]="0.02"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="20"]="0.03"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="21"]="0.04"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="22"]="0.05"
	bat.met.hour.zeros$n.hour[bat.met.hour.zeros$hour=="23"]="0.06"
}

#creating column for quadratic terms
bat.met.hour.zeros$n.wind.max2 = (as.numeric(bat.met.hour.zeros$n.Wind_speed_max_m.s))^2
bat.met.hour.zeros$n.temp = (as.numeric(bat.met.hour.zeros$n.Air_Temperature_C))^2
bat.met.hour.zeros$n.hour<-as.numeric(bat.met.hour.zeros$n.hour)
bat.met.hour.zeros$n.hour2<-(bat.met.hour.zeros$n.hour)^2

library(DHARMa)
mod.hour<-glmmTMB(activity~n.hour+(1|site)+(1|jdate),
				  zi=~n.hour+(1|site)+(1|jdate), 
				  family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
res = simulateResiduals(mod.hour)
res = recalculateResiduals(res, group = bat.met.hour.zeros$n.hour)
testTemporalAutocorrelation(res, time = unique(bat.met.hour.zeros$n.hour))
#hour data is temporally autocorrelated


mod.night<-glmmTMB(activity~jdate+(1|site),
				  zi=~jdate+(1|site), 
				  family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
res1 = simulateResiduals(mod.night)
res1 = recalculateResiduals(res, group = bat.met.hour.zeros$jdate)
testTemporalAutocorrelation(res, time = unique(bat.met.hour.zeros$jdate))
#nightly data is NOT temporally autocorrelated, is independent 

#analysis
library(lme4)
library(car)
mod.a<-glmer.nb(activity~n.Wind_speed_max_m.s+n.Rain_Duration_s+n.delta.air+
					(1|site), dat = bat.met.hour.zeros, na.action="na.fail")
Anova(mod.a)
hist(bat.met.hour.zeros$activity)

mod.global<-glmmTMB(activity~n.delta.air+n.wind.max2+n.Air_Temperature_C+n.Rain_Accumulation_mm+n.hour+
			   	(1|site)+(1|jdate),
			   zi=~n.delta.air+n.wind.max2+n.Air_Temperature_C+n.Rain_Accumulation_mm+n.hour+
			   	(1|site)+(1|jdate), 
			   family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
Anova(mod.global)
summary(mod.global)

mod.air<-glmmTMB(activity~n.delta.air+(1|site)+(1|jdate),
			  zi=~n.delta.air+(1|site)+(1|jdate), 
			  family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
mod.wind<-glmmTMB(activity~n.Wind_speed_max_m.s+(1|site)+(1|jdate),
				   zi=~n.Wind_speed_max_m.s+(1|site)+(1|jdate), 
				   family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
mod.wind2<-glmmTMB(activity~n.wind.max2+(1|site)+(1|jdate),
				 zi=~n.wind.max2+(1|site)+(1|jdate), 
				 family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
mod.rain<-glmmTMB(activity~n.Rain_Accumulation_mm+(1|site)+(1|jdate),
				   zi=~n.Rain_Accumulation_mm+(1|site)+(1|jdate), 
				   family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
mod.temp<-glmmTMB(activity~n.Air_Temperature_C+(1|site)+(1|jdate),
				  zi=~n.Air_Temperature_C+(1|site)+(1|jdate), 
				  family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
mod.hour<-glmmTMB(activity~n.hour+(1|site)+(1|jdate),
				  zi=~n.hour+(1|site)+(1|jdate), 
				  family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
mod.null<-glmmTMB(activity~1+(1|site)+(1|jdate),
				  zi=~1+(1|site)+(1|jdate), 
				  family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
	
AIC(mod.global,mod.air,mod.rain,mod.wind,mod.wind2,mod.temp,mod.hour,mod.null)

#creating tabular output
library(AICcmodavg)
aictab(cand.set=list(mod.global,mod.air,mod.rain,mod.wind,mod.wind2,mod.temp,mod.hour,mod.null),
	   modnames=c("global","air","rain","wind","wind2","temp","hour","null"))#AIC table

mod.temp.hour<-glmmTMB(activity~n.temp+n.hour+(1|site)+(1|jdate),
					   zi=~n.temp+n.hour+(1|site)+(1|jdate), 
					   family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")
mod.air.hour<-glmmTMB(activity~n.delta.air+n.hour+(1|site)+(1|jdate),
					  zi=~n.delta.air+n.hour+(1|site)+(1|jdate), 
					  family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")

mod.air.temp.hour<-glmmTMB(activity~n.delta.air+n.hour+n.temp+(1|site)+(1|jdate),
						   zi=~n.delta.air+n.hour+n.temp+(1|site)+(1|jdate), 
						   family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")

AIC(mod.global,mod.air,mod.rain,mod.wind,mod.wind2,mod.temp,mod.hour,mod.null,mod.temp.hour,mod.air.hour,mod.air.temp.hour)

#creating tabular output
library(AICcmodavg)
aictab(cand.set=list(mod.global,mod.air,mod.rain,mod.wind,mod.wind2,mod.temp,mod.hour,mod.null,
					 mod.temp.hour,mod.air.hour,mod.air.temp.hour),
	   modnames=c("global","air","rain","wind","wind2","temp","hour","null","temp.hour",
	   		   "air.hour", "air.temp.hour"))#AIC table

mod.rain.hour<-glmmTMB(activity~n.Rain_Accumulation_mm+n.hour+(1|site)+(1|jdate),
						   zi=~n.Rain_Accumulation_mm+n.hour+(1|site)+(1|jdate), 
						   family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")

mod.wind2.hour<-glmmTMB(activity~n.wind.max2+n.hour+(1|site)+(1|jdate),
					   zi=~n.wind.max2+n.hour+(1|site)+(1|jdate), 
					   family = nbinom2, data = bat.met.hour.zeros, na.action = "na.fail")

#creating tabular output
library(AICcmodavg)
aictab(cand.set=list(mod.global,mod.air,mod.rain,mod.wind,mod.wind2,mod.temp,mod.hour,mod.null,
					 mod.temp.hour,mod.air.hour,mod.air.temp.hour, mod.rain.hour, mod.wind2.hour),
	   modnames=c("global","air","rain","wind","wind2","temp","hour","null","temp.hour",
	   		   "air.hour", "air.temp.hour", "rain.hour", "wind2.hour"))#AIC table

mod.avg<-model.avg(c=mod.air.hour, mod.hour, mod.rain.hour,mod.wind2.hour)
summary(mod.avg)
#change in air, rain accumulation, and hour have significant negative effects on bat activiy

library(MuMIn)
da<-dredge(mod.b)
davg<-model.avg(da, subset=delta<4)
summary(davg)

library(ggplot2)
#graphs
bat.met.hour.zeros %>%
	ggplot(aes(x=hour, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Change in air pressure (Pa)",
		 y="Bat activity (hourly passes)")


bat.met.hour.zeros %>%
	ggplot(aes(x=Wind_speed_max_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
	theme_classic()+
	labs(x="Wind speed max (m/s)",
		 y="Bat activity (hourly passes)")

bat.met.hour.zeros %>%
	ggplot(aes(x=Wind_speed_max_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Wind speed max (m/s)",
		 y="Bat activity (hourly passes)")

bat.met.hour.zeros %>%
	ggplot(aes(x=Rain_Accumulation_mm, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Rain accumulation (mm)",
		 y="Bat activity (hourly passes)")


bat.met.hour.zeros %>%
	ggplot(aes(x=Air_Temperature_C, 
			   y=activity))+
	geom_point(aes(color=hour))+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Temperature (C)",
		 y="Bat activity (hourly passes)")


#split by species
bat.met.hour.bb <- bat.met.hour[which(bat.met.hour$sp== 'EPFU/LANO'),]
bat.met.hour.other <- bat.met.hour[which(bat.met.hour$sp== 'Other spp.'),]

#big brown analysis
mod.met.bb<-glmer.nb(activity~n.Wind_speed_max_m.s+n.Rain_Duration_s+n.delta.air+
					 	+(1|site), dat = bat.met.hour.bb, na.action="na.fail")
Anova(mod.met.bb)

library(MuMIn)
mod.met.bb.d<-dredge(mod.met.bb)
mod.met.bb.avg<-model.avg(mod.met.bb.d, subset=delta<4)
summary(mod.met.bb.avg)
#change in air pressure and wind gust significant
#Negative relationship between delta air and activity 
#positive relationship in gust and activity

#change in air pressure
bat.met.hour.bb %>%
	ggplot(aes(x=delta.air, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Change in air pressure (Pa)",
		 y="Bat activity (average hourly passes)",
		 title="EPFU/LANO")

#log transformed
bat.met.hour.bb %>%
	ggplot(aes(x=delta.air, 
			   y=log.act))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Change in air pressure (Pa)",
		 y="Bat activity (log-transformed)",
		 title="EPFU/LANO")


#wind gust
bat.met.hour.bb %>%
	ggplot(aes(x=Wind_speed_max_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Wind gust (m/s)",
		 y="Bat activity (average hourly passes)",
		 title="EPFU/LANO")

bat.met.hour.bb %>%
	ggplot(aes(x=Wind_speed_max_m.s, 
			   y=log.act))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Wind gust (m/s)",
		 y="Bat activity (log-transformed)",
		 title="EPFU/LANO")

#other spp analysis
mod.met.other<-glmer.nb(activity~n.Wind_speed_max_m.s+n.Rain_Duration_s+n.delta.air
						+(1|site), dat = bat.met.hour.other, na.action="na.fail")
Anova(mod.met.other)

mod.met.other.d<-dredge(mod.met.other)
mod.met.other.avg<-model.avg(mod.met.other.d, subset=delta<4)
summary(mod.met.other.avg)
#change in air and rain duration sig
#again negative relationship with change in air pressure
#positive relationship with rain duration...

#change in air pressure
bat.met.hour.other %>%
	ggplot(aes(x=delta.air, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Change in air pressure (Pa)",
		 y="Bat activity (average hourly passes)",
		 title="Other spp.")

bat.met.hour.other %>%
	ggplot(aes(x=delta.air, 
			   y=log.act))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Change in air pressure (Pa)",
		 y="Bat activity (log-transformed)",
		 title="Other spp.")

#rain duration
bat.met.hour.other %>%
	ggplot(aes(x=Rain_Duration_s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Rain duration (sec)",
		 y="Bat activity (average hourly passes)",
		 title="Other spp.")

bat.met.hour.other %>%
	ggplot(aes(x=Rain_Duration_s, 
			   y=log.act))+
	geom_smooth(method = "glm")+
	geom_point()+
	theme_classic()+
	labs(x="Rain duration (sec)",
		 y="Bat activity (log-transformed)",
		 title="Other spp.")



#creating column for quadratic term
bat.met.hour.all$n.wind.max2 = (as.numeric(bat.met.hour.all$n.Wind_speed_max_m.s))^2

bat.met.hour.all$log.act<-log(bat.met.hour.all$activity)

mod.met.all<-glmer.nb(activity~n.delta.air+n.wind.max2+n.Rain_Duration_s+
					 	+(1|site), dat = bat.met.hour.all, na.action="na.fail")
Anova(mod.met.all)

library(MuMIn)
mod.met.all.d<-dredge(mod.met.all)
mod.met.all.avg<-model.avg(mod.met.all.d, subset=delta<4)
summary(mod.met.all.avg)

#change in air pressure
#points
bat.met.hour.all %>%
	ggplot(aes(x=delta.air, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Change in air pressure (Pa)",
		 y="Bat activity (average hourly passes)",
		 title="All species, averaged")

#glm, no points
bat.met.hour.all %>%
	ggplot(aes(x=delta.air, 
			   y=activity))+
	geom_smooth(method = "gam")+
	theme_classic()+
	labs(x="Change in air pressure (Pa)",
		 y="Bat activity (average hourly passes)",
		 title="All species, averaged")

#wind gust
#points
bat.met.hour.all %>%
	ggplot(aes(x=n.Wind_speed_max_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "gam")+
	theme_classic()+
	labs(x="Wind gust (standardized)",
		 y="Bat activity",
		 title="All species, averaged")

bat.met.hour.all %>%
	ggplot(aes(x=Wind_speed_max_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
	theme_classic()+
	labs(x="Wind gust (m/s)",
		 y="Bat activity (hourly average passes)",
		 title="All species, averaged—GAM")

bat.met.hour.all %>%
	ggplot(aes(x=Wind_speed_max_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
	theme_classic()+
	labs(x="Wind gust (m/s)",
		 y="Bat activity (hourly average passes)",
		 title="All species, averaged—GLM")

#glm, no points
bat.met.hour.all %>%
	ggplot(aes(x=n.Wind_speed_max_m.s, 
			   y=activity))+
	geom_smooth(method = "glm")+
	theme_classic()


#MET DATA GRAPHS----
#summarizing met data
library(Rmisc)
met.gust.sum<-summarySE(met4, measurevar="Wind_speed_max_m.s", groupvars=c("jdate"))
met.gust.sum %>%
	ggplot(aes(x=jdate, 
			   y=Wind_speed_max_m.s))+
	geom_errorbar(aes(ymin=Wind_speed_max_m.s-se, ymax=Wind_speed_max_m.s+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Wind gust (m/s)")+
	theme_classic()

met.wind.sum<-summarySE(met4, measurevar="Wind_speed_avg_m.s", groupvars=c("jdate"))
met.wind.sum %>%
	ggplot(aes(x=jdate, 
			   y=Wind_speed_avg_m.s))+
	geom_errorbar(aes(ymin=Wind_speed_avg_m.s-se, ymax=Wind_speed_avg_m.s+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Wind average (m/s)")+
	theme_classic()

met4 %>%
	ggplot(aes(x=jdate, 
			   y=Wind_speed_avg_m.s))+
	geom_point()+
	geom_smooth(method = "gam")+
	labs(x="Date (Julian)",
		 y="Wind average (m/s)")+
	theme_classic()

met.rainac.sum<-summarySE(met4, measurevar="Rain_Accumulation_mm", groupvars=c("jdate"))
met.rainac.sum %>%
	ggplot(aes(x=jdate, 
			   y=Rain_Accumulation_mm))+
	geom_errorbar(aes(ymin=Rain_Accumulation_mm-se, ymax=Rain_Accumulation_mm+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Rain accumulation (mm)")+
	theme_classic()

met.raindur.sum<-summarySE(met4, measurevar="Rain_Duration_s", groupvars=c("jdate"))
met.raindur.sum %>%
	ggplot(aes(x=jdate, 
			   y=Rain_Duration_s))+
	geom_errorbar(aes(ymin=Rain_Duration_s-se, ymax=Rain_Duration_s+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Rain duration (sec)")+
	theme_classic()

met.dair.sum<-summarySE(met4, measurevar="delta.air", groupvars=c("jdate"))
met.dair.sum %>%
	ggplot(aes(x=jdate, 
			   y=delta.air))+
	geom_errorbar(aes(ymin=delta.air-se, ymax=delta.air+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Change in air pressure (Pascal)")+
	theme_classic()


dis.all.control %>%
	ggplot(aes(x=jdate, 
			   y=log.act))+
	geom_point(aes(color=sp))+
	geom_smooth(method = "gam")+
	labs(x="Date (Julian)",
		 y="Bat activity")+
	theme_classic()

bat.sum.field<-summarySE(dis.all.control, measurevar="activity", groupvars=c("jdate"))
bat.sum.field %>%
	ggplot(aes(x=jdate, 
			   y=activity))+
	geom_errorbar(aes(ymin=activity-se, ymax=activity+se), width=.1, color="red")+
	geom_line()+
	labs(x="Date (Julian)",
		 y="Bat activity")+
	theme_classic()

#aggregate data so it's hourly and not by minute
met5<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Rain_Accumulation_mm,
					  Rain_Duration_s,delta.air,Air_Pressure_pascal)~jdate+hour, dat=met4, FUN=mean)

#need to re-calculate change in air pressure
met5<-met5[order(met5$jdate),]
met5$change.air<-NA
met5<-mutate(met5, change.air = Air_Pressure_pascal-lag(Air_Pressure_pascal))
met5[is.na(met5)] <- 0

#24-HOUR GRAPHS
met5 %>%
	ggplot(aes(x=hour, 
			   y=change.air))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	labs(x="Time",
		 y="Wind speed max")+
	theme_classic()

met5 %>%
	ggplot(aes(x=hour, 
			   y=Wind_speed_avg_m.s))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	labs(x="Time",
		 y="Wind speed average")+
	theme_classic()

met5 %>%
	ggplot(aes(x=hour, 
			   y=Rain_Accumulation_mm))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	labs(x="Time",
		 y="Rain_Accumulation_mm")+
	theme_classic()

met5 %>%
	ggplot(aes(x=hour, 
			   y=Rain_Duration_s))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	labs(x="Time",
		 y="Rain_Duration_s")+
	theme_classic()

met5 %>%
	ggplot(aes(x=hour, 
			   y=delta.air))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	labs(x="Time",
		 y="Change in air pressure")+
	theme_classic()