library(lubridate)
library(dplyr)
library(BBmisc)
library(corrplot)
library(lme4)
library(glmmTMB)
library(car)

##SERC met data----
aug <- read.csv(file="SERC_TOWER_aug2021.csv",head=TRUE)
sep <- read.csv(file="SERC_TOWER_sep2021.csv",head=TRUE)
jun <- read.csv(file="SERC_TOWER_june2021.csv",head=TRUE)
jul <- read.csv(file="SERC_TOWER_july2021.csv",head=TRUE)

met<-rbind(aug,sep,jun,jul)

met$date<-as.Date(met$date,)
met$jdate<-NA
met$jdate<-yday(met$date)

#filtering for hours bats were active for dispenser trials, recorders active (7p-7a)
met1<-met%>%filter(between(hour, 0,7))
met2<-met%>%filter(between(hour, 19,23))
met.d<-rbind(met1,met2)


#filtering for hours bats were active for plant trials, recorders active (6p-8a)
met3<-met%>%filter(between(hour, 0,6))
met4<-met%>%filter(between(hour, 20,23))
met.p<-rbind(met3,met4)

#aggregate data so it's nightly averages
met.day.p<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Air_Temperature_C,Relative_Humidity_pct,
						   Rain_Accumulation_mm, Rain_Duration_s)~jdate, dat=met.p, FUN=mean)
met.day.d<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Air_Temperature_C,Relative_Humidity_pct,
						   Rain_Accumulation_mm, Rain_Duration_s)~jdate, dat=met.d, FUN=mean)


## normalize met vars-plants
met.day.p$n.Wind_speed_max_m.s <- normalize(met.day.p$Wind_speed_max_m.s, 
										  method = "range", range = c(0,1))
met.day.p$n.Wind_speed_avg_m.s <- normalize(met.day.p$Wind_speed_avg_m.s, 
										  method = "range", range = c(0,1))
met.day.p$n.Rain_Accumulation_mm <- normalize(met.day.p$Rain_Accumulation_mm, 
											method = "range", range = c(0,1))
met.day.p$n.Rain_Duration_s <- normalize(met.day.p$Rain_Duration_s, 
									   method = "range", range = c(0,1))
met.day.p$n.Air_Temperature_C <- normalize(met.day.p$Air_Temperature_C, 
										 method = "range", range = c(0,1))
met.day.p$n.rh_pct <- normalize(met.day.p$Relative_Humidity_pct, 
							  method = "range", range = c(0,1))

## normalize met vars-dispensers
met.day.d$n.Wind_speed_max_m.s <- normalize(met.day.d$Wind_speed_max_m.s, 
											method = "range", range = c(0,1))
met.day.d$n.Wind_speed_avg_m.s <- normalize(met.day.d$Wind_speed_avg_m.s, 
											method = "range", range = c(0,1))
met.day.d$n.Rain_Accumulation_mm <- normalize(met.day.d$Rain_Accumulation_mm, 
											  method = "range", range = c(0,1))
met.day.d$n.Rain_Duration_s <- normalize(met.day.d$Rain_Duration_s, 
										 method = "range", range = c(0,1))
met.day.d$n.Air_Temperature_C <- normalize(met.day.d$Air_Temperature_C, 
										   method = "range", range = c(0,1))
met.day.d$n.rh_pct <- normalize(met.day.d$Relative_Humidity_pct, 
								method = "range", range = c(0,1))

#creating column for quadratic terms
met.day.d$n.wind.max2 = (as.numeric(met.day.d$n.Wind_speed_max_m.s))^2
met.day.d$n.wind.avg2 = (as.numeric(met.day.d$n.Wind_speed_avg_m.s))^2

##CHECKING FOR COLINEARITY----
corr.d<-met.day.d[,c(8:13)]
corr.p<-met.day.p[,c(8:13)]

D1 <- cor(corr.d)#correlation matrix
corrplot(D1, method = "circle")
corrplot(D1, method = "number")

P1 <- cor(corr.p)#correlation matrix
corrplot(P1, method = "circle")
corrplot(P1, method = "number")
#need to choose between rain and wind variables

#PLANT TRIAL DATA----
plant <- read.csv(file="plant_sum_noTABR.csv",head=TRUE)
plant[, 3:11][is.na(plant[, 3:11])] <- 0

##Gathering data — compounds from col to rows
plant1<-plant %>% gather(sp, activity, EPTFUS:NOID)
plant1$sp<-as.factor(plant1$sp)
levels(plant1$sp)
plant1<-aggregate(activity ~ treatment + sp + trial + jdate, dat=plant1, FUN=sum)

plant10<-aggregate(activity ~ treatment + trial + jdate, dat=plant1, FUN=sum)
#combine bat and met data
bat.met.plant<-merge(met.day.p, plant10, by=c("jdate"))
bat.met.plant$n.wind.max2 = (as.numeric(bat.met.plant$n.Wind_speed_max_m.s))^2

#Test for overdispersion function
overdisp_fun <- function(model) {
	rdf <- df.residual(model)
	rp <- residuals(model,type="pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

test.p<-glmer(activity~((treatment*n.wind.max2)+(treatment*n.rh_pct)+
					  	(treatment*n.Rain_Duration_s)+(treatment*n.Air_Temperature_C)+(1|trial)), 
			  dat = bat.met.plant, family=poisson)
summary(test.p)
overdisp_fun(test.p)#overdispersed

mod.p<-glmer.nb(activity~((treatment*n.wind.max2)+(treatment*n.rh_pct)+
						  	(treatment*n.Rain_Duration_s)+(treatment*n.Air_Temperature_C)+(1|trial)), 
				dat = bat.met.plant)
Anova(mod.p)

mod.p1<-glmer.nb(activity~treatment+n.wind.max2+n.rh_pct+n.Rain_Duration_s+
						   	n.Air_Temperature_C+(1|trial), dat = bat.met.plant)
Anova(mod.p1)

##INDOLE DATA----
indole <- read.csv(file="Maynard_etal_indole_sum.csv",head=TRUE)
indole[, 3:11][is.na(indole[, 3:11])] <- 0

##Gathering data — compounds from col to rows
indole1<-indole %>% gather(sp, activity, EPTFUS:NOID)
indole1$sp<-as.factor(indole1$sp)
levels(indole1$sp)
indole1<-aggregate(activity ~ treatment + sp + jdate + site, dat=indole1, FUN=sum)

#data without species-level
indole10<-aggregate(activity ~ treatment + jdate + site, dat=indole1, FUN=sum)

#combining treatment and met data
indole.met<-merge(indole10, met.day.d, by="jdate")

#treatment univariate model
mod.in<-glmer.nb((activity~treatment+(1|site)), dat = indole.met,
					 na.action="na.fail")
Anova(mod.in)

#weather model
mod.in.weather<-glmer.nb(activity~(n.Air_Temperature_C+n.rh_pct+n.wind.avg2+n.Rain_Duration_s+(1|site)), 
						 dat = met.day.d,
						 na.action="na.fail")
Anova(mod.in.weather)

#prediction plot
indole.met$yhat<-predict(mod.in.weather)
predplot<-ggplot(indole.met)+
	geom_point(aes(x=n.Rain_Duration_s, y=activity))+
	geom_point(aes(x=n.Rain_Duration_s, y=yhat), color="red", size=2)+
	geom_line(aes(x=n.Rain_Duration_s, y=yhat) ,color="red", size=1)
predplot

#interaction pair model
mod.in.met<-glmer.nb(activity~((treatment*n.Air_Temperature_C)+(treatment*n.rh_pct)+
							   	(treatment*n.wind.avg2)+(treatment*n.Rain_Duration_s)+(1|site)), dat = indole.met,
					 na.action="na.fail")
Anova(mod.in.met)

library(MuMIn)
d.in.met<-dredge(mod.in.met)
d.in.met.avg<-model.avg(d.in.met, subset=delta<4)
summary(d.in.met.avg)


#interaction models
mod.in.met.treat.rain<-glmer.nb(activity~((treatment*n.Rain_Duration_s)+(1|site)), 
								dat = indole.met, na.action="na.fail")
mod.in.met.treat.wind2<-glmer.nb(activity~((treatment*n.wind.avg2)+(1|site)), 
								 dat = indole.met, na.action="na.fail")
mod.in.met.treat.temp<-glmer.nb(activity~((treatment*n.Air_Temperature_C)+(1|site)), 
								dat = indole.met, na.action="na.fail")
mod.in.met.treat.rh<-glmer.nb(activity~((treatment*n.rh_pct)+(1|site)), 
							  dat = indole.met, na.action="na.fail")

#univariate models
mod.in.met.treat<-glmer.nb(activity~(treatment+(1|site)), 
						   dat = indole.met, na.action="na.fail")
mod.in.met.rain<-glmer.nb(activity~(n.Rain_Duration_s+(1|site)), 
						  dat = indole.met, na.action="na.fail")
mod.in.met.wind2<-glmer.nb(activity~(n.wind.avg2+(1|site)), 
						   dat = indole.met, na.action="na.fail")
mod.in.met.temp<-glmer.nb(activity~(n.Air_Temperature_C+(1|site)), 
						  dat = indole.met, na.action="na.fail")
mod.in.met.rh<-glmer.nb(activity~(n.rh_pct+(1|site)), 
						dat = indole.met, na.action="na.fail")

mod.in.met.null<-glmer.nb(activity~(1+(1|site)), 
						  dat = indole.met, na.action="na.fail")

#creating tabular output
library(AICcmodavg)
aictab(cand.set=list(mod.in.met,mod.in.met.treat.rain,mod.in.met.treat.wind2,mod.in.met.treat.temp,mod.in.met.treat.rh,
					 mod.in.met.treat,mod.in.met.rain,mod.in.met.temp,mod.in.met.wind2,mod.in.met.rh,mod.in.met.null),
	   modnames=c("global","treat*rain","treat*wind2","treat*temp","treat*rh",
	   		   "treat","rain","temp","wind2","rh","null"))#AIC table

Anova(mod.in.met)

indole.met %>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm.nb", formula = y ~ x + I(x^2))+
	theme_classic()+
	labs(x="Average nightly wind speed (m/s)",
		 y="Bat activity (nightly passes)")

indole.met$rain.dur.min<-NA
indole.met$rain.dur.min<-(indole.met$Rain_Duration_s)/60

indole.met$rain.dur.hr<-NA
indole.met$rain.dur.hr<-((indole.met$Rain_Duration_s)/60)/60

library(MASS)
indole.met %>%
	ggplot(aes(x=rain.dur.min, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm.nb")+
	theme_classic()+
	labs(x="Total nightly rain duration (min)",
		 y="Bat activity (nightly passes)")

##FARNESENE TRIALS----
# Farnesene data import and cleaning ------------------------------------------------

#SUMMARY DATA----
farn <- read.csv(file="Maynard_etal_farnesene_sum.csv",head=TRUE)
farn[, 3:11][is.na(farn[, 3:11])] <- 0

##Gathering data — compounds from col to rows
farn1<-farn %>% gather(sp, activity, EPTFUS:NOID)
farn1$sp<-as.factor(farn1$sp)
levels(farn1$sp)
farn1<-aggregate(activity ~ treatment + sp + jdate + site, dat=farn1, FUN=sum)

#not species-level
farn10<-aggregate(activity ~ treatment + jdate + site, dat=farn1, FUN=sum)

test.farn<-glmer(activity~treatment+(1|site), dat = farn10,family=poisson)
summary(test.farn)
shapiro.test(resid(test.farn))#non-normal
overdisp_fun(test.farn)#overdispersed

mod.farn<-glmer.nb(activity~treatment+(1|site), dat = farn10)
Anova(mod.farn)
#treatment chi=0.398 p=0.5281
