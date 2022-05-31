#Load libraries
library(dplyr)
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

#library(cowplot)

#Note: because you need data from Q2 to answer Q1, they are in that order
#Q2a, Q2b, Q1, Q3
#all plots and end of script

##Q2a: Naturally occurring soybean HIPVS (damaged vs. undamaged plants)----

# Data import and cleaning
plant <- read.csv(file="plant_sum_noTABR.csv",head=TRUE)
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

#add 0s to the beginning of each night
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

#jdate 238-252, 255-270 (no detectors out nights of 253&254)
met1 <- met%>% filter( between(jdate, 238, 252))
met2 <- met%>% filter( between(jdate, 255, 270))
met3<-rbind(met1,met2)

#filtering for hours bats were active, recorders active (6p-7a)
met4<-met3%>%filter(between(hour, 0,6))
met5<-met3%>%filter(between(hour, 19,23))
met6<-rbind(met4,met5)

#aggregate data so it's hourly
#most variables averaged
met.hour.avg<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,
						  delta.air,Air_Pressure_pascal,
						  Air_Temperature_C, Relative_Humidity_pct)~hour + jdate, dat=met6, FUN=mean)
#rain variables summed
met.hour.sum<-aggregate(cbind(Rain_Accumulation_mm,Rain_Duration_s)~hour + jdate, dat=met6, FUN=sum)

#combine the two
met.hour<-cbind(met.hour.avg,met.hour.sum)
#remove duplicate columns
met.hour<-met.hour[,-c(9,10)]


#ordering data
met.hour<-met.hour[order(met.hour$jdate, met.hour$hour),]
#creating hourly change in air pressure var
met.hour$air2<-lag(met.hour$Air_Pressure_pascal, k=1)
met.hour$delta.air2<-met.hour$Air_Pressure_pascal-met.hour$air2

#creating log-transformed variable for rain
met.hour$rain.log<-log((met.hour$Rain_Duration_s)+0.01)

met.hour[is.na(met.hour)]<-0

#creating binary rain variable by the hour
met.hour$rain_binary<-NA
for(i in 1:length(met.hour$Rain_Duration_s)){
	if(met.hour$Rain_Duration_s[i]==0){met.hour$rain_binary[i]="0"}
	if(met.hour$Rain_Duration_s[i]>0){met.hour$rain_binary[i]="1"}
}

library(data.table)
#creating cumulative rain value
setDT(met.hour)[, rain_cum_hours := cumsum(rain_binary), by = rleid(rain_binary == 0)]

##CHECKING FOR COLINEARITY----
met1<-met.hour[,c(5:13)]

M1 <- cor(met1)#correlation matrix
corrplot(M1, method = "circle")
corrplot(M1, method = "number")
#need to choose between rain and wind variables
#wind avg, rain duration

bat.met.hour<-merge(bat.hour.all1, met.hour, by=c("jdate","hour"))

bat.met.hour$wind.avg2 <- (as.numeric(bat.met.hour$Wind_speed_avg_m.s))^2

library(MASS)
#bat.met.hour[is.na(bat.met.hour)]<-0
mod1<-glm(activity~wind.avg2+rain.log+Air_Pressure_pascal+ Air_Temperature_C + 
		  	delta.air + act2 + Wind_speed_avg_m.s + wind.avg2 + rain_cum_hours, dat = bat.met.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod1)

#dredging and model averaging
d1<-dredge(mod1)
davg1<-model.avg(d1, subset=delta<2)
summary(davg1)


exp(8.995e-02)
#bat activity increases 1.1 with every 1 degC increase in air temp
exp(8.320e-01)
#bat activity increases 2.3.1 with every 1 m/s increase in wind speed

summary(lm(bat.met.hour$activity~bat.met.hour$Air_Temperature_C))
#lm estimate=2.9
summary(lm(bat.met.hour$activity~bat.met.hour$Wind_speed_avg_m.s))
#lm estimate=2.8

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
	labs(x=" ", y="Bat activity (log-transformed)")+
	theme(text = element_text(size=20), legend.position = "none", 
		  axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=.9))+
	stat_summary(geom = 'text', label = c("f","d","d","a","e","b","c"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(-0,8.5))+
	scale_x_discrete(limits=c("EPFU/LANO","LABO/LASE", "LACI","MYLU",
							  "NYHU", "PESU", "No ID"))+
	scale_color_manual(values = c("#450757", "#443885", "#2d678e","#178f8b", "#fee800", "#2ab977","#8fd839"))
q1.plot

#EXPORT PLOT
tiff('Q1.tiff', units="in", width=8, height=5, res=400)
q1.plot
dev.off()

dis.all1$sp1 = factor(dis.all1$sp, levels=c("EPFU/LANO","LABO/LASE", "LACI","MYLU",
											"NYHU", "PESU", "No ID"))
q1.sp.plot<-ggplot(data=dis.all1, aes(x=jdate, y=log.act))+ 
	geom_point(alpha=0.4, size=2.5, aes(color=sp1))+
	theme_classic()+
	geom_smooth(method = "gam", color="black")+
	labs(x="Julian date", y="Bat activity (log-transformed)")+
	theme(text = element_text(size=19), 
		  axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=.9))+
	scale_color_manual(values = c("#450757", "#443885", "#2d678e","#178f8b", "#2ab977", "#8fd839","#fee800"),
					   limits=c("EPFU/LANO","LABO/LASE", "LACI","MYLU",
					   		 "NYHU", "PESU", "No ID"))+
	guides(color=guide_legend(title="Species"))
q1.sp.plot

#EXPORT PLOT
tiff('Q1_time.tiff', units="in", width=8, height=5, res=400)
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
	theme(text = element_text(size=20), axis.text.x = element_text(size = 20))+
	scale_x_discrete(limits=c("U", "D"),
					 labels=c("Undamaged", "Damaged"))
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
	labs(x=" ", y="Bat activity (nightly passes)")+
	theme(text = element_text(size=20), axis.text.x = element_text(size = 20))
indole.plot

#EXPORT PLOT
tiff('indole.tiff', units="in", width=6, height=5, res=400)
indole.plot
dev.off()

#farnesene
farn10$treatment[farn10$treatment=="Dispenser"]="Farnesene"

farn.plot<-ggplot(data=farn10, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA, width=.5, lwd=1)+
	geom_point(position=position_jitter(width = 0.1), alpha=0.4, size=3, color="#810f7c")+
	theme_classic()+
	labs(x=" ", y="Bat activity (nightly passes)")+
	theme(text = element_text(size=20), axis.text.x = element_text(size = 20))
farn.plot

#EXPORT PLOT
tiff('farnesene.tiff', units="in", width=6, height=5, res=400)
farn.plot
dev.off()

#Q3
#ar term
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
	theme(text = element_text(size = 13))+ 
	geom_abline(slope=1.094393, intercept=0.2518303, color="black",
				size=1.5, ci=T)
temp.plot
#geom_smooth(method = "glm", color="black")+

#EXPORT PLOT
tiff('temp.tiff', units="in", width=4.5, height=3, res=400)
temp.plot
dev.off()

#wind non-linear
wind2.plot<-bat.met.hour%>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	geom_smooth(method = "glm", formula = y ~ x + I(x^2), color="black")+
	theme_classic()+
	labs(x="Wind speed average (m/s)",
		 y="Bat activity (avg hourly passes)")+
	theme(text = element_text(size = 13))
wind2.plot

#EXPORT PLOT
tiff('wind.tiff', units="in", width=4.5, height=3, res=400)
wind2.plot
dev.off()

#wind, linear
bat.met.hour%>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(alpha=0.4, size=2.5,color="#810f7c")+
	theme_classic()+
	labs(x="Wind speed average (m/s)",
		 y="Bat activity (average hourly passes)")+
	theme(text = element_text(size = 18))+
	geom_smooth(method = "glm", color="black")
