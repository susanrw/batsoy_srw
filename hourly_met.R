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
}

#combine datasets
bat.hour <- rbind(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8,ind9,ind10,
				  ind11,ind12,ind13,ind14,ind15,far1,far2,far3,far4,far5,
				  far6,far7,far8,far9,far10)

#remove noise files
bat.hour<-bat.hour[bat.hour$AUTO.ID. != "Noise", ]  

#select columns
bat.hour<-select(bat.hour, TIME, HOUR, DATE.12, AUTO.ID., FILES, site)

#bat.hour$DATE.12 <- format(as.Date(bat.hour$DATE.12, format = "%m/%d/%y"), "%m-%d-%y")

#insert jdate
bat.hour$jdate<-NA
library(lubridate)
bat.hour$jdate<-yday(bat.hour$DATE.12)

#renaming species column
colnames(bat.hour)[4] <- "sp"

##GROUPING SPECIES----
{bat.hour2<-bat.hour
bat.hour2$sp=as.character(bat.hour2$sp)
bat.hour2$sp[bat.hour2$sp=="EPTFUS"]="EPFU/LANO"
bat.hour2$sp[bat.hour2$sp=="LASNOC"]="EPFU/LANO"
bat.hour2$sp[bat.hour2$sp=="LASBOR"]="LABO/LASE"
bat.hour2$sp[bat.hour2$sp=="LASSEM"]="LABO/LASE"
bat.hour2$sp[bat.hour2$sp=="LASCIN"]="Other spp."
bat.hour2$sp[bat.hour2$sp=="MYOLUC"]="Other spp."
bat.hour2$sp[bat.hour2$sp=="PERSUB"]="Other spp."
bat.hour2$sp[bat.hour2$sp=="NOID"]="No ID"
bat.hour2$sp[bat.hour2$sp=="NYCHUM"]="Other spp."
bat.hour2$sp[bat.hour2$sp=="MYOSEP"]="Other spp."
bat.hour2$sp[bat.hour2$sp=="MYOSOD"]="Other spp."
}

bat.hour3<-aggregate(FILES ~ jdate + sp + site + HOUR, data=bat.hour2, FUN=sum)

#renaming activity column
colnames(bat.hour3)[5] <- "activity"

ddply(bat.hour3, c("HOUR"), summarise,
	  N    = length(activity),
	  mean = mean(activity),
	  sd   = sd(activity),
	  se   = sd / sqrt(N))

##SERC met data----
aug <- read.csv(file="SERC_TOWER_aug2021.csv",head=TRUE)
sep <- read.csv(file="SERC_TOWER_sep2021.csv",head=TRUE)
jun <- read.csv(file="SERC_TOWER_june2021.csv",head=TRUE)
jul <- read.csv(file="SERC_TOWER_july2021.csv",head=TRUE)

met<-rbind(aug,sep,jun,jul)

met$date<-as.Date(met$date,)
met$jdate<-NA
library(lubridate)
met$jdate<-yday(met$date)

#control field trials 238-252, 255-270 (no detectors out nights of 253&254)
met1 <- met%>% filter( between(jdate, 238, 252))
met2 <- met%>% filter( between(jdate, 255, 270))
met3<-rbind(met1,met2)

#filtering for hours bats were active, recorders active (6p-7a)
met4<-met3%>%filter(between(hour, 0,7))
met5<-met3%>%filter(between(hour, 18,23))

met.hour<-rbind(met4,met5)

#aggregate data so it's daily
met.day<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Rain_Accumulation_mm,
					  Rain_Duration_s,delta.air,Air_Pressure_pascal)~jdate, dat=met.hour, FUN=mean)

abat<-cbind(met.hour,bat.hour3)

field.control<-aggregate(activity ~ jdate, dat=dis.all.control, FUN=sum)
bat2<-cbind(met.day,field.control)
#removing second jdate col
bat2 <- subset(bat2, select = -c(1))

#figure this out
library(BBmisc)
bat2<-normalize(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Rain_Accumulation_mm,
					  Rain_Duration_s,delta.air,Air_Pressure_pascal), method = "standardize", 
				range = c(0, 1))

#analysis?
mod.a<-glmer.nb(activity~Wind_speed_max_m.s+Wind_speed_avg_m.s+Rain_Accumulation_mm+
					Rain_Duration_s+delta.air+(1|jdate), dat = bat2)
Anova(mod.a)

#graphs
bat2 %>%
	ggplot(aes(x=delta.air, 
			   y=activity))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	theme_classic()

bat2 %>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	theme_classic()

bat2 %>%
	ggplot(aes(x=Rain_Accumulation_mm, 
			   y=activity))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	theme_classic()

bat2 %>%
	ggplot(aes(x=Rain_Duration_s, 
			   y=activity))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	theme_classic()

bat2 %>%
	ggplot(aes(x=Wind_speed_max_m.s, 
			   y=activity))+
	geom_point(aes(color=jdate))+
	geom_smooth(method = "gam")+
	theme_classic()

###start here---
met4 <- met%>% filter( between(jdate, 238, 270))


#summarizing data
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