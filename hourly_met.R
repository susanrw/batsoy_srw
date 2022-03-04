
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

#control field trials 238-244, 255-270 (I have data for inbetween for failed indole, will need to work on extracting)
met1 <- met%>% filter( between(jdate, 238, 244))
met2 <- met%>% filter( between(jdate, 255, 270))
met3<-rbind(met1,met2)
#aggregate data so it's daily
met6<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Rain_Accumulation_mm,
					  Rain_Duration_s,delta.air,Air_Pressure_pascal)~jdate, dat=met3, FUN=mean)

field.control<-aggregate(activity ~ jdate, dat=dis.all.control, FUN=sum)
bat2<-cbind(met6,field.control)
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