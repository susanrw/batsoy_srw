bean <- read.csv(file="beanDIP_activity.csv",head=TRUE)

head(bean)

library(dplyr)
library(ggplot2)
library(viridis)
library(colorbrewer)

bean$date<-as.Date.character(bean$date,"%Y%m%d")

bean %>%
	ggplot(aes(x=date, 
			   y=total.activity,
			   color=site))+
	geom_point()+
	geom_line()+
	labs(x="Date",
		 y="Total activity (no. detections)",
		 color="Site")+
	theme_classic()
#	

#read in all data
{ wy1 <- read.csv(file="id_Wye1.csv",head=TRUE)
	wy2 <- read.csv(file="id_Wye2.csv",head=TRUE)
	wy3 <- read.csv(file="id_Wye3.csv",head=TRUE)
	wy4 <- read.csv(file="id_Wye4.csv",head=TRUE)
	wy5 <- read.csv(file="id_Wye5.csv",head=TRUE)
	wy6 <- read.csv(file="id_Wye6.csv",head=TRUE)
	cv1 <- read.csv(file="id_Cv1.csv",head=TRUE)
	cv2 <- read.csv(file="id_Cv2.csv",head=TRUE)
	cv3 <- read.csv(file="id_Cv3.csv",head=TRUE)
	cv4 <- read.csv(file="id_Cv4.csv",head=TRUE)
	cv5 <- read.csv(file="id_Cv5.csv",head=TRUE)
	cv6 <- read.csv(file="id_Cv6.csv",head=TRUE)}

#Wye data
wye.bean <- rbind(wy1, wy2, wy3, wy4, wy5, wy6)
#Cville data
cv.bean <- rbind(cv1, cv2, cv3, cv4, cv5, cv6)

#creating site columns
wye.bean$site <- NA
for(i in 1:length(wye.bean$FILES)){
	if(wye.bean$FILES[i]==1){wye.bean$site[i]="Wye"}
}

cv.bean$site <- NA
for(i in 1:length(cv.bean$FILES)){
	if(cv.bean$FILES[i]==1){cv.bean$site[i]="Clarksville"}
}

#removing noise files
cv.bean<-cv.bean[cv.bean$AUTO.ID. != "Noise", ] 
wye.bean<-wye.bean[wye.bean$AUTO.ID. != "Noise", ] 

#selecting columns (need dplyr loaded)
cv.bean<-select(cv.bean, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, site)
wye.bean<-select(wye.bean, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, site)

#formatting Cville
{	cv.bean$AUTO.ID.<-as.factor(cv.bean$AUTO.ID.)
	cv.bean$DATE.12<-as.Date(cv.bean$DATE.12,"%Y-%m-%d")
	cv.bean$HOUR<-as.numeric(cv.bean$HOUR)
	cv.bean$DATE<-as.Date(cv.bean$DATE,"%Y-%m-%d")
	cv.bean$HOUR.12<-as.numeric(cv.bean$HOUR.12)
}

#formatting Wye
{	wye.bean$AUTO.ID.<-as.factor(wye.bean$AUTO.ID.)
	wye.bean$DATE.12<-as.Date(wye.bean$DATE.12,"%Y-%m-%d")
	wye.bean$HOUR<-as.numeric(wye.bean$HOUR)
	wye.bean$DATE<-as.Date(wye.bean$DATE,"%Y-%m-%d")
	wye.bean$HOUR.12<-as.numeric(wye.bean$HOUR.12)
}

#aggregating data
wye.bean.agg<-aggregate(FILES ~ DATE.12 + site + AUTO.ID., data=wye.bean, FUN=sum)
cv.bean.agg<-aggregate(FILES ~ DATE.12 + site + AUTO.ID. , data=cv.bean, FUN=sum)

#Wye plot
plot.w1<-wye.bean.agg %>%
	ggplot(aes(x=DATE.12, 
			   y=FILES,
			   color=AUTO.ID.))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 color="Species",
		 title = "Wye")+
	theme_classic()
plot.w1
plot.w1+facet_wrap(~AUTO.ID.)

wye.bean.agg %>%
	ggplot(aes(x=DATE.12, 
			   y=FILES))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 title = "Wye")+
	theme_classic()

#Cville plot
plot.c1<-cv.bean.agg %>%
	ggplot(aes(x=DATE.12, 
			   y=FILES,
			   color=AUTO.ID.))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 color="Species",
		 title = "Clarksville")+
	theme_classic()
plot.c1
plot.c1+facet_wrap(~AUTO.ID.)

cv.bean.agg %>%
	ggplot(aes(x=DATE.12, 
			   y=FILES))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 color="Site")+
	theme_classic()

#aggregating data by night/hour
wye.bean.agg.h<-aggregate(FILES ~ DATE.12 + site + AUTO.ID. + HOUR.12, data=wye.bean, FUN=sum)
cv.bean.agg.h<-aggregate(FILES ~ DATE.12 + site + AUTO.ID. + HOUR.12, data=cv.bean, FUN=sum)

#Wye plot-night
plot.w2<-wye.bean.agg.h %>%
	ggplot(aes(x=HOUR.12,y=FILES))+
	geom_point(aes(color=DATE.12))+
	theme_classic()+
	labs(x="HOUR",
		 y="Relative activity (no. nightly detections)",
		 color="Date", title = "Wye")
plot.w2
plot.w2+geom_smooth(aes(group=DATE.12))
plot.w2+geom_smooth()
plot.w2+geom_smooth()+
	facet_wrap(~AUTO.ID.)

#Cville plot-night
plot.c2<-cv.bean.agg.h %>%
	ggplot(aes(x=HOUR.12,y=FILES))+
	geom_point(aes(color=DATE.12))+
	theme_classic()+
	labs(x="HOUR",
		 y="Relative activity (no. nightly detections)",
		 color="Date", title = "Clarksville")
	
plot.c2
plot.c2+geom_smooth(aes(group=DATE.12))
plot.c2+geom_smooth()
plot.c2+facet_wrap(~AUTO.ID.)
plot.c2+facet_wrap(~AUTO.ID.)+geom_smooth()

#combining into one dataset
all.bean <- rbind(wye.bean, cv.bean)


#formatting
{	all.bean$AUTO.ID.<-as.factor(all.bean$AUTO.ID.)
	all.bean$DATE.12<-as.Date(all.bean$DATE.12,"%Y-%m-%d")
	all.bean$HOUR<-as.numeric(all.bean$HOUR)
	all.bean$DATE<-as.Date(all.bean$DATE,"%Y-%m-%d")
	all.bean$HOUR.12<-as.numeric(all.bean$HOUR.12)
}

all.bean.agg<-aggregate(FILES ~ DATE.12 + site + AUTO.ID., data=all.bean, FUN=sum)

plot.all1<-all.bean.agg %>%
	ggplot(aes(x=DATE.12, 
			   y=FILES,
			   color=site))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Relative activity (no. nightly detections)",
		 color="Site")+
	theme_classic()
plot.all1
plot.all1+facet_wrap(~AUTO.ID.)
plot.all1+facet_wrap(~site)

all.bean.agg.h<-aggregate(FILES ~ DATE.12 + site + AUTO.ID. + HOUR.12, data=all.bean, FUN=sum)

#All plot-night
plot.all2<-all.bean.agg.h %>%
	ggplot(aes(x=HOUR.12,y=FILES))+
	geom_point()+
	theme_classic()+
	labs(x="Hour", y="Relative activity (no. nightly recordings)")
plot.all2
plot.all2+geom_smooth(aes(color=site))
plot.all2+geom_smooth(aes(color=site))+
	facet_wrap(~AUTO.ID.)
plot.all2+geom_smooth()+
	facet_wrap(~site)

plot.all4<-all.bean.agg.h %>%
	ggplot(aes(x=HOUR.12,y=FILES))+
	geom_point(aes(color=AUTO.ID.))+
	theme_classic()+
	labs(x="Hour", y="Relative activity (no. nightly recordings)")
plot.all4
plot.all4+
	facet_wrap(~site)


#aggregating without species
all.bean.agg.h2<-aggregate(FILES ~ DATE.12 + site + HOUR.12, data=all.bean, FUN=sum)

#All plot-night
plot.all3<-all.bean.agg.h2 %>%
	ggplot(aes(x=HOUR.12,y=FILES))+
	geom_point(aes(color=DATE.12))+
	theme_classic()+
	labs(x="Hour", y="Relative activity (no. nightly recordings)")
plot.all3
plot.all3+geom_smooth(aes(group=site))
plot.all3+geom_smooth()+
	facet_wrap(~site)


#HERBIVORY----
chew <- read.csv(file="beanDIP_chew.csv",head=TRUE)

chew$chew_pct<-as.numeric(chew$chew_pct)
chew$chew_prop<-(chew$chew_pct)/100

chew.ag<-aggregate(chew_prop ~ date + site, data=chew, FUN=mean)

chew.ag$chew_prop<-as.numeric(chew.ag$chew_prop)
chew.ag$date<-as.Date(chew.ag$date,"%Y-%m-%d")

chew.ag %>%
	ggplot(aes(x=date, 
			   y=chew_prop,
			   color=site))+
	geom_point()+
	geom_smooth()+
	labs(x="Date",
		 y="Proportion chew damage",
		 color="Site")+
	theme_classic()

#Cville chew + bat
cv.bean.agg$DATE.12<-as.Date(cv.bean.agg$DATE.12,"%Y-%m-%d")

{cv.bat.chew1<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-08-07"), ] 
cv.bat.chew2<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-08-28"), ]
cv.bat.chew3<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-09-03"), ]
cv.bat.chew4<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-07-28"), ]
cv.bat.chew5<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-07-08"), ]}

#Combine
cv.bat <- rbind(cv.bat.chew1, cv.bat.chew2, cv.bat.chew3, cv.bat.chew4, cv.bat.chew5)

cv.bat$date<-cv.bat$DATE.12
cv.bat$date<-as.Date(cv.bat$date,"%Y-%m-%d")
chew.ag$date<-as.Date(chew.ag$date,"%Y-%m-%d")

chew.ag<-chew.ag[order(chew.ag$site),]
cv.chew.ag <- chew.ag[c(1:6),]
wy.chew.ag <- chew.ag[c(7:12),]

cv.bat.chew<-merge(cv.bat, cv.chew.ag, by="date", all = T)


#Wye chew + bat
wye.bean.agg$DATE.12<-as.Date(wye.bean.agg$DATE.12,"%Y-%m-%d")

{wy.bat.chew1<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-08-08"), ] 
	wy.bat.chew2<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-08-28"), ]
	wy.bat.chew3<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-09-03"), ]
	wy.bat.chew4<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-07-27"), ]
	wy.bat.chew5<- cv.bean.agg[cv.bean.agg$DATE.12 == c("2021-07-06"), ]}

#Combine
wy.bat <- rbind(wy.bat.chew1, wy.bat.chew2, wy.bat.chew3, wy.bat.chew4, wy.bat.chew5)

wy.bat$date<-wy.bat$DATE.12
wy.bat$date<-as.Date(wy.bat$date,"%Y-%m-%d")

wy.bat.chew<-merge(wy.bat, wy.chew.ag, by="date", all = T)

all.bat.chew<-rbind(wy.bat.chew, cv.bat.chew)

library(lme4)
library(car)
mod1<-glmer(FILES ~ chew_prop + 1|date, data = cv.bat.chew)
summary(mod1)
Anova(mod1)

mod2<-lm(FILES~AUTO.ID. + chew_prop, data = cv.bat.chew)
summary(mod2)

cv.bat.chew %>%
	ggplot(aes(x=chew_prop, 
			   y=FILES,
			   color=AUTO.ID.))+
	geom_point()+
	geom_smooth(method = "lm")+
	labs(x="Chew damage",
		 y="Bat activity",
		 color="Species")+
	theme_classic()

mod3<-lmer(FILES ~ chew_prop + 1|date, data = wy.bat.chew)
summary(mod3)
Anova(mod3)

mod4<-lm(FILES~AUTO.ID. + chew_prop, data = wy.bat.chew)
summary(mod4)

wy.bat.chew %>%
	ggplot(aes(x=chew_prop, 
			   y=FILES,
			   color=AUTO.ID.))+
	geom_point()+
	labs(x="Chew damage",
		 y="Bat activity",
		 color="Species")+
	theme_classic()+
	scale_x_continuous(limits = c(0.02,0.1))
