bat1 <- read.csv(file="Trial 1.csv",head=TRUE)

library(lme4)

bat1<-bat1[1:899,]

bat1 <- bat1[order(bat1$Treatment),]

{
bat1$AUTO.ID.<-as.factor(bat1$AUTO.ID.)
bat1$DATE.12<-as.factor(bat1$DATE.12)
bat1$Treatment<-as.factor(bat1$Treatment)
bat1$Side<-as.factor(bat1$Side)
bat1$HOUR<-as.numeric(bat1$HOUR)
bat1$Pass<-as.numeric(bat1$Pass) }

bat2<-aggregate(Pass ~ AUTO.ID. + DATE.12 + Treatment + Side + HOUR, data=bat1, FUN=sum)

bat2$Treatment <- factor(bat2$Treatment, levels=c("Reference", "Damaged", "Undamaged"))

mod1<-lmer(Pass~Treatment+AUTO.ID.+1|Side, dat = bat2)
summary(mod1)
shapiro.test(resid(mod1))

hist(bat2$Pass)
shapiro.test(bat2$Pass)

#squareroot transformation
bat2$activity_sqrt<-sqrt(bat2$Pass)
shapiro.test(bat2$activity_sqrt)#didn't help
hist(bat2$activity_sqrt)

#log transformation
bat2$activity_log<-log(bat2$Pass)
shapiro.test(bat2$activity_log)#didn't help
hist(bat2$activity_log)

#cuberoot transformation
bat2$activity_cube<-(bat2$Pass)^(1/3)
shapiro.test(bat2$activity_cube)#didn't help
hist(bat2$activity_cube)

mod2<-glm(Pass~Treatment+AUTO.ID.+HOUR+DATE.12+Side, dat = bat2)
summary(mod2)
shapiro.test(mod2$residuals)

mod3<-glm(Pass~Treatment+AUTO.ID., dat=bat2)
summary(mod3)
shapiro.test(mod3$residuals)

library(multcomp)
summary(glht(mod3, linfct=mcp(Treatment="Tukey")))
summary(glht(mod3, linfct=mcp(AUTO.ID.="Tukey")))
#sig more big browns that LANO, LASE, MYLU, NYHU

library(ggplot2)
library(viridis)
ggplot(data=bat2, aes(x=Treatment, y=Pass))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

#removing reference treatment
bat2 <- bat2[order(bat2$Treatment),]
bat3<-bat2[-c(1:52),]

mod4<-glm(Pass~Treatment+AUTO.ID., dat=bat3)
summary(mod4)

library(multcomp)
summary(glht(mod4, linfct=mcp(AUTO.ID.="Tukey")))
#sig more big browns than LASE, MYLU, NYHU

ggplot(data=bat3, aes(x=Treatment, y=Pass))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=AUTO.ID.), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

library(dplyr)
bat4<-aggregate(Pass ~ DATE.12 + Treatment + Side, data=bat3, FUN=sum)

bat4 %>%
	ggplot(aes(Treatment,Pass)) +
	geom_point(aes(fill=Treatment),size=3) +
	geom_line(aes(group = DATE.12))

bat5<-aggregate(Pass ~ Treatment + AUTO.ID., data=bat3, FUN=sum)

bat5 %>%
	ggplot(aes(Treatment,Pass,color=AUTO.ID.)) +
	geom_point(aes(fill=Treatment),size=3) +
	geom_line(aes(group = AUTO.ID.))

t123 <- read.csv(file="Trials1_5.csv",head=TRUE)

t123$Trial<-as.character(t123$Trial)

t123 %>%
	ggplot(aes(Treatment,Pass)) +
	geom_point(aes(color=Side),size=3) +
	geom_line(aes(group = DATE.12))+
	theme_classic()

t123 %>%
	ggplot(aes(Treatment,Pass)) +
	labs(x="",
		 y="Activity (no. detections)")+
	geom_boxplot()+
	geom_point(aes(color=Trial),size=3)+
	theme_classic()+
	theme(text = element_text(size=18))
	

t123 %>%
	ggplot(aes(Treatment,Pass)) +
	geom_point(aes(color=Trial),size=3) +
	geom_line(aes(group = DATE.12))+
	theme_classic()+
	labs(x="",
		 y="Activity (no. detections)")+
	theme(text = element_text(size=18))

base <- read.csv(file="baseline_activity.csv",head=TRUE)

base$date<-as.Date.character(base$DATE.12,"%Y%m%d")
base$exp<-as.character(base$exp)

base %>%
	ggplot(aes(x=date, 
			   y=Pass,
			   color=Side
			   ))+
	geom_point(aes(shape=exp), size=3)+
	geom_line()+
	labs(x="",
		 y="Total activity (no. detections)",
		 color="Side",
		 shape="Experiment")+
	theme_classic()+
	theme(text = element_text(size=15), legend.title = element_text(size = 12), 
		  legend.text = element_text(size = 10), axis.text.x = element_text(angle=20, hjust=1))+
	scale_x_date(date_breaks = "3 days", date_labels = "%b %d")
	

base %>%
	ggplot(aes(x=date, 
			   y=Pass,
			   color=Side
	))+
	geom_point()+
	geom_line()+
	labs(x="",
		 y="Total activity (no. detections)",
		 color="Side")+
	theme_classic()+
	theme(text = element_text(size=15), legend.title = element_text(size = 12), 
		  legend.text = element_text(size = 10), axis.text.x = element_text(angle=20, hjust=1))+
	scale_x_date(date_breaks = "3 days", date_labels = "%b %d")	

##
