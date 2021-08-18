sum <- read.csv(file="sum.csv",head=TRUE)

library(ggplot2)
library(viridis)
library(reshape2)

df <- melt(sum, id.vars='treat')
df <- df[-c(1:8),]

df$treat<-as.factor(df$treat)

df2 <- melt(sum, id.vars = c('treat', 'date', 'side'))
df2$date<-as.Date.character(df2$date,"%Y%m%d")
df2$treat<-as.character(df2$treat)
df2$value<-as.numeric(df2$value)

df3<-aggregate(value~treat+date+side, df2, FUN="sum") 
df4<-aggregate(value~treat+date, df2, FUN="mean") 

ggplot(data=df, aes(x=treat, y=value))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=variable), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	scale_y_continuous(limits = c(0,450), breaks = c(0,50,100,150,200,250,300,350,400,450))

ggplot(data=df, aes(x=treat, y=value))+ 
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))

ggplot(data=df2, aes(x=date, y=value))+ 
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, aes(color=variable), size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), axis.text.x = element_text(angle=45, hjust=1))+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	facet_wrap(~ treat)

ggplot(data=df2, aes(x=date, y=value))+ 
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")+
	theme(text = element_text(size=18), axis.text.x = element_text(angle=45, hjust=1))+
	scale_color_viridis(discrete = T, option = "D")+
	guides(color=guide_legend(title="Bat spp."))+
	facet_wrap(~ treat)

ggplot(data=df3, aes(x=treat, y=value))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")

ggplot(data=df4, aes(x=treat, y=value))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="black", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Activity (No. recordings)")

df3 %>%
	ggplot(aes(treat,value)) +
	geom_point(aes(shape=side),size=3) +
	geom_line(aes(group = date))

library(lme4)
library(car)
library(multcomp)

g1<-lmer(value~variable+treat+1|date, data = df2)
summary(g1)
Anova(g1)

g2<-glm(value~variable*treat, data = df2)
summary(g2)
Anova(g2)

g.2<-glm(value~variable+treat, data = df2)
summary(glht(g.2, linfct=mcp(variable="Tukey")))
#a lot of big browns

g3<-glm(value~treat+date, data=df3)
summary(g3)
Anova(g3)
