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
library(cowplot)

# Data import and cleaning ------------------------------------------------

#SUMMARY DATA----
plant <- read.csv(file="plant_sum_noTABR.csv",head=TRUE)
plant[, 3:11][is.na(plant[, 3:11])] <- 0

##Gathering data — compounds from col to rows
plant1<-plant %>% gather(sp, activity, EPTFUS:NOID)
plant1$sp<-as.factor(plant1$sp)
levels(plant1$sp)
plant1<-aggregate(activity ~ treatment + sp + trial, dat=plant1, FUN=sum)

##GROUPING SPP WITH SIMILAR CALLS----
{plant2<-plant1
plant2$sp=as.character(plant2$sp)
plant2$sp[plant2$sp=="EPTFUS"]="EPFU/LANO"
plant2$sp[plant2$sp=="LASNOC"]="EPFU/LANO"
plant2$sp[plant2$sp=="LASBOR"]="LABO/LASE"
plant2$sp[plant2$sp=="LASSEM"]="LABO/LASE"
plant2$sp[plant2$sp=="LASCIN"]="Other bat spp."
plant2$sp[plant2$sp=="MYOLUC"]="Other bat spp."
plant2$sp[plant2$sp=="PERSUB"]="Other bat spp."
plant2$sp[plant2$sp=="NOID"]="Other bat spp."
plant2$sp[plant2$sp=="NYCHUM"]="Other bat spp."
}

plant3<-aggregate(activity ~ treatment + sp + trial, dat=plant2, FUN=sum)
plant3$log.act<-log(plant3$activity)

#Test for overdispersion function
overdisp_fun <- function(model) {
	rdf <- df.residual(model)
	rp <- residuals(model,type="pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

test1<-glmer(activity~treatment*sp+(1|trial), dat = plant3,family=poisson)
summary(test1)
overdisp_fun(test1)#overdispersed
shapiro.test(resid(test1))#normal

mod.p<-glmer.nb(activity~treatment*sp+(1|trial), dat = plant3)
Anova(mod.p)
#treatment chi=2.9491 p=0.08592, sp chi=31.6798 p<0.0001, interaxn chi=1.0503 p=0.59146

#contrasts
p1<-emmeans(mod.p,pairwise~sp, type="response")
cld(p1$emmeans,  Letters ='abcde')
#LABO/LASE a, EPFU/LANO & Other b

plant3.tab <- ddply(plant3, c("sp"), summarise,
					   N    = length(activity),
					   mean = mean(activity),
					   sd   = sd(activity),
					   se   = sd / sqrt(N))
plant3.tab
#(Other-LABO)/LABO
(283.6-82.0)/82.0
#2.46

#(EPFU/LANO-LABO)/LABO
(262.5-82.0)/82.0
#2.2

#LABO/LASE were 2.2 and 2.5 less active compared to EPFU/LANO and other bat species

plant3.n <- ddply(plant3, c("trial"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
plant3.n

##REMOVING NO IDS----
plant5<-plant1[plant1$sp != "NOID", ]  

{plant4<-plant5
	plant4$sp=as.character(plant4$sp)
	plant4$sp[plant4$sp=="EPTFUS"]="EPFU/LANO"
	plant4$sp[plant4$sp=="LASNOC"]="EPFU/LANO"
	plant4$sp[plant4$sp=="LASBOR"]="LABO/LASE"
	plant4$sp[plant4$sp=="LASSEM"]="LABO/LASE"
	plant4$sp[plant4$sp=="LASCIN"]="Other bat spp."
	plant4$sp[plant4$sp=="MYOLUC"]="Other bat spp."
	plant4$sp[plant4$sp=="PERSUB"]="Other bat spp."
	plant4$sp[plant4$sp=="NYCHUM"]="Other bat spp."
}


test2<-glmer(activity~treatment*sp+(1|trial), dat = plant4,family=poisson)
summary(test2)
shapiro.test(resid(test2))#non-normal, overdispersed

mod.p2<-glmer.nb(activity~treatment*sp+(1|trial), dat = plant4)
Anova(mod.p2)
#treatment p=0.22, sp p<0.001, interaxn p=0.67

#contrasts
p2<-emmeans(mod.p2,pairwise~sp, type="response")
cld(p2$emmeans,  Letters ='abcde')
#Other spp. a, LABO/LASE b, EPFU/LANO c

plant4.tab <- ddply(plant4, c("sp"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
plant4.tab
#(EPFU/LANO-Other)/other
(131.25-18.15)/18.15
#6.23, EPFU/LANO were 623% more active than Other spp.

#(EPFU/LANO-LABO)/LABO
(131.25-41)/41
#2.2, EPFU/LANO were 220% more active than LABO/LASE


##ONLY EPFU/LABO----
brown <- plant2[which(plant2$sp== 'EPFU/LANO'),]

test3<-glmer(activity~treatment+(1|trial), dat = brown,family=poisson)
shapiro.test(resid(test3))#normal
summary(test3)

mod.brown<-glmer.nb(activity~treatment+(1|trial), dat = brown)
Anova(mod.brown)
#treatment p=0.20

#GRAPHS----

#main graph (boxplots w/ data points)
plant.plot1<-ggplot(data=plant3, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	theme(text = element_text(size=18))+
	scale_x_discrete(limits=c("U", "D"),
					 labels=c("Undamaged", "Damaged"))
plant.plot1

#small graph (means and SEs)
plant.small<-ggplot(data=plant3, aes(x=treatment, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")+
	scale_x_discrete(limits=c("U", "D"),
					 labels=c("Undamaged", "Damaged"))
plant.small

#graph with inlay
plant.with.inset <-
	ggdraw() +
	draw_plot(plant.plot1) +
	draw_plot(plant.small, x = 0.45, y = .6, width = .5, height = .4)
plant.with.inset

ggsave(filename = "plant.png", 
	   plot = plant.with.inset,
	   width = 17, 
	   height = 12,
	   units = "cm",
	   dpi = 300)

#species graph
ggplot(data=plant3, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Plant trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("b","a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,900))

#log transformed main graph (boxplots w/ data points)
plant.plot.log<-ggplot(data=plant3, aes(x=treatment, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity (log)")+
	theme(text = element_text(size=18))+
	scale_x_discrete(limits=c("U", "D"),
					 labels=c("Undamaged", "Damaged"))
plant.plot.log

#NO NOID GRAPH----
ggplot(data=plant4, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "Plant trials, excluding NoIDs")+
	theme(text = element_text(size=15), legend.title = )+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))

ggplot(data=plant4, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Plant trials, excluding NoIDs")+
	theme(text = element_text(size=16), legend.position = "none")+
	stat_summary(geom = 'text', label = c("c","b","a"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,900))

#BIG BROWN GRAPH----
ggplot(data=brown, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "EPFU/LANO only, plant trials")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")+
	scale_x_discrete(labels=c("Damaged", "Undamaged"))

