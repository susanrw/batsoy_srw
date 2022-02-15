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

# Indole data import and cleaning ------------------------------------------------

#SUMMARY DATA----
indole <- read.csv(file="Maynard_etal_indole_sum.csv",head=TRUE)
indole[, 3:11][is.na(indole[, 3:11])] <- 0

##Gathering data — compounds from col to rows
indole1<-indole %>% gather(sp, activity, EPTFUS:NOID)
indole1$sp<-as.factor(indole1$sp)
levels(indole1$sp)
indole1<-aggregate(activity ~ treatment + sp + jdate + site, dat=indole1, FUN=sum)

##GROUPING SPECIES----
{indole2<-indole1
indole2$sp=as.character(indole2$sp)
indole2$sp[indole2$sp=="EPTFUS"]="EPFU/LANO"
indole2$sp[indole2$sp=="LASNOC"]="EPFU/LANO"
indole2$sp[indole2$sp=="LASBOR"]="LABO/LASE"
indole2$sp[indole2$sp=="LASSEM"]="LABO/LASE"
indole2$sp[indole2$sp=="LASCIN"]="Other spp."
indole2$sp[indole2$sp=="MYOLUC"]="Other spp."
indole2$sp[indole2$sp=="PERSUB"]="Other spp."
indole2$sp[indole2$sp=="NOID"]="No ID"
indole2$sp[indole2$sp=="NYCHUM"]="Other spp."
}

indole3<-aggregate(activity ~ treatment + sp + jdate + site, dat=indole2, FUN=sum)
indole3$log.act<-log(indole3$activity)
indole3$log.act[which(!is.finite(indole3$log.act))] <- 0

#indole3<-indole3[order(indole3$sp),]

test.i<-glmer(activity~treatment*sp+(1|jdate), dat = indole3,family=poisson)
summary(test.i)
shapiro.test(resid(test.i))#non-normal

#Test for overdispersion function
overdisp_fun <- function(model) {
	rdf <- df.residual(model)
	rp <- residuals(model,type="pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(test.i)#overdispersed

mod.i<-glmer.nb(activity~treatment*sp+(1|jdate), dat = indole3)
Anova(mod.i)
#treatment chi=2.7201 p=0.09909, sp chi=625.2476 p<0.0001, interaxn chi=1.1916 p=0.75502

#contrasts
i1<-emmeans(mod.i,pairwise~sp, type="response")
cld(i1$emmeans,  Letters ='abcde')
#LABO/LASE a, Other b, EPFU/LANO c

indole.tab <- ddply(indole3, c("sp"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
indole.tab

#(EPFU/LANO-NoID)/NoID
(214.517730-72.985816)/72.985816
#1.94, EPFU/LANO were ~2x more active than Other spp.

#(EPFU/LANO-Other)/other
(214.517730-28.078014)/28.078014
#6.64, EPFU/LANO were 6.6x more active than Other spp.

#(EPFU/LANO-LABO)/LABO
(214.517730-8.943262)/8.943262
#22.9865, EPFU/LANO were ~23x more active than LABO/LASE

indole.n <- ddply(indole3, c("site"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
indole.n

indole.tab.treat <- ddply(indole3, c("treatment"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
indole.tab.treat

#full plot, grouped by species
ggplot(data=indole3, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "Indole trials")+
	theme(text = element_text(size=15))

#full plot, with boxplots
indole.plot1<-ggplot(data=indole3, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "Indole trials")+
	theme(text = element_text(size=20))+
	scale_y_continuous(limits = c(0,2400))
indole.plot1

#small plot (means and SEs)
indole.small<-ggplot(data=indole3, aes(x=treatment, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")
indole.small

#plots with inlay
indole.with.inset <-
	ggdraw() +
	draw_plot(indole.plot1) +
	draw_plot(indole.small, x = 0.45, y = .62, width = .5, height = .4)
indole.with.inset

#save plot
ggsave(filename = "indole.png", 
	   plot = indole.with.inset,
	   width = 17, 
	   height = 12,
	   units = "cm",
	   dpi = 300)

#species plot
ggplot(data=indole3, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Indole trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("d","a","c", "b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,2000))

#log-transformed full plot
indole.plot.log<-ggplot(data=indole3, aes(x=treatment, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity (log)",
		 title = "Indole trials")+
	theme(text = element_text(size=20))
indole.plot.log

#log-transformed species plot
ggplot(data=indole3, aes(x=sp, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Indole trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("d","a","c", "d"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,8))



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

##GROUPING SPECIES----
{farn2<-farn1
farn2$sp=as.character(farn2$sp)
farn2$sp[farn2$sp=="EPTFUS"]="EPFU/LANO"
farn2$sp[farn2$sp=="LASNOC"]="EPFU/LANO"
farn2$sp[farn2$sp=="LASBOR"]="LABO/LASE"
farn2$sp[farn2$sp=="LASSEM"]="LABO/LASE"
farn2$sp[farn2$sp=="LASCIN"]="Other spp."
farn2$sp[farn2$sp=="MYOLUC"]="Other spp."
farn2$sp[farn2$sp=="PERSUB"]="Other spp."
farn2$sp[farn2$sp=="NOID"]="No ID"
farn2$sp[farn2$sp=="NYCHUM"]="Other spp."
}

farn3<-aggregate(activity ~ treatment + sp + jdate + site, dat=farn2, FUN=sum)
farn3$log.act<-log(farn3$activity)
farn3$log.act[which(!is.finite(farn3$log.act))] <- 0

test.f<-glmer(activity~treatment*sp+(1|jdate), dat = farn3,family=poisson)
summary(test.f)
shapiro.test(resid(test.f))#non-normal, overdispersed

overdisp_fun(test.f)#overdispersed

mod.f<-glmer.nb(activity~treatment*sp+(1|jdate), dat = farn3)
Anova(mod.f)
#treatment chi=2.7420 p=0.09774, sp chi=67.3332 p<0.001, interaxn chi=3.0187  p=0.38875

#contrasts
f1<-emmeans(mod.f,pairwise~sp, type="response")
cld(f1$emmeans,  Letters ='abcde')
#LABO=A, Other=A EPFU=B, NoID=B


farn.tab <- ddply(farn3, c("sp"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
farn.tab
#(EPFU/LANO-LABO/LASE)/LABO/LASE
(122.0750-26.4625)/26.4625
#3.6, EPFU/LANO are 3.6x more active than LABO/LASE

#(EPFU/LANO-OTHER)/OTHER
(122.0750-40.1500)/40.1500
#2.04, EPFU/LANO are over 2x more active than Other spp.

farn.n <- ddply(farn3, c("site"), summarise,
				  N    = length(activity),
				  mean = mean(activity),
				  sd   = sd(activity),
				  se   = sd / sqrt(N))
farn.n
#N=24/3 species=8 nights total,2 treatments at 10 sites, N=40

#main graph (boxplots w/ data points)
farn.plot1<-ggplot(data=farn3, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "Farnesene")+
	theme(text = element_text(size=20))+
	scale_y_continuous(limits = c(0,1500))
farn.plot1

#small graph (means and SEs)
farn.small<-ggplot(data=farn3, aes(x=treatment, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")
farn.small

#plot with inlay
farn.with.inset <-
	ggdraw() +
	draw_plot(farn.plot1) +
	draw_plot(farn.small, x = 0.45, y = .6, width = .5, height = .4)
farn.with.inset

#save plot
ggsave(filename = "farnesene.png", 
	   plot = farn.with.inset,
	   width = 17, 
	   height = 12,
	   units = "cm",
	   dpi = 300)

#species plot
ggplot(data=farn3, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Farnesene trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("b","a","b","a"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,1400))

#log-transformed plot, main graph
farn.plot.log<-ggplot(data=farn3, aes(x=treatment, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity (log)",
		 title = "Farnesene")+
	theme(text = element_text(size=20))
farn.plot.log

#log-transformed species plot
ggplot(data=farn3, aes(x=sp, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Farnesene trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("b","a","b","a"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,8))

##COMBINING INDOLE AND FARNESENE TRIALS----
indole3$compound<-NA
indole3$compound<-"indole"

farn3$compound<-NA
farn3$compound<-"farnesene"

dis.all <- rbind(farn3, indole3)

#exclude treatment trials
dis.all.control<-dis.all[dis.all$treatment != "Dispenser", ]  

q1<-glmer.nb(activity~sp+(1|site), data = dis.all.control)
Anova(q1)

#contrasts
q1c<-emmeans(q1,pairwise~sp, type="response")
cld(q1c$emmeans,  Letters ='abcde')
#LABO=A, Other=B, NoID=C, EPFU/LANO=D

q1.tab <- ddply(dis.all.control, c("sp"), summarise,
				  N    = length(activity),
				  mean = mean(activity),
				  sd   = sd(activity),
				  se   = sd / sqrt(N))
q1.tab
#(EPFU/LANO-LABO)/LABO
(192.12844-14.80734)/14.80734
#11.98

#(EPFU/LANO-Other)/Other
(192.12844-29.48624)/29.48624
#5.5

#(EPFU/LANO-NoID)/NoID
(192.12844-88.06422)/88.06422
#1.18

#species plot
ggplot(data=dis.all.control, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="All control trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("d","a","c","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,2000))

#log-transformed species plot
ggplot(data=dis.all.control, aes(x=sp, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Farnesene trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("d","a","c","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,8))

##CHORNO PLOT DATA----
#SUMMARY DATA----
forest <- read.csv(file="Maynard_etal_forest_sum.csv",head=TRUE)
forest[, 3:11][is.na(forest[, 3:11])] <- 0

#removing MYOSEP col (all zeros)
forest <- subset(forest, select = -c(9))

##Gathering data — compounds from col to rows
forest1<-forest %>% gather(sp, activity, EPTFUS:NOID)
forest1$sp<-as.factor(forest1$sp)
levels(forest1$sp)
forest1<-aggregate(activity ~ sp + jdate + site + age.class + stand.age, dat=forest1, FUN=sum)

##GROUPING SPECIES----
{forest2<-forest1
forest2$sp=as.character(forest2$sp)
forest2$sp[forest2$sp=="EPTFUS"]="EPFU/LANO"
forest2$sp[forest2$sp=="LASNOC"]="EPFU/LANO"
forest2$sp[forest2$sp=="LASBOR"]="LABO/LASE"
forest2$sp[forest2$sp=="LASSEM"]="LABO/LASE"
forest2$sp[forest2$sp=="LASCIN"]="Other spp."
forest2$sp[forest2$sp=="MYOLUC"]="Other spp."
forest2$sp[forest2$sp=="PERSUB"]="Other spp."
forest2$sp[forest2$sp=="NOID"]="No ID"
forest2$sp[forest2$sp=="NYCHUM"]="Other spp."
forest2$sp[forest2$sp=="MYOSOD"]="Other spp."
}

#one plot had 1 MYOSOD detection, so adding a new species
#Lumping MYOSOD with "Other" 

forest3<-aggregate(activity ~ sp + jdate + site + age.class + stand.age, dat=forest2, FUN=sum)
forest3$log.act<-log(forest3$activity)
forest3$log.act[which(!is.finite(forest3$log.act))] <- 0

for.tab <- ddply(forest3, c("sp"), summarise,
				N    = length(activity),
				mean = mean(activity),
				sd   = sd(activity),
				se   = sd / sqrt(N))
for.tab

#zero-inflated negative binomial
library(glmmTMB)
m1 <- glmmTMB(activity~stand.age*sp+(1|jdate),
			   zi=~stand.age,
			   family=nbinom2, data=forest3)
Anova(m1)
#only species significant

m2 <- glmmTMB(activity~age.class*sp+(1|jdate),
			  zi=~age.class,
			  family=nbinom2, data=forest3)
Anova(m2)
#only species significant

#contrasts
for.con<-emmeans(m2,pairwise~sp, type="response")
cld(for.con$emmeans,  Letters ='abcde')
#LABO=A, Other=B, NoID=B, EPFU/LANO=B
#same results with both models

#species plot
ggplot(data=forest3, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Forest plots")+
	theme(text = element_text(size=20), legend.position = "none",
		  axis.text.x = element_text(angle = 20, vjust = 0.8, hjust = 1))+
	stat_summary(geom = 'text', label = c("b","a","b","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,20))

##Comparing field and forest----
dis.all.control <- dis.all.control[order(dis.all.control$jdate),]
#fitering control field dates 
for.field<-filter(dis.all.control, jdate == "255"|jdate=="256"|jdate=="257"|jdate=="258")
#removing treatment and compound columns
for.field <- subset(for.field, select = -c(1,7))

forest4<-filter(forest3, jdate == "255"|jdate=="256"|jdate=="257"|jdate=="258")
#removing age columns
forest4 <- subset(forest4, select = -c(4:5))

for.field$habitat<-NA
for.field$habitat<-"field"

forest4$habitat<-NA
forest4$habitat<-"forest"

for.field.all <- rbind(for.field, forest4)

#zero-inflated neg binom
m3 <- glmmTMB(activity~habitat*sp+(1|jdate),
			  zi=~sp,
			  family=nbinom2, data=for.field.all)
Anova(m3)
#all significant

m4 <- glmer.nb(activity~habitat*sp+(1|jdate), data=for.field.all)
Anova(m4)
#all significant

#species plot
plot.ff<-ggplot(data=for.field.all, aes(x=habitat, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity (log)")+
	theme(text = element_text(size=20), legend.position = "none",
		  axis.text.x = element_text(angle = 20, vjust = 0.8, hjust = 1))
plot.ff
plot.ff+facet_wrap(~sp)

plot.ff2<-ggplot(data=for.field.all, aes(x=sp, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=habitat))+
	theme_classic()+
	labs(x=" ", y="Relative activity (log)")+
	theme(text = element_text(size=20), legend.position = "none",
		  axis.text.x = element_text(angle = 20, vjust = 0.8, hjust = 1))
plot.ff2
plot.ff2+facet_wrap(~habitat)

ff.sum<-ggplot(data=for.field.all, aes(x=habitat, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")+
	theme(text = element_text(size=20))
ff.sum

ff.tab <- ddply(for.field.all, c("habitat"), summarise,
				 N    = length(activity),
				 mean = mean(activity),
				 sd   = sd(activity),
				 se   = sd / sqrt(N))
ff.tab
#forest mean=1.67, field=215.41
(215.41-1.67)/1.67

#I think these are only coming out as significant because the activity levels were so diff

library(effects)
allEffects(m4)  #this will give you a numerical interpretation, which is again sometimes funky to understand
plot(allEffects(m4)) #This will give you a visual representation of your interactions, which is my preferred method. 

##SERC met data----
indole <- read.csv(file="Maynard_etal_indole_sum.csv",head=TRUE)

###BIG BROWN DATA----
##INDOLE BIG BROWN ONLY---
brown.ind <- indole2[which(indole2$sp== 'EPFU/LANO'),]

test3.ind<-glmer(activity~treatment+(1|jdate), dat = brown.ind,family=poisson)
shapiro.test(resid(test3.ind))#normal
summary(test3.ind)#not normal

mod.brown.ind<-glmer.nb(activity~treatment+(1|jdate), dat = brown.ind)
Anova(mod.brown.ind)
#treatment p=0.38

ggplot(data=brown.ind, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "EPFU/LANO only, indole trials")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")

ggplot(data=brown.ind, aes(x=treatment, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "EPFU/LANO only, indole trials")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")

##farn BIG BROWN ONLY---
brown.farn <- farn2[which(farn2$sp== 'EPFU/LANO'),]

test3.farn<-glmer(activity~treatment+(1|jdate), dat = brown.farn,family=poisson)
shapiro.test(resid(test3.farn))#normal
summary(test3.farn)#not normal

mod.brown.farn<-glmer.nb(activity~treatment+(1|jdate), dat = brown.farn)
Anova(mod.brown.farn)
#treatment p=0.17

ggplot(data=brown.farn, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	stat_summary(fun.data = "mean_se", colour="red", size=1.5, shape="diamond")+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "EPFU/LANO only, Farnesene trials")+
	theme(text = element_text(size=15), legend.title = )+
	scale_color_viridis(discrete = T, option = "D")

ggplot(data=brown.farn, aes(x=treatment, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "EPFU/LANO only, Farnesene trials")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")


