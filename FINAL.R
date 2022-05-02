#Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(car)
library(emmeans)
library(plyr)

library(viridis)


library(multcomp)


library(cowplot)

##Q2a: Naturally occurring soybean HIPVS (damaged vs. undamaged plants)----

# Data import and cleaning ------------------------------------------------

#SUMMARY DATA----
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

# Indole data import and cleaning ------------------------------------------------

#SUMMARY DATA----
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

#PLOTS----

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
