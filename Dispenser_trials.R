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
indole2$sp[indole2$sp=="LASCIN"]="Other bat spp."
indole2$sp[indole2$sp=="MYOLUC"]="Other bat spp."
indole2$sp[indole2$sp=="PERSUB"]="Other bat spp."
indole2$sp[indole2$sp=="NOID"]="Other bat spp."
indole2$sp[indole2$sp=="NYCHUM"]="Other bat spp."
}

indole3<-aggregate(activity ~ treatment + sp + jdate + site, dat=indole2, FUN=sum)

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
#treatment p=0.32, sp p<0.0001, interaxn p=0.45

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
#(EPFU/LANO-Other)/other
(216.0500-101.0638)/101.0638
#1.14, EPFU/LANO were 114% more active than Other spp.

#(EPFU/LANO-LABO)/LABO
(216.0500-9.7000)/9.7000
#21.27, EPFU/LANO were 1467% more active than LABO/LASE

indole.tab.treat <- ddply(indole3, c("treatment"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
indole.tab.treat

#full plot
ggplot(data=indole3, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "Indole trials")+
	theme(text = element_text(size=15))

#data plot w/o species color
indole.plot1<-ggplot(data=indole3, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "Indole trials")+
	theme(text = element_text(size=20))+
	scale_y_continuous(limits = c(0,2400))
indole.plot1

#small plot
indole.small<-ggplot(data=indole3, aes(x=treatment, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")
indole.small

indole.with.inset <-
	ggdraw() +
	draw_plot(indole.plot1) +
	draw_plot(indole.small, x = 0.45, y = .62, width = .5, height = .4)
indole.with.inset

ggsave(filename = "indole.png", 
	   plot = indole.with.inset,
	   width = 17, 
	   height = 12,
	   units = "cm",
	   dpi = 300)

ggplot(data=indole3, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Indole trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("c","a","b"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,1600))

##INDOLE BIG BROWN ONLY----
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

##FARNESENE TRIALS----
# Farnesene data import and cleaning ------------------------------------------------

#SUMMARY DATA----
farn <- read.csv(file="Maynard_etal_farnesene_sum.csv",head=TRUE)

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
farn2$sp[farn2$sp=="LASCIN"]="Other bat spp."
farn2$sp[farn2$sp=="MYOLUC"]="Other bat spp."
farn2$sp[farn2$sp=="PERSUB"]="Other bat spp."
farn2$sp[farn2$sp=="NOID"]="Other bat spp."
farn2$sp[farn2$sp=="NYCHUM"]="Other bat spp."
}

farn3<-aggregate(activity ~ treatment + sp + jdate + site, dat=farn2, FUN=sum)

test.f<-glmer(activity~treatment*sp+(1|jdate), dat = farn3,family=poisson)
summary(test.f)
shapiro.test(resid(test.f))#non-normal, overdispersed

overdisp_fun(test.f)#overdispersed

mod.f<-glmer.nb(activity~treatment*sp+(1|jdate), dat = farn3)
Anova(mod.f)
#treatment p=0.3102, sp p<2e-16, interaxn p=0.2219

#contrasts
f1<-emmeans(mod.f,pairwise~sp, type="response")
cld(f1$emmeans,  Letters ='abcde')


farn.tab <- ddply(farn3, c("sp"), summarise,
					N    = length(activity),
					mean = mean(activity),
					sd   = sd(activity),
					se   = sd / sqrt(N))
farn.tab
#(Other-EPFU/LANO)/EPFU/LANO
(152.81250-125.20513)/125.20513
#.22

#(Other-LABO)/LABO
(152.81250-28.22667)/28.22667
#4.41

farn.plot1<-ggplot(data=farn3, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity",
		 title = "Farnesene")+
	theme(text = element_text(size=20))+
	scale_y_continuous(limits = c(0,1400))
farn.plot1

farn.small<-ggplot(data=farn3, aes(x=treatment, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")
farn.small

library(ggpubr)
ggarrange(farn.plot1, farn.small,
		  labels = c("a", "b"),heights = c(2,2),
		  ncol = 1, nrow = 2)

farn.with.inset <-
	ggdraw() +
	draw_plot(farn.plot1) +
	draw_plot(farn.small, x = 0.45, y = .6, width = .5, height = .4)
farn.with.inset

ggsave(filename = "farnesene.png", 
	   plot = farn.with.inset,
	   width = 17, 
	   height = 12,
	   units = "cm",
	   dpi = 300)

ggplot(data=farn3, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity", title="Farnesene trials")+
	theme(text = element_text(size=20), legend.position = "none")+
	stat_summary(geom = 'text', label = c("b","a","c"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,900))

##farn BIG BROWN ONLY----
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
