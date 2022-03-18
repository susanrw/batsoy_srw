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

#data without species-level
indole10<-aggregate(activity ~ treatment + jdate + site, dat=indole1, FUN=sum)

#creating column for quadratic terms
met.day$n.wind.max2 = (as.numeric(met.day$n.Wind_speed_max_m.s))^2
met.day$n.wind.avg2 = (as.numeric(met.day$n.Wind_speed_avg_m.s))^2

indole.met<-merge(indole10, met.day, by="jdate")

#interaction pair model
mod.in.met<-glmer.nb(activity~((treatment*n.Air_Temperature_C)+(treatment*n.rh_pct)+
					 	(treatment*n.wind.avg2)+(treatment*n.Rain_Duration_s)+(1|site)), dat = indole.met,
					 na.action="na.fail")
Anova(mod.in.met)

d.in.met<-dredge(mod.in.met)
d.in.met.avg<-model.avg(d.in.met, subset=delta<4)
summary(d.in.met.avg)


#interaction models
mod.in.met.treat.rain<-glmer.nb(activity~((treatment*n.Rain_Duration_s)+(1|site)), 
								dat = indole.met, na.action="na.fail")
mod.in.met.treat.wind2<-glmer.nb(activity~((treatment*n.wind.avg2)+(1|site)), 
								dat = indole.met, na.action="na.fail")
mod.in.met.treat.temp<-glmer.nb(activity~((treatment*n.Air_Temperature_C)+(1|site)), 
								dat = indole.met, na.action="na.fail")
mod.in.met.treat.rh<-glmer.nb(activity~((treatment*n.rh_pct)+(1|site)), 
								dat = indole.met, na.action="na.fail")

#univariate models
mod.in.met.treat<-glmer.nb(activity~(treatment+(1|site)), 
								dat = indole.met, na.action="na.fail")
mod.in.met.rain<-glmer.nb(activity~(n.Rain_Duration_s+(1|site)), 
								dat = indole.met, na.action="na.fail")
mod.in.met.wind2<-glmer.nb(activity~(n.wind.avg2+(1|site)), 
						  dat = indole.met, na.action="na.fail")
mod.in.met.temp<-glmer.nb(activity~(n.Air_Temperature_C+(1|site)), 
						  dat = indole.met, na.action="na.fail")
mod.in.met.rh<-glmer.nb(activity~(n.rh_pct+(1|site)), 
							  dat = indole.met, na.action="na.fail")

mod.in.met.null<-glmer.nb(activity~(1+(1|site)), 
						  dat = indole.met, na.action="na.fail")

#creating tabular output
library(AICcmodavg)
aictab(cand.set=list(mod.in.met,mod.in.met.treat.rain,mod.in.met.treat.wind2,mod.in.met.treat.temp,mod.in.met.treat.rh,
					 mod.in.met.treat,mod.in.met.rain,mod.in.met.temp,mod.in.met.wind2,mod.in.met.rh,mod.in.met.null),
	   modnames=c("global","treat*rain","treat*wind2","treat*temp","treat*rh",
	   		   "treat","rain","temp","wind2","rh","null"))#AIC table

Anova(mod.in.met)

indole.met %>%
	ggplot(aes(x=Wind_speed_avg_m.s, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
	theme_classic()+
	labs(x="Average nightly wind speed (m/s)",
		 y="Bat activity (nightly passes)")

indole.met$rain.dur.min<-NA
indole.met$rain.dur.min<-(indole.met$Rain_Duration_s)/60

indole.met$rain.dur.hr<-NA
indole.met$rain.dur.hr<-((indole.met$Rain_Duration_s)/60)/60

indole.met %>%
	ggplot(aes(x=rain.dur.hr, 
			   y=activity))+
	geom_point()+
	geom_smooth(method = "glm")+
	theme_classic()+
	labs(x="Total nightly rain duration (min)",
		 y="Bat activity (nightly passes)")

#creating log-transformed value for graphs
indole10$log.act<-log(indole10$activity)
indole10$log.act[which(!is.finite(indole10$log.act))] <- 0

#Test for overdispersion function
overdisp_fun <- function(model) {
	rdf <- df.residual(model)
	rp <- residuals(model,type="pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

test.in<-glmer(activity~treatment+(1|site), dat = indole10,family=poisson)
summary(test.in)
shapiro.test(resid(test.in))#non-normal
overdisp_fun(test.in)#overdispersed

mod.in<-glmer.nb(activity~treatment+(1|site), dat = indole10)
Anova(mod.in)
#treatment chi=2.2635 p=0.1325

#table
in10.tab <- ddply(indole10, c("treatment"), summarise,
					 N    = length(activity),
					 mean = mean(activity),
					 sd   = sd(activity),
					 se   = sd / sqrt(N))
in10.tab

#graph
ggplot(data=indole10, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity (nightly passes)",
		 title = "Indole trials")+
	theme(text = element_text(size=15))+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond", color="#d9a9f6")


##GROUPING SPECIES INDOLE----
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

test.i<-glmer(activity~treatment*sp+(1|site), dat = indole3,family=poisson)
summary(test.i)
shapiro.test(resid(test.i))#non-normal

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
#ggsave(filename = "indole.png", 
	   #plot = indole.with.inset,
	   #width = 17, 
	   #height = 12,
	   #units = "cm",
	   #dpi = 300)

#species plot (raw data)
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

#not species-level
farn10<-aggregate(activity ~ treatment + jdate + site, dat=farn1, FUN=sum)

#creating log-transformed value for graphs
farn10$log.act<-log(farn10$activity)
farn10$log.act[which(!is.finite(farn10$log.act))] <- 0

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

#graph
ggplot(data=farn10, aes(x=treatment, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5)+
	theme_classic()+
	labs(x=" ", y="Relative activity (nightly passes)",
		 title = "Farnesene trials")+
	theme(text = element_text(size=15))+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond", color="#d9a9f6")

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
#importing "failed" indole trial (only control plots)
indole0 <- read.csv(file="Maynard_etal_indole2_control.csv",head=TRUE)
indole0[, 3:11][is.na(indole0[, 3:11])] <- 0

##Gathering data — compounds from col to rows
indole01<-indole0 %>% gather(sp, activity, EPTFUS:NOID)
indole01$sp<-as.factor(indole01$sp)
levels(indole01$sp)
indole01<-aggregate(activity ~ treatment + sp + jdate + site, dat=indole01, FUN=sum)

indole1$compound<-NA
indole1$compound<-"indole"

indole01$compound<-NA
indole01$compound<-"indole"

farn1$compound<-NA
farn1$compound<-"farnesene"

dis.all1 <- rbind(farn1, indole1, indole01)

##GROUPING SPECIES----
{dis.all<-dis.all1
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

dis.all2<-aggregate(activity ~ treatment + sp + jdate + site, dat=dis.all, FUN=sum)
dis.all2$log.act<-log(dis.all2$activity)
dis.all2$log.act[which(!is.finite(dis.all2$log.act))] <- 0

#exclude treatment trials
dis.all.control<-dis.all2[dis.all2$treatment != "Dispenser", ]  
dis.all.control <- dis.all.control[order(dis.all.control$jdate),]

dis.all.treat<-dis.all2[dis.all2$treatment != "Control", ]  
dis.all.treat <- dis.all.treat[order(dis.all.treat$jdate),]

q1<-glmer.nb(activity~sp+(1|site), data = dis.all.control)
Anova(q1)

#contrasts
q1c<-emmeans(q1,pairwise~sp, type="response")
cld(q1c$emmeans,  Letters ='abcdefg')
#MYLU=A, NYHU=B, PESU=C, LABO/LASE+LACI=D, NOID=E, EPFU/LANO=f

q1.tab <- ddply(dis.all.control, c("sp"), summarise,
				  N    = length(activity),
				  mean = mean(activity),
				  sd   = sd(activity),
				  se   = sd / sqrt(N))
q1.tab


#species plot
all.plot<-ggplot(data=dis.all.control, aes(x=sp, y=activity))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	theme(text = element_text(size=20), legend.position = "none", 
		  axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.9))+
	stat_summary(geom = 'text', label = c("f","d","d","a","e","b","c"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,1600))
all.plot

#small plot (means and SEs)
all.small<-ggplot(data=dis.all.control, aes(x=sp, y=activity))+ 
	theme_classic()+
	labs(x=" ", y="Relative activity")+
	stat_summary(fun.data = "mean_se", size=1.5, shape="diamond")+
	theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.9))
all.small

#plot with inlay
all.with.inset <-
	ggdraw() +
	draw_plot(all.plot) +
	draw_plot(all.small, x = 0.45, y = .6, width = .5, height = .4)
all.with.inset

#log-transformed species plot
ggplot(data=dis.all.control, aes(x=sp, y=log.act))+ 
	geom_boxplot(outlier.shape = NA)+
	geom_point(position=position_jitter(width = 0.025), alpha=0.4, size=2.5, aes(color=sp))+
	theme_classic()+
	labs(x=" ", y="Relative activity (log-transformed)", title="All control trials")+
	theme(text = element_text(size=20), legend.position = "none", 
		  axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.9))+
	stat_summary(geom = 'text', label = c("f","d","d","a","e","b","c"),
				 fun = max, vjust = -0.8, size=5.5)+
	scale_y_continuous(limits = c(0,8))



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


