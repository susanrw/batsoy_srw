# Data import and cleaning ------------------------------------------------


#read in all data
{ cr1 <- read.csv(file="chrono_1633.csv",head=TRUE)
cr2 <- read.csv(file="chrono_0505.csv",head=TRUE)
cr3 <- read.csv(file="chrono_1804.csv",head=TRUE)
cr4 <- read.csv(file="chrono_4704.csv",head=TRUE)
cr5 <- read.csv(file="chrono_4455.csv",head=TRUE)
cr6 <- read.csv(file="chrono_4659.csv",head=TRUE)}

#creating age class columns
cr1$age.class <- NA
for(i in 1:length(cr1$FILES)){
	if(cr1$FILES[i]==1){cr1$age.class[i]="1"}}

cr2$age.class <- NA
for(i in 1:length(cr2$FILES)){
	if(cr2$FILES[i]==1){cr2$age.class[i]="2"}}

cr3$age.class <- NA
for(i in 1:length(cr3$FILES)){
	if(cr3$FILES[i]==1){cr3$age.class[i]="2"}}

cr4$age.class <- NA
for(i in 1:length(cr4$FILES)){
	if(cr4$FILES[i]==1){cr4$age.class[i]="3"}}

cr5$age.class <- NA
for(i in 1:length(cr5$FILES)){
	if(cr5$FILES[i]==1){cr5$age.class[i]="3"}}

cr6$age.class <- NA
for(i in 1:length(cr6$FILES)){
	if(cr6$FILES[i]==1){cr6$age.class[i]="3"}}

#creating age columns
cr1$age <- NA
for(i in 1:length(cr1$FILES)){
	if(cr1$FILES[i]==1){cr1$age[i]="18"}}

cr2$age <- NA
for(i in 1:length(cr2$FILES)){
	if(cr2$FILES[i]==1){cr2$age[i]="54"}}

cr3$age <- NA
for(i in 1:length(cr3$FILES)){
	if(cr3$FILES[i]==1){cr3$age[i]="80"}}

cr4$age <- NA
for(i in 1:length(cr4$FILES)){
	if(cr4$FILES[i]==1){cr4$age[i]="125"}}

cr5$age <- NA
for(i in 1:length(cr5$FILES)){
	if(cr5$FILES[i]==1){cr5$age[i]="144"}}

cr6$age <- NA
for(i in 1:length(cr6$FILES)){
	if(cr6$FILES[i]==1){cr6$age[i]="286"}}

#combining data
cr.tot <- rbind(cr1, cr2, cr3, cr4, cr5, cr6)

#removing noise files
cr.tot<-cr.tot[cr.tot$AUTO.ID. != "Noise", ] 

#selecting columns (need dplyr loaded)
cr.tot<-select(cr.tot, DATE, TIME, HOUR, DATE.12, TIME.12, HOUR.12, AUTO.ID., FILES, age.class, age)

#formatting =
{	cr.tot$AUTO.ID.<-as.factor(cr.tot$AUTO.ID.)
	cr.tot$DATE.12<-as.Date(cr.tot$DATE.12,"%Y-%m-%d")
	cr.tot$HOUR<-as.numeric(cr.tot$HOUR)
	cr.tot$DATE<-as.Date(cr.tot$DATE,"%Y-%m-%d")
	cr.tot$HOUR.12<-as.numeric(cr.tot$HOUR.12)
	cr.tot$age<-as.numeric(cr.tot$age)
	cr.tot$age.class<-as.factor(cr.tot$age.class)
}

#aggregating data
cr.agg<-aggregate(FILES ~ DATE.12 + age + AUTO.ID. + age.class, data=cr.tot, FUN=sum)

#plot
plot.cr<-cr.agg %>%
	ggplot(aes(x=age, 
			   y=FILES))+
	geom_point(aes(color=AUTO.ID.))+
	geom_smooth(method = "lm")+
	labs(x="Stand age",
		 y="Relative activity (no. nightly detections)",
		 title="Chrono plots")+
	theme_classic()+
	scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300))
plot.cr
plot.cr+facet_wrap(~AUTO.ID.)

#plot2
plot.cr2<-cr.agg %>%
	ggplot(aes(x=age.class, 
			   y=FILES))+
	geom_boxplot()+
	geom_point(aes(color=AUTO.ID.))+
	labs(x="Stand age",
		 y="Relative activity (no. nightly detections)")+
	theme_classic()
plot.cr2
plot.cr2+facet_wrap(~AUTO.ID.)

##2022 additions---
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

