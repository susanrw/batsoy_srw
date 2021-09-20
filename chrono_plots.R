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
