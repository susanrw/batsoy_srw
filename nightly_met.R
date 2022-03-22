library(lubridate)
library(dplyr)

##SERC met data----
aug <- read.csv(file="SERC_TOWER_aug2021.csv",head=TRUE)
sep <- read.csv(file="SERC_TOWER_sep2021.csv",head=TRUE)
jun <- read.csv(file="SERC_TOWER_june2021.csv",head=TRUE)
jul <- read.csv(file="SERC_TOWER_july2021.csv",head=TRUE)

met<-rbind(aug,sep,jun,jul)

met$date<-as.Date(met$date,)
met$jdate<-NA
met$jdate<-yday(met$date)

#filtering for hours bats were active for dispenser trials, recorders active (7p-7a)
met1<-met%>%filter(between(hour, 0,7))
met2<-met%>%filter(between(hour, 19,23))
met.d<-rbind(met1,met2)


#filtering for hours bats were active for plant trials, recorders active (6p-8a)
met3<-met%>%filter(between(hour, 0,6))
met4<-met%>%filter(between(hour, 20,23))
met.p<-rbind(met3,met4)

#aggregate data so it's nightly averages
met.day.p<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Air_Temperature_C,Relative_Humidity_pct,
						   Rain_Accumulation_mm, Rain_Duration_s)~jdate, dat=met.p, FUN=mean)
met.day.d<-aggregate(cbind(Wind_speed_max_m.s,Wind_speed_avg_m.s,Air_Temperature_C,Relative_Humidity_pct,
						   Rain_Accumulation_mm, Rain_Duration_s)~jdate, dat=met.d, FUN=mean)

