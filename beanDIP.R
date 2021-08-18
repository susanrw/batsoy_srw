bean <- read.csv(file="beanDIP_activity.csv",head=TRUE)

head(bean)

library(dplyr)
library(ggplot2)
library(viridis)
library(colorbrewer)

bean$date<-as.Date.character(bean$date,"%Y%m%d")

bean %>%
	ggplot(aes(x=date, 
			   y=total.activity,
			   color=site))+
	geom_point()+
	geom_line()+
	labs(x="Date",
		 y="Total activity (no. detections)",
		 color="Site")+
	theme_classic()
#	
