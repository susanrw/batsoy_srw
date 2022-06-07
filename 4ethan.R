

mod1<-glm(activity~Air_Pressure_pascal+ Air_Temperature_C + 
		  	delta.air + act2 + Wind_speed_avg_m.s, dat = bat.met.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod1)

library(MuMIn)
#dredging and model averaging
d1<-dredge(mod1)
davg1<-model.avg(d1, subset=delta<2)
summary(davg1)

#removing zeros from cumulative rain hours data
rain.bat.hour<-bat.met.hour[bat.met.hour$rain_cum_hours != "0", ]  

mod2<-glm(activity~rain_cum_hours, dat = rain.bat.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod2)

library(car)
Anova(mod2)

#creating model with top variables to make prediction plots
mod3<-glm(activity~Air_Temperature_C + act2 + Wind_speed_avg_m.s + wind.avg2, dat = bat.met.hour,
		  family = Gamma(link=log),na.action = "na.fail")
summary(mod3)

bat.met.hour$yhat<-predict(mod3)
predplot1<-ggplot(bat.met.hour)+
	geom_point(aes(x=Air_Temperature_C, y=activity))+
	geom_point(aes(x=Air_Temperature_C, y=yhat), color="red", size=2)+
	geom_line(aes(x=Air_Temperature_C, y=yhat) ,color="red", size=1)
predplot1

predplot2<-ggplot(bat.met.hour)+
	geom_point(aes(x=wind.avg2, y=activity))+
	geom_point(aes(x=wind.avg2, y=yhat), color="red", size=2)+
	geom_line(aes(x=wind.avg2, y=yhat) ,color="red", size=1)
predplot2

predplot3<-ggplot(bat.met.hour)+
	geom_point(aes(x=Wind_speed_avg_m.s, y=activity))+
	geom_point(aes(x=Wind_speed_avg_m.s, y=yhat), color="red", size=2)+
	geom_line(aes(x=Wind_speed_avg_m.s, y=yhat) ,color="red", size=1)
predplot3

rain.bat.hour$yhat<-predict(mod2)
predplot4<-ggplot(rain.bat.hour)+
	geom_point(aes(x=rain_cum_hours, y=activity))+
	geom_point(aes(x=rain_cum_hours, y=yhat), color="red", size=2)+
	geom_line(aes(x=rain_cum_hours, y=yhat) ,color="red", size=1)
predplot4

#effect sizes
exp(8.995e-02)
#bat activity increases 1.1 with every 1 degC increase in air temp
exp(8.320e-01)
#bat activity increases 2.3.1 with every 1 m/s increase in wind speed

summary(lm(bat.met.hour$activity~bat.met.hour$Air_Temperature_C))
#lm estimate=2.9
summary(lm(bat.met.hour$activity~bat.met.hour$Wind_speed_avg_m.s))
#lm estimate=2.8