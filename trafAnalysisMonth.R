library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(RMariaDB)
library(emmeans)
library(MuMIn)
library(performance)
library(car)
library(tibble)
library(dplyr)
library(rcompanion)
library(pgirmess)
library(ordinal)
library(gplots)
library(effectsize)
#options(scipen = 10)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######Run the Following ONCE########
#con <- dbConnect(MariaDB(), dbname = "Datasets",host="192.168.0.100",password="1234")

#dbListTables(con)

#dbRequest <- dbSendQuery(con,"SELECT * 
#                         FROM trafficData
#                         WHERE State = 'NJ'")
#trafData <- dbFetch(dbRequest) 
#trafData <- trafData[-1,]





#dbClearResult(dbRequest)
#dbDisconnect(con)

#To keep the sample consistant, the seed will be set
#set.seed(2)
#write.csv(trafData,"trafDataNJ.csv")

#########Start Here after grabbing data from server#####

trafDataNJbase <- read.csv("trafDataNJ.csv")

trafDataNJbase$Date <- as.Date(trafDataNJbase$Start_Time)

trafDataNJbase$Month <- as.factor(format(trafDataNJbase$Date, format = "%m"))
trafDataNJbase$Year <- as.factor(format(trafDataNJbase$Date, format = "%Y"))



#trafDataMonth <- trafDataNJbase %>% group_by(Month,Year)%>%
#  summarise(frequency = n(), precip = mean(Precipitation.in.),
#            wind = mean(Wind_Speed.mph.),vis=mean(Visibility.mi.),
#            humid = mean(Humidity...),temp = mean(Temperature.F.))

#plot(trafDataMonth$frequency~trafDataMonth$Year)

#So, must cut out to 2021.

trafDataNJbase<-trafDataNJbase[trafDataNJbase$Year=="2021",] %>% droplevels()
#trafDataNJbase$Month <- as.factor(format(trafDataNJbase$Date, format = "%m"))



#plot(trafDataMonth$frequency~trafDataMonth$Month)


trafDataNJbase$StreetCity <-  paste(trafDataNJbase$Street,trafDataNJbase$City)
trafDataNJbase$StreetCity <- trimws(trafDataNJbase$StreetCity)

trafDataNJ <- trafDataNJbase %>% group_by(Month,StreetCity)%>%
  summarise(frequency = n(), precip = mean(Precipitation.in.),
            wind = mean(Wind_Speed.mph.),vis=mean(Visibility.mi.),
            humid = mean(Humidity...),temp = mean(Temperature.F.))



#########Does Month affect Accident Count###########

trafDataNJ$Month <- as.factor(trafDataNJ$Month)

plot(trafDataNJ$frequency~trafDataNJ$Month)




freqVSmonth <- glmmTMB(frequency~(Month)+(1|StreetCity),
                       family=nbinom2(),data=trafDataNJ)
fitMonth <- r.squaredGLMM(freqVSmonth)
plot(simulateResiduals(freqVSmonth))
#check_overdispersion(freqVSmonth)
Anova(freqVSmonth,type=2)
summary(freqVSmonth)
fitMonth

plot(trafDataNJ$frequency~trafDataNJ$Month)

plotmeans(trafDataNJ$frequency~trafDataNJ$Month)


#Month does affect accident rate! Significant, but low effect sizes

#########Does precip affect Accident Count###########


plot(trafDataNJ$frequency~trafDataNJ$precip)




freqVSprecip <- glmmTMB(frequency~precip+(1|StreetCity),
                        family=nbinom2(),data=trafDataNJ)
fitPrecip <- r.squaredGLMM(freqVSprecip)
#plot(simulateResiduals(freqVSmonth))
Anova(freqVSprecip,type=2)
summary(freqVSprecip)
fitPrecip

#Nope!

#########Does temp  affect Accident Count###########

plot(trafDataNJ$frequency~trafDataNJ$temp)


freqVStemp <- glmmTMB(frequency~temp+(1|StreetCity),family=nbinom2(),data=trafDataNJ)
fitTemp <- r.squaredGLMM(freqVStemp)
#plot(simulateResiduals(freqVSmonth))
Anova(freqVStemp,type=2)
summary(freqVStemp)
fitTemp


ggplot(data=trafDataNJ, aes(y=frequency,x=temp))+geom_point()+
  geom_smooth(method="lm")

#YES, but R2 is VERY low (0.003)

#########Does precip+temp affect Accident Count###########


plot(trafDataNJ$frequency~trafDataNJ$precip)




freqVSprecipTemp <- glmmTMB(frequency~precip*temp+(1|StreetCity),
                        family=nbinom2(),data=trafDataNJ)
fitPrecipTemp <- r.squaredGLMM(freqVSprecipTemp)
#plot(simulateResiduals(freqVSmonth))
Anova(freqVSprecipTemp,type=2)
summary(freqVSprecipTemp)
fitPrecipTemp

ggplot(data=trafDataNJ, aes(y=frequency,x=(precip*temp)))+geom_point()+
  geom_smooth(method="lm") + scale_y_log10() + scale_x_continuous(trans="log1p")

#Yes, but R2 is very low again (.005)

########Month and Temp SHOULD be autocorrelated, so should not be done together######
#Lets see though

freqVsMonthPrecipTemp <- glmmTMB(frequency~Month+precip*temp+(1|StreetCity),
                            family=nbinom2(),data=trafDataNJ)

fitPrecipTempMonth <- r.squaredGLMM(freqVsMonthPrecipTemp)
Anova(freqVsMonthPrecipTemp,type=2)
summary(freqVsMonthPrecipTemp)
fitPrecipTempMonth

check_autocorrelation(freqVsMonthPrecipTemp)
#Maybe not?
