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
library(viridis)
#options(scipen = 100)
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




trafDataNjDate <- trafDataNJbase %>% group_by(Date)%>%
  summarise(frequency = n(), precip = mean(Precipitation.in.),
            wind = mean(Wind_Speed.mph.),vis=mean(Visibility.mi.),
            humid = mean(Humidity...),temp = mean(Temperature.F.))

trafDataNjDate$Month <- as.factor(format(trafDataNjDate$Date, format = "%m"))


trafDataNjMonth <- trafDataNJbase %>% group_by(Month)%>%
  summarise(frequency = n(), precip = mean(Precipitation.in.),
            wind = mean(Wind_Speed.mph.),vis=mean(Visibility.mi.),
            humid = mean(Humidity...),temp = mean(Temperature.F.))



#########Does Month affect Accident Count###########

trafDataNJ$Month <- as.factor(trafDataNJ$Month)

jpeg("rplot.jpg", width = 1920, height = 1080)
plotmeans(trafDataNJ$frequency~trafDataNJ$Month,ylab="Frequency",xlab="Month",
          main="Accidents over Time")
dev.off()

ggplot(data=trafDataNJ,aes(y=frequency, x = Month,color=Month))+
  geom_point()+
  geom_boxplot()


freqVSmonth <- glmmTMB(frequency~(Month),
                       family=nbinom2(),data=trafDataNJ)
fitMonth <- r.squaredGLMM(freqVSmonth)
#plot(simulateResiduals(freqVSmonth))
#check_overdispersion(freqVSmonth)
Anova(freqVSmonth,type=2)
summary(freqVSmonth)
emmeans(freqVSmonth,pairwise~Month,type = "response")
fitMonth

plotmeans(trafDataNJ$frequency~trafDataNJ$Month)



####Does date affect accident count?#######

freqVsDate <- glmmTMB(frequency~(Date),family="nbinom2",
                      data=trafDataNjDate)
fitDate <- r.squaredGLMM(freqVsDate)
#plot(simulateResiduals(freqVSmonth))
#check_overdispersion(freqVSmonth)
Anova(freqVsDate,type=2)
summary(freqVsDate)
fitDate

ggplot(data=trafDataNjDate,aes(y=frequency, x = Date))+
  geom_point(data=trafDataNjDate,aes(y=frequency, x = Date, color=Month))+
  geom_point(data=trafDataNjDate,aes(y=frequency, x = Date),
             shape=1,color="black")+
  geom_smooth(method="glm", method.args = list(family = "nbinom2"),
              color="red")+
  scale_color_viridis(discrete=T,option="turbo",begin=.25,end=.75)+
  scale_x_date(date_breaks = "1 month",date_labels = "%b %y")+
  ggtitle("Accident Frequency in NJ Over 2021", "p = < 0.005, Pseudo R²= 0.38, Distribtion = Overdispersed Poisson")

