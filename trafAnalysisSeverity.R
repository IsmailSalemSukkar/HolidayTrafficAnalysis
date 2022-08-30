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
options(scipen = 10)
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
#write.csv(trafData,"trafDataNJSev.csv")

#########Start Here after grabbing data from server#####

trafDataNJSev <- read.csv("trafDataNJ.csv")

trafDataNJSev$Date <- as.Date(trafDataNJSev$Start_Time)
trafDataNJSev$Month <- as.factor(format(trafDataNJSev$Date, format = "%m"))


trafDataNJSev$Time <- as.POSIXct(trafDataNJSev$Start_Time) %>% 
  format(format = "%H:%M:%S")

trafDataNJSev$Civil_Twilight <- as.factor(trafDataNJSev$Civil_Twilight)
trafDataNJSev$Severity <- factor(trafDataNJSev$Severity,
                              levels = c("1", "2", "3","4"))
trafDataNJSev<- trafDataNJSev[!trafDataNJSev$Civil_Twilight=="",] %>% droplevels()



#########Hypothesis 1: Does Day/Night have an effect on accident severity rate?######

#Lets try a parametric method first!

ggplot(data=trafDataNJSev,aes(y=as.numeric(Severity),x=Civil_Twilight))+geom_boxplot()
#Very concentrated around 2


severVsLight <- glmmTMB((Severity)~Civil_Twilight, data=trafDataNJSev)

plot(simulateResiduals(severVsLight))
#The assumptions fail completely, however lets keep going and see

Anova(severVsLight,type=2)
#We can see that Day/Night is significant!

r.squaredGLMM(severVsLight)
#But its effect size is weak...

pairs(emmeans(severVsLight,~Civil_Twilight,type = "response",adjust="tukey"))
#And we can see that there is a difference when performing an ad-hoc test

#So we expect to see a significant, but weak, effect that day/night has on severity

#Lets do a non-parametric method to confirm

kruskal.test(Severity~Civil_Twilight,data=trafDataNJSev) 
#Appears that Day/Night does have an effect



epsilonSquared(trafDataNJSev$Severity, trafDataNJSev$Civil_Twilight)
#However, the effect is very weak


kruskalmc(Severity~Civil_Twilight,data=trafDataNJSev)
#And disappears once you do a post-hoc test...

#Due to the lack of assumptions being met, and VERY weak effect sizes (less than .001), 
#we can assume that despite some statisitical significance, there is no real world
#significance of day vs night.


#########Hypothesis 2: Does Month have an effect on accident severity rate?######

#What may be interesting is does the month have an effect on accident severity rate?
#It is commonly said that november is especially dangerous to drive due to thanksgiving
#Lets see if there is any basis to that

summary(trafDataNJSev$Month)

plot(trafDataNJSev$Month)
#We can see that accidents ramp up from September thru December
#With November and December having the most

plot(Severity~Month,data=trafDataNJSev)
#Interestingly, the reverse is true when accounting for severity.
#Months 3-6 are most severe, they do somewhat ramp up to be fair


severVsMonth <- glmmTMB(Severity~Month, data=trafDataNJSev)
summary(severVsMonth)

Anova(severVsMonth,type=2)

plot(simulateResiduals(severVsMonth))
#Assumptions still not great, but not exactly a major issue due to
#this being ordinal
r.squaredGLMM(severVsMonth)

((emmeans(severVsMonth,~Month,type = "response",adjust="tukey")))

#Interestingly, Month is seen as significant, but a very weak effect (0.02)

#########Hypothesis 3: Does Month AND Day/Night have an effect on accident severity rate?######

severVsMonthDay <- glmmTMB(Severity~Month*Civil_Twilight, data=trafDataNJSev)
#AIC is slowly decreasing, and R-squared is increasing!
summary(severVsMonthDay)
Anova(severVsMonthDay,type=2)
r.squaredGLMM(severVsMonthDay)
emmeans(severVsMonthDay,~Month|Civil_Twilight,type = "response",adjust="tukey")
emmeans(severVsMonthDay,pairwise~Month|Civil_Twilight,type = "response",adjust="tukey")

#So far, many of these hypothesis relying on ordinal vs nominal variables are very weak

#########Hypothesis 4: Does Precip have an effect on accident severity rate?######
severVsMonthDay <- clm(Severity ~ Precipitation.in., data=trafDataNJSev)

1 - severVsMonthDay$logLik/severNull$logLik


summary(severVsMonthDay)
r.squaredGLMM(severVsMonthDay)



#Severity is clearly flawed due to whoever assigning these values being too libreal with the number 2,
#or cars are incredibly safe

fitNull <- clm(Severity~1,data=trafDataNJSev)
fit = clm(Severity~Temperature.F.+Precipitation.in.,data=trafDataNJSev)

1 - fit$logLik/fitNull$logLik

summary(fit)

r.squaredLR(fit)



