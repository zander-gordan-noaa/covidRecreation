library(feather)
library(survey)
options(survey.lonely.psu="adjust",multicore=TRUE)
library(tidycensus)
library(tidyverse)

census_api_key("dc352be83efcd7e4189b8a5d1be652187e50c66d",install = TRUE)

#Load SE MRIP data
#subset: private boats, 2012-2016, GOM, not inland
trips=read_feather("../mrip/tripsSE.feather")
trips=subset(trips, wp_int>0 & YEAR %in% c(2018:2020) & month<13 )

trips$one=1

#Trips survey design object
tripss=svydesign(id=~psu_id, strata=~strat_id,weights=~wp_int, data=trips)

tripsT=svyby(~one,~YEAR+month+MODE_FX+ST,FUN=svytotal,design=tripss,drop.empty.groups=FALSE)

tripsT$one[is.na(tripsT$one)]=0
tripsT$MONTH=as.numeric(tripsT$month)
tripsT$year=as.numeric(as.character(tripsT$YEAR))
tripsT$fips=as.numeric(as.character(tripsT$ST))

#https://github.com/nytimes/covid-19-data/tree/master/rolling-averages
cv=read.csv("us-states.csv",stringsAsFactors = FALSE)
cv$YEAR=substr(cv$date,1,4)
cv$MONTH=as.numeric(substr(cv$date,6,7))
cv$fips=as.numeric(substr(cv$geoid,5,6))

cva=aggregate(cases~YEAR+MONTH+fips,FUN=sum,data=cv)

tripsT1=merge(tripsT,cva,by.x=c("year","MONTH","fips"),by.y=c("YEAR","MONTH","fips"),all.x = TRUE)
tripsT1$cases[is.na(tripsT1$cases)]=0

acsY = function(y) {
  d=get_acs(geography = "state", 
            variables = "B01001_001", 
            year = y,
            survey = "acs1")
  d$year=y
  return(d)
}

pop=do.call(rbind,lapply(2010:2019,FUN=function(x) acsY(x)))

pop20 <- get_decennial(geography = "state", 
                       variables = "P1_001N", 
                       year = 2020)
pop20$year=2020
pop20=plyr::rename(pop20,replace=c("value" = "estimate"))

pop=plyr::rbind.fill(pop,pop20)
pop$fips=as.numeric(pop$GEOID)

tripsT2=merge(tripsT1,pop,by.x=c("year","fips"),by.y=c("year","fips"),all.x=TRUE)

#https://github.com/OxCGRT/USA-covid-policy
stringentw= openxlsx::read.xlsx(xlsxFile = "OxCGRTUS_timeseries_all.xlsx", sheet = 1)
stringent <- reshape::melt(stringentw, id=names(stringentw[1:5]))
stringent$month=match(substr(stringent$variable,3,5),month.abb)
stringent$year=as.numeric(substr(stringent$variable,6,9))
stringent$fips=cdlTools::fips(stringent$region_name)

stringentm=aggregate(value~year+month+fips,FUN=sum,data=stringent,na.rm=TRUE)

tripsT3=merge(tripsT2,stringentm,by.x=c("year","MONTH","fips"),by.y=c("year","month","fips"),all.x=TRUE)
tripsT3$value[is.na(tripsT3$value)]=0

tripsT3$ym=paste0(tripsT3$year,tripsT3$month)
library(ggplot2)
library(dplyr)
tripsT3 %>%
  ggplot( aes(x=ym,y=value, group=ST, color=ST)) +
  geom_line()


pm=glm(one~ST+month+value+I(value^2),offset=log(estimate),
       data=tripsT3,family = quasipoisson,subset=MODE_FX==7 & year>2018)
summary(pm)
cm=glm(one~ST+month+value+I(value^2),offset=log(estimate),
       data=tripsT3,family = quasipoisson,subset=MODE_FX==5 & year>2018)
summary(cm)
sm=glm(one~ST+month+value+I(value^2),offset=log(estimate),
       data=tripsT3,family = quasipoisson,subset=MODE_FX==3 & year>2018)
summary(sm)

pd=predict(pm,newdata = data.frame(ST="12",month="06",estimate=1000000,value=seq(0,2500,by=50)))
cd=predict(cm,newdata = data.frame(ST="12",month="06",estimate=1000000,value=seq(0,2500,by=50)))
sd=predict(sm,newdata = data.frame(ST="12",month="06",estimate=1000000,value=seq(0,2500,by=50)))


plot(y=pd,x=seq(0,2500,by=50),type="l",ylim=c(11.5,12.5),
     main="Predicted Private Boat Trips as a Function of the Stringency Index",
     xlab="Stringincy Index (monthly sum of daily records: higher is more stringent)",
     ylab="Millions of Fishing Trips (Scaled to June in FL)")
plot(y=cd,x=seq(0,2500,by=50),type="l",ylim=c(8.5,9.5),
     main="Predicted Charter Boat Trips as a Function of the Stringency Index",
     xlab="Stringincy Index (monthly sum of daily records: higher is more stringent)",
     ylab="Millions of Fishing Trips (Scaled to June in FL)")
plot(y=sd,x=seq(0,2500,by=50),type="l",ylim=c(12,13),
     main="Predicted Shore Trips as a Function of the Stringency Index",
     xlab="Stringincy Index (monthly sum of daily records: higher is more stringent)",
     ylab="Millions of Fishing Trips (Scaled to June in FL)")



selast=pm$coefficients["value"] + 2*(0:2500)*pm$coefficients["I(value^2)"]
elast= selast*(0:2500)

plot(100*selast,type="l")
plot(100*elast,type="l")




pm=glm(one~offset(log(estimate))+ST+month+I(cases/(estimate/100000)),
       data=tripsT2,family = quasipoisson,subset=MODE_FX==3)
summary(pm)
round(exp(pm$coefficients),3)

tripsT2$ym=paste0(tripsT2$year,tripsT2$month)
library(ggplot2)
library(dplyr)
tripsT2 %>%
  ggplot( aes(x=ym,y=I(cases/(estimate/100000)), group=ST, color=ST)) +
  geom_line()



