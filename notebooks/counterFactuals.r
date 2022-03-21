library(ciTools)
library(tidyverse)
#Load the final dataset
tripsTMPS=readRDS("tripsTMPS.rds")

no_data_sms<- tripsTMPS %>% 
  filter(Mode == "7") %>% 
  group_by(State, Month) %>% 
  summarise(max_Trips = max(Trips), .groups = "drop") %>% 
  filter(max_Trips == 0) %>% 
  select(State, Month)

data_for_new_model <- tripsTMPS %>% 
  filter(Mode == "7") %>% 
  anti_join(no_data_sms, by = c("State", "Month"))

mf=formula(Trips ~ State + Month + I(Stringency/100) + I((Stringency/100)^2) + offset(log(Population)))
pm=glm(mf,data=data_for_new_model,family = quasipoisson )

exp(sum(coef(pm)[c(1,4)]))*
  sum(sapply(coef(pm)[c(17:27)],exp))*
  with(data_for_new_model,Population[Year==2020 & Month=="01" & State=="Florida" & Mode==7])

exp(sum(coef(pm)[c(1,4)])+coef(pm)[21])*
  with(data_for_new_model,Population[Year==2020 & Month=="01" & State=="Florida" & Mode==7])

sum(subset(ppt,select=pred,Month=="06" & State=="Florida"))

exp(sum(coef(pm)[c(1,4)]))*
  sum(sapply(coef(pm)[c(17:27)],exp))*
  with(data_for_new_model,Population[Year==2020 & Month=="01" & State=="Florida" & Mode==7])

sum(subset(ppt,select=pred,Month %in% c("02","03","04","05","06","07","08","09","10","11","12") & State=="Florida"))

####What if you set the stringency to zero for every month and state of 2020?####

nd=subset(data_for_new_model,Mode==7 & Year==2020)
nd$Stringency=0
ppt=add_ci(nd,pm)

pv=data.frame(T20c19=aggregate(Trips~State,sum,data=subset(tripsTMPS,Mode==7 & Year==2020)),
           T20=aggregate(pred~State,sum,data=ppt)$pred)
pv$change=100*(pv$T20-pv$T20c19.Trips)/pv$T20
pv$changeS=scale(pv$change,scale=FALSE)
barplot(as.matrix(pv$change),name = usdata::state2abbr(pv$T20c19.State), beside=TRUE,ylim=c(-20,20),
        ylab="% Change: Without - With C-19 Policies")  


####What if you used the 2020 stringency levels for corresponding month and state of 2019?####

s2020=subset(tripsTMPS,Year==2020 & Mode==7,select=c(State,Month,Stringency))

t2019p=subset(tripsTMPS,Year==2019 & Mode==7,select=c(Year,State,Month,Population,Trips))
t2019p=merge(t2019p,s2020,by=c("State","Month"))

ppt=add_ci(t2019p,pm)

ppta=aggregate(cbind(Trips,pred)~State,sum,data=ppt)

ppta$change=with(ppta,100*(Trips-pred)/Trips)

barplot(as.matrix(ppta$change),name = usdata::state2abbr(ppta$State), beside=TRUE,
        ylab="% Change: Without - With C-19 Policies",ylim=c(-30,30))  


####Attempting stringency elasticity calculations####
#e = s*(b + 2*r*s)
#where e is elasticity, s is the stringency index, and b and r are 
#the parameters on stringency and stringency squared, respectively

ms=aggregate(Stringency~State,mean,data=s2020)
ms$elast=with(ms,(Stringency/100)*(coef(pm)["I(Stringency/100)"] + 2*coef(pm)["I((Stringency/100)^2)"]*(Stringency/100)))
