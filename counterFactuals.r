

mf=formula(Trips ~ State + Month + I(Stringency/100) + I((Stringency/100)^2) + log(Population))
pm=glm(mf,data=tripsTMPS,family = quasipoisson,subset=Mode==7 )


####What if you set the stringency to zero for every month and state of 2020?####

pTrips0=function(m){
  nd=data.frame(State=unique(tripsTMPS$State),
                Month=unique(tripsTMPS$Month),
                Population=with(tripsTMPS,Population[Mode==7 & Year==2020]),
                Stringency=0)
  add_ci(nd,m)
}


ppt=pTrips0(pm)

pv=data.frame(T20c19=aggregate(Trips~State,sum,data=subset(tripsTMPS,Mode==7 & Year==2020)),
           T20=aggregate(pred~State,sum,data=ppt)$pred)
pv$change=100*(pv$T20-pv$T20c19.Trips)/pv$T20

barplot(as.matrix(pv$change),name = usdata::state2abbr(pv$T20c19.State), beside=TRUE,
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
