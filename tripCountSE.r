library(feather)
library(survey)
options(survey.lonely.psu="certainty",multicore=TRUE)
library(cdlTools)

#Read the trips data
trips <- read_feather("data/trips1721.feather")

#Add state abbreviation for easier interpretation
trips$STa=fips(trips$ST,to="Abbreviation")
with(subset(trips,MODE_FX==5),table(STa,WAVE))
with(subset(trips,MODE_FX==7),table(STa,WAVE))
with(subset(trips,MODE_FX==3),table(STa,WAVE))

#Add counter varible
trips$one=1

#Trips survey design object
tripss=svydesign(id=~psu_id, strata=~strat_id,weights=~wp_int,data=trips,nest=TRUE)

#Estimate total trip counts by year,month,mode, and state, including SEs
tripsT=svyby(~one,~YEAR+month+MODE_FX+ST,FUN=svytotal,
             design=subset(tripss,wp_int > 0 & month != "99" & MODE_FX != "4" & ST == ST_RES),
             drop.empty.groups=FALSE,multicore = TRUE)

#Calculate PSEs
tripsT$pse=100*tripsT$se/tripsT$one

#For the FES state/month combinations not sampled see Table 1 in:
#https://media.fisheries.noaa.gov/2021-11/MRIP-Fishing-Effort-Survey-2019-Annual-Report.pdf
#Should we also remove the same WAVE/ST combinations. Some are different, specifically in
#ME and NH during WAVES 2 and 6
tripsT$notSampled=with(tripsT,
                       (month %in% c("01","02") & ST %in% c(9,10,13,23:25,33,34,36,44,45,51))
                       | (month %in% c("03","04","11","12") & ST %in% c(23,33)))

table(tripsT$notSampled,tripsT$MODE_FX,useNA = "always")
table(tripsT$notSampled,tripsT$month,useNA = "always")

#Remove state/month combinations not sampled
tripsTs=subset(tripsT,notSampled==FALSE)

#Identify state/month combinations with zero or 100 pse
tripsTs$pseB=with(tripsTs, pse %in% c(0,100) | is.na(pse))

table(tripsTs$pseB)

table(tripsTs$pseB,tripsTs$month,tripsTs$MODE_FX,useNA = "always")

saveRDS(tripsTs,file="tripsTs.rds")



