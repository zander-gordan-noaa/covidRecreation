library(data.table)
library(feather)
library(haven)

#Function to download and extract one MRIP data archive (fn=file name)
getMRIP = function(fn) {
  
  #Archive location on the www
  furl=paste0("https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/SAS/",fn)
  
  #Temp archive name
  zf="./zip/temp.zip"
  
  #Download the archive
  download.file(furl, zf)
  
  #Names of csv files in the archive
  fname = unzip(zf, list=TRUE)
  
  #Location of MRIP csv files
  zd="./sas"
  
  #Unzip the files and remove the archive
  unzip(zf, exdir=zd, overwrite=TRUE)
  file.remove(zf)
  
}

#List of mrip survey data archives: 2017-2020
mripZ=list("ps_2017_sas.zip",
           "ps_2018_sas.zip",
           "ps_2019_sas.zip",
           "ps_2020_sas.zip",
           "ps_2021_sas.zip")

#Download and extract the mrip survey data to the csv directory
lapply(mripZ,FUN=getMRIP)

#List of all the files in the csv directory
flist <- list.files("./sas",full.names = TRUE)

#catch and trip file csv names
tripsf=grep("trip",flist,value=TRUE)

#Temporary list of trip data objects read from the csv files
l <- lapply(tripsf, read_sas)
#Combine the trip data objects into one file
trips <- subset(rbindlist( l ,fill=TRUE))

#Remove the sas files
do.call(file.remove, list(flist))

#Add the zip codes back to the data
#zipsA=read_sas("../mrip/zipCodes/zips_2004_2017.sas7bdat")
#zipsB=read_sas("../mrip/zipCodes/se_2018_2020.sas7bdat")[,c("ID_CODE","ZIP")]
#zips=rbind(zipsA,zipsB)

#trips=merge(trips,zips,by="ID_CODE",all.x = TRUE)
#trips$ZIP[trips$YEAR<2004]=trips$ZIP.x[trips$YEAR<2004]
#trips$ZIP[trips$YEAR>2003]=trips$ZIP.y[trips$YEAR>2003]
#trips=subset(trips, select=-c(ZIP.x,ZIP.y))


#Save the combined trip and files in feather format
write_feather(trips,"trips1721.feather")
