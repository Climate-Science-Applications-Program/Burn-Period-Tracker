# create Burn Period climatologies from DRI-CEFA gap-filled RAWS data
# MAC 02/19/23

library(dplyr)
library(XML)
library(RCurl)

file_list <- list.files(path="./data/raws/AZ", full.names = TRUE)
file_names<- list.files(path="./data/raws/AZ", full.names = FALSE)

#####
# calculate burning hours
burnList<-list()
  k=1
offStn<-list()
  j=1

for(i in 1:length(file_list)){
  # read in RAWS file
  tempRAWS<-read.csv(file_list[i])
  tempRAWS$DateTime<-as.POSIXct(tempRAWS$DateTime, format = "%Y-%m-%dT%H:%M:%S")
  tempRAWS$year<-as.numeric(format(tempRAWS$DateTime,"%Y"))
  tempRAWS$month<-as.numeric(format(tempRAWS$DateTime,"%m"))
  tempRAWS$day<-as.numeric(format(tempRAWS$DateTime,"%d"))
  
  # get station metadata and check for recent data
  #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-",format(Sys.time(), "%Y"),"&end=31-Dec-",format(Sys.time(), "%Y"),"&time=&sort=&ndays=")
  stnID<-substr(file_names[i], 1,6)
  url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",stnID,"&sig=&type=&start=&end=&time=&sort=asc&ndays=5&user=")
  # past year
  #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
    xData <- getURL(url)
    xmldoc <- xmlParse(xData)
    rawsMeta <- xmlToDataFrame(xData)
  
  # skip station, if empty response  
    if(nrow(rawsMeta)==0){
      offStn[[j]]<-stnID
      print(paste0(stnID,", no response"))
      j=j+1
    }else{
      
      stnName<-rawsMeta$sta_nm[1]
      lat<-rawsMeta$latitude[1]
      lon<-rawsMeta$longitude[1]
      
      # count of burn hours
      burnHRS<-  tempRAWS %>%
        group_by(year, month, day) %>%
        summarize(n_hours = n(),
                  rh_lt_20 = sum(`RelativeHumidity...` <= 20, na.rm = TRUE),
                  minRH = min(as.numeric(as.character(`RelativeHumidity...`)), na.rm = TRUE),
                  maxRH = max(as.numeric(as.character(`RelativeHumidity...`)), na.rm = TRUE))
      
      # add in date field
      burnHRS$date<-as.Date(paste0(as.numeric(burnHRS$month),"-",as.numeric(burnHRS$day),"-",as.numeric(burnHRS$year)), format="%m-%d-%Y")
      
      # thin out days with fewer than 21 hours ~ 10% missing and create complete dataframe
      #burnHRS<-tidyr::complete(burnHRS, date = seq.Date(min(date), max(date), by="day"), fill = list())
      burnHRS<-subset(burnHRS, n_hours>=21)
      # complete dates
      dates<-  as.data.frame(seq.Date(min(burnHRS$date), max(burnHRS$date), by="day"))
      colnames(dates)<-"date"
      # complete list
      burnHRS<-merge(burnHRS, dates, by="date", all.y=TRUE)
      
      stnName<-rawsMeta$sta_nm[1]
      lat<-rawsMeta$latitude[1]
      lon<-rawsMeta$longitude[1]
      
      # add in station info
      burnHRS$LATITUDE<-as.numeric(rawsMeta$latitude[1])
      burnHRS$LONGITUDE<-as.numeric(rawsMeta$longitude[1])
      burnHRS$STA_NAME<-rawsMeta$sta_nm[1]
      #burnHRS$PSA_NAME<-sw_rawsDF$PSA_NAME[i]
      burnHRS$StationNum<-stnID
      
      # add in doy
      burnHRS$doy<-as.numeric(format(burnHRS$date, "%j"))
      burnHRS$day<-as.numeric(format(burnHRS$date, "%d"))
      burnHRS$year<-as.numeric(format(burnHRS$date, "%Y"))
      burnHRS$month<-as.numeric(format(burnHRS$date, "%m"))
      
      # put into list
      burnList[[k]]<-burnHRS
      k=k+1
      # print loop update
      print(paste0(stnName,", ",i," out of ",length(file_list)))
    }
}

save(burnList, file = "/home/crimmins/RProjects/BurnPeriodTracker/burnList.RData")
#####

