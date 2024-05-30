# BP TRACKER ARCHIVE VERSION ---- creates historic plots ----
# adapted from BurnPeriodTracker2_beta.R
#
# Burn Period Tracker with added stations and updated climatology method
# adapted from BurnPeriod_tracker_wFcst.R
# MAC 02/28/23, V2.0
# MAC 06/16/23, V2.1
# experimental version with updates to forecast plots, forecast table...
#####
# To do: fix disconnect between observed and forecasted burn period, 
#####

print(Sys.Date())

ptm <- proc.time()

library(rgdal)
library(tidyr)
library(dplyr)
library(stringr)
library(XML)
library(RCurl)
library(ggplot2)
library(scales)
library(magick)
library(RColorBrewer)

# DEAL WITH PANDOC ERROR
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

# ggplot inset data
states <- map_data("state")

#####
# load spatial data
# psa zones
psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/BurnPeriodTracker/shapes", layer="National_PSA_Current")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")
# get psa centroids for factor order
sw_psaDF<- cbind.data.frame(sw_psa, rgeos::gCentroid(sw_psa,byid=TRUE))
#####

##### load data from burnPeriodClimo.R ----
# load in RAWS data
load("/home/crimmins/RProjects/BurnPeriodTracker/data/burnClimoList.RData")
#####

# create page data list
pageData<-list()
# list of errors
errorList<-list()
# list for maps
mapList<-list()

# stations<-c()
# for(i in 1:length(burnList)){
#   stations[i]<-burnList[[i]]$STA_NAME[1]
# }
# which(stations=="BOSQUE")

# set years to loop through 

# WIMS available back to 2014
yrs<-c(2014:2014)

for(l in 1:length(yrs)){
  arcYR<-yrs[l]
  # create year dirs in archive
  dir.create(paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR),showWarnings = FALSE)
  dir.create(paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR,"/plots/"),showWarnings = FALSE)
  dir.create(paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR,"/plots/plotly/"),showWarnings = FALSE)
  
    #####
    # loop through list and make plots
    for(i in 1:length(burnList)){
      # develop daily climo for station in list
      temp<-burnList[[i]]
      
      # build plot label
      plotTitle<-paste0(arcYR," ",sub("_", " ", temp$STA_NAME[1]), " Daily Burn Period Tracker (Climatology period of record: ",min(temp$year),"-",max(temp$year),")")
      
      dayQuantiles<- temp %>% group_by(doy) %>% summarise(
        q05 = quantile(rh_lt_20,0.05,na.rm='TRUE'),
        q50 = quantile(rh_lt_20,0.50,na.rm='TRUE'),
        q95 = quantile(rh_lt_20,0.95,na.rm='TRUE'),
        min = min(rh_lt_20,na.rm='TRUE'),
        max = max(rh_lt_20,na.rm='TRUE'),
        avg = mean(rh_lt_20,na.rm='TRUE'),
        n = n())
      
      # moving average
      dayQuantiles$rollAvg<-zoo::rollmean(dayQuantiles$avg,14,fill=NA, align = 'center')
      # pad NAs
      dayQuantiles<-tidyr::fill(dayQuantiles, rollAvg, .direction = "downup")
      
      # get recent RAWS data
      url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-",arcYR,"&end=31-Dec-",arcYR,"&time=&sort=&ndays=")
      # past year
      #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
      tryCatch({
      xData <- getURL(url)
      xmldoc <- xmlParse(xData)
      currYear <- xmlToDataFrame(xData)
      # if no data, skip
      if(nrow(currYear)!=0){
        # correct date field
        currYear$date<-as.Date(currYear$obs_dt,format="%m/%d/%Y")
        currYear$doy<-format(currYear$date,"%j")
        # get current Year numeric
        yr<-((format(currYear$date[1],"%Y")))
        
        # current year burn hours
        currBurnHRS<-  currYear %>%
          group_by(date) %>%
          summarize(n_hours = n(),
                    rh_lt_20 = sum(as.numeric(as.character(rh)) <= 20, na.rm = TRUE),
                    minRH = min(as.numeric(as.character(rh_min)), na.rm = TRUE),
                    maxRH = max(as.numeric(as.character(rh_max)), na.rm = TRUE))
        currBurnHRS$doy<-format(currBurnHRS$date,"%j")
        currBurnHRS$doy<-as.numeric(currBurnHRS$doy)
        # set missing days to NA
        currBurnHRS$rh_lt_20<-ifelse(currBurnHRS$n_hours<22,NA,currBurnHRS$rh_lt_20)
        
        # add in dummy dates
        dumYr<-as.numeric(format(Sys.Date(),"%Y"))
        dayQuantiles$date<-as.Date(paste0(dumYr,dayQuantiles$doy),format="%Y %j")
        #currBurnHRS$date<-as.Date(paste0(2016,currBurnHRS$doy),format="%Y %j")
        currBurnHRS$date<-as.Date(paste0(dumYr,format(currBurnHRS$date,"%m"),
                                         format(currBurnHRS$date,"%d")),format="%Y%m%d")
        #temp$dummyDate<-as.Date(paste0(2016,temp$doy),format="%Y %j")
        currBurnHRS$roll_rh_hrs<-zoo::rollmean(currBurnHRS$rh_lt_20,10,fill=NA, align = 'right')
        
        # better names for Plotly labels
        colnames(dayQuantiles)[c(2,4,9)]<-c("q5th_percentile","q90th_percentile","mean")
          dayQuantiles$mean<-round(dayQuantiles$mean,1)
        colnames(currBurnHRS)[c(3,7)]<-c("Burn_hours","avg_10days")
        
        #####
        # plot with legend data
        currBurnHRS$var<-"Observed"
         tempBurnHrs<-currBurnHRS[,c("date","Burn_hours","var")]
         colnames(tempBurnHrs)<-c("date","hours","var")
        # fcst_bhrs$var<-"Forecast"
        #  tempFcstHrs<-fcst_bhrs[,c("date","Burn_hours_forecast","var")]
        #  colnames(tempFcstHrs)<-c("date","hours","var")
        tempDay10avg<-currBurnHRS[,c("date","avg_10days")]
         tempDay10avg$var<-"10-day Avg"
         colnames(tempDay10avg)<-c("date","hours","var")
        #plotData<-rbind.data.frame(tempBurnHrs,tempFcstHrs,tempDay10avg)
        plotData<-rbind.data.frame(tempBurnHrs,tempDay10avg)
        
    
        #####
        
        # make the plot
        barWidth<-1
        p1<-ggplot(dayQuantiles,aes(date,mean))+
          theme_bw()+
          #theme(plot.background = element_blank(),
          #      panel.grid.minor = element_blank(),
          #      panel.grid.major = element_blank(),
          #      panel.border = element_blank(),
          #      panel.background = element_blank()) +
          geom_line(colour='grey',size=0.5)+
          geom_linerange(dayQuantiles, mapping=aes(x=date, ymin=q5th_percentile, ymax=q90th_percentile), colour = "grey83",alpha=0.4, size=barWidth, show.legend = NA)
        # p<-p1 + geom_line(data=currBurnHRS,aes(date,Burn_hours, color=yr), size=0.5) +
        #   scale_colour_manual(values=c("red"),name='Year')+
        #   theme(legend.position=c(0.92,0.75),
        #         legend.title=element_blank(),
        #         legend.background = element_rect(fill=alpha('white', 0)))+
        #   #scale_y_discrete(name ="Burn period (hrs)", 
        #   #                 limits=c(0,4,8,12,16,20,24))+
        #   scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
        #   
        #   scale_x_date(labels = date_format("%b"), date_breaks = "1 month")+
        #   ggtitle(plotTitle)
        
        p<-p1 + geom_line(data=plotData,aes(date,hours, color=var,linetype=var), size=0.5) +
          scale_colour_manual(values=c("darkorange4","red"))+
          scale_linetype_manual(values=c("dashed", "solid","solid"))+
          theme(legend.position=c(0.95,0.74),
                legend.title=element_blank(),
                legend.background = element_rect(fill=alpha('white', 0)))+
          #scale_y_discrete(name ="Burn period (hrs)", 
          #                 limits=c(0,4,8,12,16,20,24))+
          scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24),expand = c(0.01,0))+
          
          scale_x_date(labels = date_format("%b"), date_breaks = "1 month", expand = c(0,0))+
          ggtitle(plotTitle)
        
        # p<-p + geom_line(data=currBurnHRS,aes(date,avg_10days), size=0.5,linetype = "dashed", color="darkorange4") +
        #   #scale_colour_manual(values=c("red"),name='Year')+
        #   theme(legend.position=c(0.92,0.75),
        #         legend.title=element_blank(),
        #         legend.background = element_rect(fill=alpha('white', 0)))+
        #   #scale_y_discrete(name ="Burn period (hrs)", 
        #   #                 limits=c(0,4,8,12,16,20,24))+
        #   scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
        #   
        #   scale_x_date(labels = date_format("%b"), date_breaks = "1 month")+
        #   ggtitle(plotTitle)
        
        # add in forecast line
        #p<-p + geom_line(data=fcst_bhrs, aes(date,Burn_hours_forecast), size=0.5, color="forestgreen")
        
        
        # interactive plot
        pLy<-plotly::ggplotly(p)
        htmlwidgets::saveWidget(pLy, paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR,"/plots/plotly/",temp$STA_NAME[1],"_BurnPeriod.html"))
        
        # INSET MAP - OPTIONAL
        # point<-as.data.frame(t((out$meta$ll)))
        # # inset map:
        # zoomLev<-5
        sw_psa_df<-fortify(sw_psa)
        stationLatLon<-temp[1,c("LATITUDE","LONGITUDE")]
        insetmap<-ggplot() +
          geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
          geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey", alpha=0.8)  + # get the state border back on top
          #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
          coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
          geom_point(data = stationLatLon, aes(x = LONGITUDE, y = LATITUDE), size=0.75, color='red')+
          theme_bw(base_size=5)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
        
        g <- ggplotGrob(insetmap)
        p<-p + annotation_custom(grob = g, xmin = as.Date(paste0(dumYr,"-11-19")), xmax = Inf, ymin = 20, ymax = Inf)+
          labs(caption=paste0("Updated: ",format(Sys.time(), "%Y-%m-%d")," (current through ",format(currBurnHRS$date[max(which(is.na(currBurnHRS$Burn_hours)==TRUE))-1], "%m-%d"),")",
                              "\nBurn Period is total hours/day with RH<20%\n10-day moving avg(dashed line);\nClimatology represents daily smoothed mean and range\n of values between 5th and 95th percentiles\nRAWS Data Source: famprod.nwcg.gov & cefa.dri.edu"))
        
        
        # write out file
        png(paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR,"/plots/",temp$STA_NAME[1],"_BurnPeriod.png"), width = 12, height = 6, units = "in", res = 300L)
        #grid.newpage()
        print(p, newpage = FALSE)
        dev.off()
        
        # add logos
        # Call back the plot
        plot <- image_read(paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR,"/plots/",temp$STA_NAME[1],"_BurnPeriod.png"))
        # And bring in a logo
        logo_raw <- image_read("/home/crimmins/RProjects/BurnPeriodTracker/CLIMAS_UACOOP_SWCC_horiz.png") 
        logo <- image_resize(logo_raw, geometry_size_percent(width=70,height = 70))
        # Stack them on top of each other
        #final_plot <- image_append((c(plot, logo)), stack = TRUE)
        #final_plot <- image_mosaic((c(plot, logo)))
        final_plot <- image_composite(plot, logo, offset = "+210+1560")
        # And overwrite the plot without a logo
        image_write(final_plot, paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR,"/plots/",temp$STA_NAME[1],"_BurnPeriod.png"))
        # ----
        
        # # # plotly interactive of all years
        # temp2<-temp
        # temp2$dummyDate<-as.Date(paste0(2016,temp$doy),format="%Y %j")
        # colnames(currBurnHRS)[1]<-"dummyDate"
        # currBurnHRS$year<-as.numeric(yr)
        # temp2<-rbind(currBurnHRS,temp2[,c("dummyDate","n_hours","rh_lt_20","minRH","maxRH","doy","year")])
        # colnames(temp2)<-c("Date","n_hours","Burn Hours","minRH","maxRH","Day of year","Year")
        # temp2$Date<-format(temp2$Date, "%b-%d")
        # temp2$Year<-as.factor(temp2$Year)
        # 
        # # quantiles
        # temp3<-dayQuantiles[,c("doy","q05","q95","rollAvg","date")]
        #   colnames(temp3)<-c("Day of year","5th %tile","95th %tile","Avg","Date")
        #   temp3$Date<-format(temp3$Date, "%b-%d")
        # 
        # # color ramp
        #   colourCount = length(unique(temp2$Year))
        #   getPalette = colorRampPalette(brewer.pal(9, "Set1"))
        # 
        # barWidth<-1
        # p1<-ggplot(temp3,aes(`Day of year`,`Avg`))+
        #   theme_bw()+
        #   #theme(plot.background = element_blank(),
        #   #      panel.grid.minor = element_blank(),
        #   #      panel.grid.major = element_blank(),
        #   #      panel.border = element_blank(),
        #   #      panel.background = element_blank()) +
        #   geom_line(colour='grey',size=0.5)+
        #   geom_linerange(temp3, mapping=aes(x=`Day of year`, ymin=`5th %tile`, ymax=`95th %tile`, group=1, text=Date), colour = "grey83",alpha=0.4, size=barWidth, show.legend = NA)
        # 
        #  pLy<-p1+geom_line(data=temp2,aes(`Day of year`,`Burn Hours`, color=Year, group=1,text=Date), size=0.5)+
        #    scale_x_continuous(breaks=c(1,32,61,93,122,153,183,214,245,275,306,336),labels=temp3$Date[c(1,32,61,93,122,153,183,214,245,275,306,336)])+
        #    scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
        #    scale_color_manual(name="Year",values = getPalette(colourCount))+
        #    xlab("Day of Year")+
        #    ylab("Burn period (hrs)")+
        #    ggtitle(plotTitle)+
        #    theme_bw()
        #  pLy<-plotly::ggplotly(pLy)
        #  htmlwidgets::saveWidget(pLy, paste0("/home/crimmins/RProjects/FireClimate/BurnPeriodTracker/BurnTrackerPlots/plotly/",temp$STA_NAME[1],"_BurnPeriod.html"))
        # #######
        print(temp$STA_NAME[1])
        print(paste0(i, " of ",length(burnList)))
      
        # store data
        temp2<-cbind.data.frame(temp$STA_NAME[1],temp$StationNum[1],temp$LATITUDE[1],temp$LONGITUDE[1], temp$elev[1], currBurnHRS$Burn_hours[max(which(currBurnHRS$n_hours==24))])
        colnames(temp2)<-c("STA_NAME","StationNum","LATITUDE","LONGITUDE","Elevation","CurrBurnHours")
        pageData[[i]]<-temp2
        #
        
        # get station data for obs/forecast map
        #temp1<-currBurnHRS[currBurnHRS$date >= Sys.Date()-6 & currBurnHRS$date <= Sys.Date()-1,]
        
        # deal with limited data at New Year, added 1/3/24 ----
        if((nrow(currBurnHRS)-9)<1){
          stIdx<-1
        }else{
          stIdx<-(nrow(currBurnHRS)-9)
        }
        ######
        
      }
      else{
        
      }
      
      },  error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                            errorList[[i]]<-temp$STA_NAME[1]})
      
    }
    
    proc.time() - ptm
    
    # save environment for diagnostics
    save.image("~/RProjects/BurnPeriodTracker/burnPeriodEnv.RData")
    
    # #####
    # create map page
    library(leaflet)
    library(knitr)
    library(rmarkdown)
    library(rmdformats)
    library(DT)
    library(leaflet.esri)
    
    # DEAL WITH PANDOC ERROR
    Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
    
    # station metdata
    sw_rawsDF<-do.call(rbind, pageData)
    
    # remove ROOSEVELT - empty station
    #sw_rawsDF<-subset(sw_rawsDF, STA_NAME!="ROOSEVELT")
    
    # create links
    sw_rawsDF$fileName<-paste0(sw_rawsDF$STA_NAME,"_BurnPeriod.png")
    # plotly links
    sw_rawsDF$plotlyName<-paste0("./plotly/",sw_rawsDF$STA_NAME,"_BurnPeriod.html")
    # create data table
    #pageTable<-sw_rawsDF[,c(2,3,1,5,6,7,10,11,12,13,14)]
    pageTable<-sw_rawsDF
    #pageTable$dataLink<-paste0('https://www.weather.gov/wrh/timeseries?site=',pageTable$StationNum)
    
    
    # image link for table
    pageTable$imageLinks<-paste0('<a href="',sw_rawsDF$fileName,'"><img alt="Thumb" src="',sw_rawsDF$fileName,'"width=150" height="70"></a>')
    # data link <a href="https://www.w3schools.com">Visit W3Schools</a>
    #pageTable$STA_NAME<-paste0('<a href="',pageTable$dataLink,'">',pageTable$STA_NAME,'</a>')
    # plotly hyperlink
    pageTable$Interactive<-paste0('<a href="',sw_rawsDF$plotlyName,'">',sw_rawsDF$STA_NAME,'</a>')
    
    
    # pop up labels
    pageTable$popUpLabel<-paste0(pageTable$STA_NAME,": ",pageTable$CurrBurnHours," hrs")
    # pad name for constant width of popup
    maxWidth<-max(nchar(as.character(pageTable$popUpLabel)))
    for(k in 1:nrow(pageTable)){
      pageTable$pageTable$popUpLabel[k]<-paste0(pageTable$popUpLabel[k],paste(rep("&nbsp;",(maxWidth-nchar(as.character(pageTable$popUpLabel[k]))+5) ), sep="", collapse=""))
    }
    # or
    #pageTable$STA_NAME2<-stringr::str_pad(as.character(pageTable$STA_NAME), max(nchar(as.character(pageTable$STA_NAME))),
    #        side = c("right"), pad = "_")
    #
    stnLabs <- lapply(seq(nrow(pageTable)), function(i) {
      paste0( '<p> <b>', pageTable$imageLinks[i], '</b></p>',
              '<p>',pageTable$popUpLabel[i], '</p>')
    })
    
    # adjust table
    pageTable<-pageTable[,c("STA_NAME","StationNum","LATITUDE","LONGITUDE","Elevation","CurrBurnHours","imageLinks","Interactive")]
    colnames(pageTable)<-c("Name","Number","Latitude","Longitude","Elevation(ft)","Latest Burn Period","Tracker","Interactive")
    
    # create Website with markdown ----
    pageTitle<-paste0("Southwest U.S. Burn Period Tracker - ",arcYR)
    render(paste0('/home/crimmins/RProjects/BurnPeriodTracker/BurnPeriod_Markdown_historic.Rmd'), output_file='index.html',
           output_dir=paste0("/home/crimmins/RProjects/BurnPeriodTracker/archive/",arcYR,"/plots/"), clean=TRUE)
    
    # #####
    
    source('/home/crimmins/RProjects/BurnPeriodTracker/pushNotify.R')
    
    if(length(errorList)!=0){
      source('/home/crimmins/RProjects/BurnPeriodTracker/pushNotifyError.R')
    }

}
