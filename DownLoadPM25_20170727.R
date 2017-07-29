# This code is modified to include extreme value since the aim of this SW project is to explroer SW effect (extreme PM2.5 value.)
x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

######################
############Download AQS Site
######################
PM25_AQS=data.frame(matrix(nrow=0,ncol=0))
PM25_Monitor=data.frame(matrix(nrow=0,ncol=0))
test2=c(1990:2016)

ptm <- proc.time()
for (i in 1:length(test2)){  
	tryCatch({
	url=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_",test2[i],".zip",sep='')
	download.file(url,'temp2.zip')
	temp=read.csv(unz('temp2.zip',paste("daily_88101_",test2[i],".csv",sep='')),header=TRUE)
	names(temp)=c('StateCode','CountyCode','SiteID','Parameter','POC','Latitude','Longitude','Datum','Name','SampleDuration',
		'PollutantStandard','Date','Unit','EventType','ObsCount','ObsPercent','Value','MaxValue','MaxHour','AQI','MethodCode',
		'MethodName','SiteName','Address','StateName','CountyName','CityName','CBSAName','DateChange')
	temp$Value[temp$Unit=='Nanograms/cubic meter (LC)']=temp$Value/1000
	temp$Unit=as.character(temp$Unit)
	temp$Unit[temp$Unit=='Nanograms/cubic meter (LC)']='Micrograms/cubic meter (LC)'
	#obs_range=c(1,16:24)
	obs_range=c(1:24)
	temp=filter(temp,ObsCount %in% obs_range,Unit=='Micrograms/cubic meter (LC)')
	temp$Date=as.Date(as.character(temp$Date),format="%Y-%m-%d")
	temp2=temp[,c(1:7,12,13,15,17,20,25:27)]
	temp2=filter(temp2,StateCode!='CC')
	temp2$FIPS_C=paste(sprintf("%02d",as.numeric(as.character(temp2$StateCode))),sprintf("%03d",as.numeric(as.character(temp2$CountyCode))),sep='')
	temp2$FIPS_C[temp2$FIPS_C=='12086']='12025'
	temp2$FIPS=paste(temp2$FIPS_C,sprintf("%04d",temp2$SiteID),sep='')
	temp2$FIPSPOC=paste(temp2$FIPS,sprintf("%02d",temp2$POC),sep='')
	
	#Take average by Monitor, POC, and Date
	temp3=aggregate(Value~FIPSPOC+Date,temp2,mean,na.rm=TRUE)
	PM25_AQS=rbind(PM25_AQS,temp3)
	temp4=select(temp2,FIPSPOC,Latitude,Longitude) %>%
		distinct(FIPSPOC, .keep_all = TRUE)
	PM25_Monitor=rbind(PM25_Monitor,temp4)

	rm(url,temp,temp2,temp3,temp4)
	}, error=function(e){})
}
proc.time() - ptm #This takes about 6min

PM25_AQS=rename(PM25_AQS,PM25_Value=Value)

dim(PM25_AQS)
PM25_Monitor=distinct(PM25_Monitor,FIPSPOC, .keep_all = TRUE)
dim(PM25_Monitor)

##Take Out Off-mainland
Outside_main=c('02','15','66','72','78','80')
PM25_AQS=PM25_AQS[!(substr(PM25_AQS$FIPSPOC,1,2) %in% Outside_main),]
PM25_Monitor=PM25_Monitor[!(substr(PM25_Monitor$FIPSPOC,1,2) %in% Outside_main),]

PM25_AQS=arrange(PM25_AQS,FIPSPOC,Date)
PM25_Monitor=arrange(PM25_Monitor,FIPSPOC)

test=substr(PM25_AQS$Date,1,4)
table(test)
rm(test)

test=substr(PM25_Monitor$FIPSPOC,1,2)
table(test)
rm(test)

test=substr(PM25_Monitor$FIPSPOC,1,5)
table(test)
rm(test)

save(PM25_AQS,file="C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data\\PM25_Data_20170727.RData") #Units are in PPM
save(PM25_Monitor,file="C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data\\PM25_Monitor_20170727.RData") #Units are in PPM

rm(list=ls())

