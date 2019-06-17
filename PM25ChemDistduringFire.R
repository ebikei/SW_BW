x<-c("dplyr","ggplot2","data.table", "skimr")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)


############Download AQS Site
######################
PM25_AQS=data.frame(matrix(nrow=0,ncol=0))
PM25_Monitor=data.frame(matrix(nrow=0,ncol=0))
test2=c(2015:2018)

# Load Parameter List; Pick up species I want to DL.
## Note this list is created by downloading all year and take out parameter name.
## Code is not saved, so if need to update list, create code from scratch.

load('K:\\AirData\\OriginalData\\PM25Spec_List.RData') #PM25Spec_List
ex_list=c('Ambient Max Temperature','Ambient Min Temperature','Ambient Temperature',
          'EC1 PM2.5 LC','EC2 PM2.5 LC','Sample Baro Pressure','Sample Max Baro Pressure',
          'EC3 PM2.5 LC','EC CSN_Rev Unadjusted PM2.5 LC TOR','Sample Min Baro Pressure',
          'EC CSN_Rev Unadjusted PM2.5 LC TOT','EC1 CSN_Rev Unadjusted PM2.5 LC',
          'EC2 CSN_Rev Unadjusted PM2.5 LC','EC3 CSN_Rev Unadjusted PM2.5 LC',
          'OC CSN Unadjusted PM2.5 LC TOT','OC CSN_Rev Unadjusted PM2.5 LC TOR',
          'OC CSN_Rev Unadjusted PM2.5 LC TOT','OC1 CSN Unadjusted PM2.5 LC TOT',
          'OC1 CSN_Rev Unadjusted PM2.5 LC','OC1 PM2.5 LC','OC2 CSN Unadjusted PM2.5 LC TOT',
          'OC2 CSN_Rev Unadjusted PM2.5 LC','OC2 PM2.5 LC','OC3 CSN Unadjusted PM2.5 LC TOT',
          'OC3 CSN_Rev Unadjusted PM2.5 LC','OC3 PM2.5 LC','OC4 CSN Unadjusted PM2.5 LC TOT',
          'OC4 CSN_Rev Unadjusted PM2.5 LC','OC4 PM2.5 LC','OCX Carbon PM2.5 LC',
          'OP CSN PM2.5 LC TOT','OP CSN_Rev Unadjusted PM2.5 LC TOR','OP CSN_Rev Unadjusted PM2.5 LC TOT',
          'OP PM2.5 LC TOR','OP PM2.5 LC TOT','Optical EC PM2.5 LC TOT','UV Carbon PM2.5 LC',
          'Samarium PM2.5 LC','Rubidium PM2.5 LC','Phosphorus PM2.5 LC','Antimony PM2.5 LC','Beryllium PM2.5 LC',
          'Gallium PM2.5 LC','Indium PM2.5 LC','Iridium PM2.5 LC','Lanthanum PM2.5 LC','Molybdenum PM2.5 LC',
          'Niobium PM2.5 LC','Tantalum PM2.5 LC','Terbium PM2.5 LC','Tungsten PM2.5 LC','Yttrium PM2.5 LC',
          'Zirconium PM2.5 LC')
PM25Spec_List_filter=PM25Spec_List[! PM25Spec_List %in% ex_list]
Outside_main=c('02','15','66','72','78','80')

PM25_Spec_Data=data.frame()
ptm <- proc.time()
for (i in 1:length(test2)){  
     tryCatch({
          
          url=paste("https://aqs.epa.gov/aqsweb/airdata/daily_SPEC_",test2[i],".zip",sep='')
          download.file(url,'temp2.zip')
          temp=read.csv(unz('temp2.zip',paste("daily_SPEC_",test2[i],".csv",sep='')),header=TRUE) %>%
               filter(State.Code!='CC') %>%
               mutate(State.Code=sprintf("%02d",as.numeric(as.character(State.Code)))) %>%
               mutate(FIPS=paste(sprintf("%02d",as.numeric(as.character(State.Code))),sprintf("%03d",as.numeric(as.character(County.Code))),sprintf("%04d",as.numeric(as.character(Site.Num))),sep=''),
                      FIPS_POC=paste(FIPS,sprintf("%02d",as.numeric(as.character(POC))),sep='')) %>%
               filter(!State.Code %in% Outside_main, !Parameter.Name %in% ex_list) %>%
               mutate(Date=as.Date(as.character(Date.Local),format="%Y-%m-%d"),ParameterName=as.character(Parameter.Name)) %>%
               select(FIPS_POC,Date,Parameter.Code,ParameterName,Arithmetic.Mean, Latitude, Longitude)
          
          temp$FIPS_POC[substr(temp$FIPS_POC,1,5)=='12086']=paste('12025',substr(temp$FIPS_POC[substr(temp$FIPS_POC,1,5)=='12086'],6,11),sep='')
          temp=arrange(temp,ParameterName,FIPS_POC,Arithmetic.Mean) %>%
               mutate(willdelete=paste(ParameterName,FIPS_POC,sep=''))
          temp$Numbering=sequence(rle(c(temp$willdelete))$lengths)
          temp=select(temp,-willdelete)
          
          PM25_Spec_Data=rbind(PM25_Spec_Data, temp)
          rm(temp, url)
     }, error=function(e){})
}
proc.time() - ptm #This takes about 35min

Oct_List = c(seq(as.Date("2015-10-01"), as.Date('2015-10-31'), by = '1 day'), 
             seq(as.Date("2016-10-01"), as.Date('2016-10-31'), by = '1 day'),
             seq(as.Date("2017-10-01"), as.Date('2017-10-31'), by = '1 day'))
FirePeriod = seq(as.Date("2017-10-09"), as.Date('2017-10-18'), by = '1 day')
County = c('001', '013', '041', '055', '075', '081','085','095', '067', '113')

temp0 = PM25_Spec_Data %>% 
     filter(ParameterName != 'Average Ambient Pressure', ParameterName != 'Average Ambient Temperature', substr(ParameterName, 1, 6) != 'Sample') %>% 
     filter(substr(FIPS_POC, 1, 2) == '06') %>% 
     filter(substr(FIPS_POC, 3, 5) %in% County) %>% 
     filter(Date %in% Oct_List) 

temp0.1 = group_by(temp0, FIPS_POC, ParameterName) %>%
     count() %>% 
     filter(n > 20) %>%
     ungroup() %>%
     distinct(FIPS_POC)

temp1 = temp0 %>%   
     filter(FIPS_POC %in% temp0.1$FIPS_POC) %>%   
     mutate(FireDays = as.factor(ifelse(Date %in% FirePeriod, 'Fire', 'NonFire')))  %>%
     arrange(FIPS_POC, ParameterName, Arithmetic.Mean)


temp1 %>%
     group_by(ParameterName, FireDays) %>% 
     summarize(Value = mean(Arithmetic.Mean))  %>%
     ungroup() %>%
     spread(FireDays, Value) %>% 
  #   rename(NonFire = '0', Fire = '1') %>%
     filter(NonFire > 0 & Fire > 0) %>%
     print(n = Inf)

pol.list = c('Aluminum PM2.5 LC', 'Ammonium Ion PM2.5 LC', 'Chlorine PM2.5 LC', 'EC PM2.5 LC TOR', 'Iron PM2.5 LC', 'OC PM2.5 LC TOR', 'Potassium PM2.5 LC', 'Silicon PM2.5 LC', 'Sodium Ion Pm2.5 LC', 'Titanium PM2.5 LC', 'Total Nitrate PM2.5 LC', 'Zinc PM2.5 LC')

temp1 %>%
     filter(Arithmetic.Mean > 0, ParameterName %in% pol.list) %>%
     ggplot(aes(x = FireDays, y = Arithmetic.Mean)) + #, fill = FireDays)) +
          facet_wrap(~ ParameterName, scales = "free") +
          geom_boxplot() +# fatten = 2) +  
          theme_bw() +
          {}     
