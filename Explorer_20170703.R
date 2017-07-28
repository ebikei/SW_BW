x<-c("dplyr",'ggplot2','data.table','tidyverse','stringr')
lapply(x, require, character.only=T)
setwd('C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data')

Original=fread('calibration2004_2009_scaled_noneg.csv') %>%
	data.frame() %>%
	mutate(fips = str_sub(paste0('0', fips) , -5, -1), date = as.Date(date, format = '%m/%d/%Y'))
length(unique(Original$fips))

# Import SW111 definition 
load('SW111dates.Rdata') #s
rownames(s) = c(1:dim(s)[1])
s$date = as.Date(s$date, '%Y-%m-%d')
s$fips = str_sub(paste0('0', s$fips) , -5, -1)

Original = inner_join(Original, s, by = c('fips','date')) %>%
	select(-id)


load("C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data\\PM25_Data_20170727.RData") #PM25_AQS
PM25_AQS$PM25_Value[PM25_AQS$PM25_Value < 0] = 0

PM25_AQS2 = PM25_AQS %>%
	mutate(Monitor = substr(FIPSPOC, 1, 9), 
		PM25_Value = ifelse(PM25_Value > 750, PM25_Value / 1000, PM25_Value)) %>%
	filter(Date>'2003-05-31',Date<'2010-08-01')
PM25_AQS3 = aggregate(PM25_Value ~ Date+Monitor,mean, data = PM25_AQS2)	
PM25_AQS4= mutate(PM25_AQS3 , fips = substr(Monitor, 1, 5)) %>%
	group_by(fips, Date) %>%
	summarize(PM25 = mean(PM25_Value, na.rm = TRUE)) %>%
	rename(date = Date) %>%
	data.frame()
PM25_AQS4$n = sequence(rle(PM25_AQS4$fips)$lengths)
First_Date = data.frame(PM25_AQS4[!duplicated(PM25_AQS4$fips), c('fips', 'date')])
names(First_Date)[2] = 'FirstObsDate'
Last_Date = data.frame(PM25_AQS4[!duplicated(PM25_AQS4$fips, fromLast=TRUE),c('fips', 'date', 'n')])
names(Last_Date)[2] = 'LastObsDate'
Obs_List = merge(First_Date,Last_Date,by='fips') %>%
		mutate(PeriodLength = as.numeric(LastObsDate - FirstObsDate), Freq = PeriodLength/n) %>%
		filter(n>450,PeriodLength>2000)
MonList = unique(Obs_List$fips)
PM25_AQS5 = filter(PM25_AQS4, fips %in% MonList) %>%
	data.table()
setkey(PM25_AQS5,fips,date)
PM25_AQS6 = PM25_AQS5[CJ(unique(fips), seq(min(date), max(date), by = 1))] %>%
	data.frame() %>%
	select(-n)

StateList = c('53','41','06','32','04','30','16','56','49','35','08')

df = right_join(Original, PM25_AQS6, by = c('fips', 'date'))
df2 = filter( df, !is.na(CDID)) #Pick up forest fire data available counties
df3 = filter(df, fips %in% unique(df2$fips))  %>% #narrow down data to counties which have forest fire data
	filter(substr(fips,1,2) %in% StateList)
#df4 = filter( df3, !is.na(CDID))#,!is.na( PM25))

cor (df3$PM25monitor, df3$PM25,use = "na.or.complete")
cor (df3$totalpm, df3$PM25,use = "na.or.complete")

SW.Data=df3
#save(SW.Data,file='C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data\\SWData.RData')

