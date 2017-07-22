x<-c("dplyr",'ggplot2','data.table','tidyverse','stringr')
lapply(x, require, character.only=T)
setwd('C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data')

Original=fread('calibration2004_2009_scaled_noneg.csv') %>%
	data.frame() %>%
	mutate(fips = str_sub(paste0('0', fips) , -5, -1), date = as.Date(date, format = '%m/%d/%Y'))

length(unique(Original$fips))

load("K:\\AirData\\OriginalData\\PM25_Data_20160120.RData") #PM25_AQS

PM25_AQS2= mutate(PM25_AQS,fips= substr(FIPSPOC,1,5)) %>%
	group_by(fips, Date) %>%
	summarize(PM25.monitor = mean(PM25_Value, na.rm = TRUE)) %>%
	rename(date = Date) %>%
	data.frame()

df = left_join(Original, PM25_AQS2, by = c('fips', 'date'))

df2 = filter( df, !is.na(PM25.monitor))

fips.list = unique( df2$fips)

df3=filter( df, fips %in% fips.list)

cor (df2$totalpm, df2$PM25.monitor)

cor (df2$fitted, df2$PM25.monitor)