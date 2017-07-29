x<-c("dplyr",'ggplot2','data.table','tidyverse','stringr','foreign')
lapply(x, require, character.only=T)
setwd('C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data')

# Import SW111 definition 
load('SW111dates.Rdata') #s

table = s %>%
	mutate(year = substr(date,1,4)) %>%
	group_by(fips, year) %>%
	summarize(N = sum(SW111,na.rm = TRUE)) %>%
	arrange(year, fips) %>% 
	spread(year, N) %>%
	data.frame() %>%
	mutate(fips2 = str_sub(paste0('0',fips),-5,-1)) %>%
	select(fips2, X2004, X2005, X2006, X2007, X2008, X2009) %>%
	rename(fips = fips2)

write.dbf(table, 'K:\\Research\\SW_Birth\\GIS\\SWEvent.dbf')	
	