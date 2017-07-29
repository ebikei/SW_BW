x<-c("dplyr",'sp','rgdal','ggplot2','tidyverse')
lapply(x, require, character.only=T)
setwd('K:\\AirData\\UCDavisData\\GIS')

# Study area
StateList = c('53','41','06','32','04','30','16','56','49','35','08')

County=readOGR("K:\\GISData\\USCounty", "gz_2010_us_050_00_5m")
County2=County[County@data$STATE %in% StateList,]
plot(County2,col='white',lwd=1.5,main='Map')

County2@data$fips=paste(County2@data$STATE,County2@data$COUNTY,sep='')
load('C:\\Users\\kebisu\\Documents\\Research\\SW_Birth\\Data\\SWData.RData') #SW.Data

SW.event = SW.Data %>% 
	mutate(year = paste('Year', substr(date, 1, 4),sep='')) %>%
	group_by(fips, year) %>%
	summarize(N = sum(SW111,na.rm=TRUE)) %>%
	arrange(year, fips) %>% 
	spread(year, N) %>%
	select(-Year2003, -Year2010) 

County3=merge(County2,SW.event, by='fips')
	
plot(County3,col='white',lwd=1.5,main='Map',col=County3$Year2004)

map=fortify(County3)
ggplot(map,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(),color="grey20")


###############################

Neighborhoods <- fortify(County2)

ggmap()+geom_polygon(aes(x=long, y=lat), fill='grey', size=.2,color='green', data=Neighborhoods, alpha=0)

> library(rgdal)
> library(ggplot2)
> sfn = readOGR(".","sfzipcodes") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
> ggplot(data = sfn, aes(x = long, y = lat, group = group)) + geom_path()