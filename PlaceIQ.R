library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(gridExtra)
library(grid)
library(lattice)
library(lubridate)
options(scipen=999)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ma <- function(x,n=7){stats::filter(x,rep(1/n,n), sides=2)}

###### PlaceIQ Data
# Import raw brand timeseries of counts
rawBrandCounts=fread("~/Desktop/PlaceIQ/national_level.csv") %>%
  transmute(Date=as_date(Date,"%Y-%m-%d"),Brand,`Unique Devices`) %>%
  group_by(Date,Brand) %>% summarise(Visits=sum(`Unique Devices`)) %>%
  arrange(.,as.Date(Date, "%m/%d/%Y"),Brand)

ggplot(rawBrandCounts,aes(x=Date,y=Visits,group=Brand)) + 
  geom_line(aes(colour = factor(Brand)))

# Import raw gas station counts
gasStationCounts=fread("~/Desktop/PlaceIQ/GasStations.csv")%>%
  transmute(Visits,Date=as_date(Date,"%m/%d/%y"),DMA,Place,`DMA Name`) %>%
  group_by(Date) %>% summarise(Visits=sum(Visits))

ggplot(rawBrandCounts,aes(x=Date,y=Visits)) +
  geom_line()

# Timeseries Normalization
combTable_piq=left_join(rawBrandCounts,gasStationCounts,by=c("Date")) %>% ungroup() %>%
  transmute(Date,Brand,
            StoreVisits=Visits.x,
            GasVisit=Visits.y,
            MaxGasVisit=max(Visits.y),
            NormStoreVisits=Visits.x/(Visits.y))

ggplot(combTable_piq,aes(x=Date,y=NormStoreVisits,group=Brand)) + 
  geom_line(aes(colour = factor(Brand)))

###### NPD Data
tldToDf=function(brand,uri){
  raw=unlist(fromJSON(uri))
  dates=substr(names(raw),1,10)
  names(raw)=NULL
  rslt=data.frame(brand=brand,date=dates,raw)[-length(raw),]
  return(rslt)
}

combTable_npd=rbind(
  tldToDf("Applebees","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=npd&vertical=restaurant_aggregate&store.merchant=Applebee%27s&granularity=day"),
  tldToDf("Chilis","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=npd&vertical=restaurant_aggregate&store.merchant=Chili%27s&granularity=day"),
  tldToDf("CrackerBarrel","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=npd&vertical=restaurant_aggregate&store.merchant=Cracker%20Barrel&granularity=day"),
  tldToDf("Dennys","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=npd&vertical=restaurant_aggregate&store.merchant=Denny's&granularity=day"),
  tldToDf("IHOP","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=npd&vertical=restaurant_aggregate&store.merchant=IHOP&granularity=day"),
  tldToDf("Olive_Garden","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=npd&vertical=restaurant_aggregate&store.merchant=Olive%20Garden&granularity=day")
)

ggplot(combTable_npd,aes(x=date,y=scale(raw),group=brand)) + 
  geom_line(aes(colour = factor(brand)))

###### DE First-party
ihp_de=tldToDf("IHOP_de","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=dineequity&vertical=restaurant&store.merchant=ihop&granularity=day")
apb_de=tldToDf("Applebees_de","http://tld05-25.mtv.quantifind.com:3000/v1/item/sales?source=dineequity&vertical=restaurant&store.merchant=applebees&granularity=day")

###### Graphs
## Applebee's
apb_data=rbind(
  combTable_piq %>% filter(.,Brand=='Applebees') %>%
    transmute(date=Date,brand=Brand,counts=scale(NormStoreVisits),source="PlaceIQ"),
  apb_npd %>%
    transmute(date,brand,counts=scale(raw),source="NPD"),
  apb_de %>%
    transmute(date,brand,counts=scale(raw),source="DE"))

ggplot(filter(apb_data,source!="NPD",
              date > '2016-09-01',
              date < '2017-04-01'),
       aes(x=date,y=ma(counts)),group=source) + 
  geom_line(aes(colour = source)) + 
  ggtitle("Applebees 7-day Rolling Window")

# PlaceIQ X DE
apb_de_piq=inner_join(
  subset(combTable_piq,Brand=='Applebees') %>%
    transmute(date=Date,brand=Brand,counts=NormStoreVisits,source="PlaceIQ"),
  apb_de %>%
    transmute(date=as.Date(date,"%Y-%m-%d"),brand,counts=raw,source="DE"),
  by=c("date","date")
)

ggplot(apb_de_piq, aes(x=counts.x, y=scale(counts.y))) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("DE: KPI")+
  ggtitle(paste("Applebee's Daily Roll-up, Correlation: ",round(cor(apb_de_piq$counts.y,apb_de_piq$counts.x),4)))

# PlaceIQ X DE Weekly
apb_de_piq_week=apb_de_piq %>%
  mutate(year=lubridate::year(date),
         week_num=lubridate::week(date)) %>%
  group_by(brand.x,source.x,source.y,year,week_num) %>%
  summarise(piq_sum=sum(counts.x),de_sum=sum(counts.y))

x=filter(apb_de_piq_week,piq_sum>0.22)
ggplot(x, 
       aes(x=piq_sum, y=scale(de_sum))) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("DE: KPI")+
  ggtitle(paste("Applebee's Weekly Roll-up, Correlation: ",round(cor(x$de_sum,x$piq_sum),4)))

## IHOP
ihp_data=rbind(
  combTable_piq %>% filter(.,Brand=='IHOP') %>%
    transmute(date=Date,brand=Brand,counts=scale(NormStoreVisits),source="PlaceIQ"),
 ihp_npd %>%
    transmute(date,brand,counts=scale(raw),source="NPD"),
 ihp_de %>%
    transmute(date,brand,counts=scale(raw),source="DE"))

ggplot(filter(ihp_data,source!="NPD",
              date > '2016-09-01',
              date < '2017-04-01'),
       aes(x=date,y=ma(counts)),group=source) + 
  geom_line(aes(colour = source)) + 
  ggtitle("IHOP 7-day Rolling Window")

# PlaceIQ X DE
ihp_de_piq=inner_join(
  subset(combTable_piq,Brand=='IHOP') %>%
    transmute(date=Date,brand=Brand,counts=NormStoreVisits,source="PlaceIQ"),
  ihp_de %>%
    transmute(date=as.Date(date,"%Y-%m-%d"),brand,counts=raw,source="DE"),
  by=c("date","date")
)

ggplot(ihp_de_piq, aes(x=counts.x, y=scale(counts.y))) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("DE: KPI")+
  ggtitle(paste("IHOP Daily, Correlation: ",round(cor(ihp_de_piq$counts.y,ihp_de_piq$counts.x),4)))

# PlaceIQ X DE Weekly
ihp_de_piq_week=ihp_de_piq %>%
  mutate(year=lubridate::year(date),
         week_num=lubridate::week(date)) %>%
  group_by(brand.x,source.x,source.y,year,week_num) %>%
  summarise(piq_sum=sum(counts.x),de_sum=sum(counts.y))

x=filter(ihp_de_piq_week,piq_sum>0.2)

ggplot(x, aes(x=piq_sum, y=scale(de_sum))) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("NPD: KPI")+
  ggtitle(paste("IHOP Weekly Roll-up, Correlation: ",round(cor(x$de_sum,x$piq_sum),4)))


#########################################################################################################
## Applebee's
# PlaceIQ X NPD Daily
apb_npd_piq=inner_join(
  subset(combTable_piq,Brand=='Applebees') %>%
    transmute(date=Date,brand=Brand,counts=NormStoreVisits,source="PlaceIQ"),
  apb_npd %>%
    transmute(date=as.Date(date,"%Y-%m-%d"),brand,counts=raw,source="NPD"),
  by = c("date" = "date")
)

ggplot(apb_npd_piq, aes(x=counts.x, y=counts.y)) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("NPD: Revenue in Cents")+
  ggtitle(paste("Correlation: ",round(cor(apb_npd_piq$counts.y,apb_npd_piq$counts.x),4)))

# PlaceIQ X NPD Weekly
apb_npd_piq_week=apb_npd_piq %>%
  mutate(year=lubridate::year(date),
         week_num=lubridate::week(date)) %>%
  group_by(brand.x,source.x,source.y,year,week_num) %>%
  summarise(piq_sum=sum(counts.x),npd_sum=sum(counts.y))

ggplot(apb_npd_piq_week, aes(x=piq_sum, y=npd_sum)) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("NPD: Revenue in Cents")+
  ggtitle(paste("Weekly Roll-up, Correlation: ",
                round(cor(apb_npd_piq_week$npd_sum,
                          apb_npd_piq_week$piq_sum),4)))

## IHOP
# PlaceIQ X NPD Daily
ihp_npd_piq=inner_join(
  subset(combTable_piq,Brand=='IHOP') %>%
    transmute(date=Date,brand=Brand,counts=NormStoreVisits,source="PlaceIQ"),
  ihp_npd %>%
    transmute(date=as.Date(date,"%Y-%m-%d"),brand,counts=raw,source="NPD"),
  by = c("date" = "date")
)

ggplot(ihp_npd_piq, aes(x=counts.x, y=counts.y)) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("NPD: Revenue in Cents")+
  ggtitle(paste("Correlation: ",round(cor(ihp_npd_piq$counts.y,ihp_npd_piq$counts.x),4)))

# PlaceIQ X NPD Weekly
ihp_npd_piq_week=ihp_npd_piq %>%
  mutate(year=lubridate::year(date),
         week_num=lubridate::week(date)) %>%
  group_by(brand.x,source.x,source.y,year,week_num) %>%
  summarise(piq_sum=sum(counts.x),npd_sum=sum(counts.y))

ggplot(ihp_npd_piq_week, aes(x=piq_sum, y=npd_sum)) + 
  geom_point()+
  geom_smooth(method=lm)+
  xlab("PlaceIQ: Normalized")+
  ylab("NPD: Revenue in Cents")+
  ggtitle(paste("Weekly Roll-up, Correlation: ",
                round(cor(ihp_npd_piq_week$npd_sum,
                          ihp_npd_piq_week$piq_sum),4)))


## Chilis
chl_data=rbind(
  subset(combTable_piq,Brand=='Chilis') %>%
    transmute(date=Date,brand=Brand,counts=scale(NormStoreVisits),source="PlaceIQ"),
  chl_npd %>%
    transmute(date,brand,counts=scale(raw),source="NPD"))
ggplot(chl_data,aes(x=date,y=ma(counts)),group=source) + 
  geom_line(aes(colour = source))+ 
  ggtitle("Chilis")

## Cracker Barrel
crb_data=rbind(
  subset(combTable_piq,Brand=='CrackerBarrel') %>%
    transmute(date=Date,brand=Brand,counts=scale(NormStoreVisits),source="PlaceIQ"),
  crb_npd %>%
    transmute(date,brand,counts=scale(raw),source="NPD"))
ggplot(crb_data,aes(x=date,y=ma(counts)),group=source) + 
  geom_line(aes(colour = source))+ 
  ggtitle("CrackerBarrel")

## Denny's
dny_data=rbind(
  subset(combTable_piq,Brand=='Dennys') %>%
    transmute(date=Date,brand=Brand,counts=scale(NormStoreVisits),source="PlaceIQ"),
  dny_npd %>%
    transmute(date,brand,counts=scale(raw),source="NPD"))
ggplot(dny_data,aes(x=date,y=ma(counts)),group=source) + 
  geom_line(aes(colour = source))+ 
  ggtitle("Dennys")

## Olive Garden
olg_data=rbind(
  subset(combTable_piq,Brand=='Olive_Garden') %>%
    transmute(date=Date,brand=Brand,counts=scale(NormStoreVisits),source="PlaceIQ"),
  olg_npd %>%
    transmute(date,brand,counts=scale(raw),source="NPD"))
ggplot(olg_data,aes(x=date,y=ma(counts)),group=source) + 
  geom_line(aes(colour = source))+ 
  ggtitle("Olive_Garden")



grid.arrange(apb,chl,crb,dny,ihp,olg, ncol=2)
