## Google Analytics
# Author: Yekun
# Date: 2017-10-05

################
# load package #
################

library(RGA)
library(googlesheets)
library(rmongodb)
library(dplyr)
library(stringr)
library(lubridate)
library(rjson)
library(shiny)
library(plyr)
library(ggplot2)
library(magrittr)
library(data.table)

prevM <-function(x){7 * floor(as.numeric(x-1+4) / 7) + as.Date(1-4,origin = "1970-01-01")}

##################################
# Get Mongo Customer Information #
##################################

# establish connection mongo db
mongo=mongo.create(host = "mongo-03.dev.quantifind.com")
mongo.is.connected(mongo)


customers = data.frame(stringsAsFactors = FALSE)
cursor = mongo.find(mongo, "signum_users.customers")
## create the counter
i = 1
## iterate over the cursor
while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = data.frame(customer_id=mongo.oid.to.string(tmp$`_id`),
                      customer_name=tmp$name,
                      enabled=tmp$enabled)
  # bind to the master dataframe
  customers = rbind.fill(customers, tmp.df)
  # to print a message, uncomment the next 2 lines cat('finished game ', i,
  # '\n') i = i +1
}

## create the empty data frame
users = data.frame(stringsAsFactors = FALSE)
cursor = mongo.find(mongo, "signum_users.users")
## create the counter
i = 1
## iterate over the cursor
while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = data.frame(user_id=mongo.oid.to.string(tmp$`_id`),
                      customer_id=mongo.oid.to.string(tmp$`_customer`),
                      name=paste(tmp$name$first,tmp$name$last),
                      email=tmp$email,
                      created_date=tmp$createdAt,
                      updated_date=tmp$updatedAt)
  # bind to the master dataframe
  users = rbind.fill(users, tmp.df)
  # to print a message, uncomment the next 2 lines cat('finished game ', i,
  # '\n') i = i +1
}
mongoUserMetaData = left_join(users,customers,by = c("customer_id" = "customer_id"))


######################################
#  Get Signum Google Analystics Data # 
######################################

# get access token
authorize()

# get a GA profiles
gaProfiles <- list_profiles()
# choose the profile ID by site URL
id <- as.numeric(gaProfiles[grep("http://signum.quantifind.com",gaProfiles$websiteUrl),"id"])
# get date when GA tracking began
firstDate <- firstdate(as.numeric(id))

#### get page data ####
gaPageData = get_ga(id,start.date=firstDate,end.date="today",
                    dimensions = c("ga:dimension1","ga:dateHourMinute","ga:pagePath","ga:pagePathLevel1","ga:pageTitle"),
                    metrics = c("ga:pageviews","ga:timeOnPage"))

gaPageReport = left_join(gaPageData,mongoUserMetaData,by=c("dimension1"="user_id")) %>%
  filter(customer_name != "Disabled") %>%
  filter(!(customer_name != "Quantifind" & str_detect(email,"@quantifind.com"))) %>%
  transmute(customer_name=customer_name,
            user_id=dimension1,
            user_name=name,
            user_email=email,
            date_time=as.character(strptime(dateHourMinute,"%Y%m%d%H%M")),
            date=as.Date(strptime(dateHourMinute,"%Y%m%d")),
            week=prevM(as.Date(strptime(dateHourMinute,"%Y%m%d"))),
            product=str_replace_all(pageTitle,"Signum \\| ",""),
            path=pagePath,
            time_on_page_minutes=timeOnPage/60)


#### get session data
gaSessionData = get_ga(id, start.date = firstDate, end.date= "today",
                       dimensions = c("ga:dimension1","ga:date"),
                       metrics = c("ga:uniquePageviews","ga:sessionDuration","ga:pageviewsPerSession","ga:sessions"))

gaSessionReport = left_join(gaSessionData,mongoUserMetaData,by=c("dimension1"="id")) %>%
  filter(customer_name != "Disabled") %>%
  transmute(customer=customer_name,
            user_id=dimension1,
            user_name=name,
            date=date,
            sessions=sessions,
            page_views=uniquePageviews,
            session_duration_minutes=sapply(sessionDuration/60,function(x)round(x,digits=5)))

#### get usage meta data
gaMetaData = get_ga(id, start.date = firstDate, end.date= "today",
                    dimensions = c("ga:dimension1",
                                   "ga:dateHourMinute",
                                   "ga:region",
                                   "ga:city",
                                   "ga:browser",
                                   "ga:operatingSystem",
                                   "ga:browserVersion"),
                    metrics = c("ga:pageviews"))

gaMetaReport = gaMetaData %>%
  transmute(user_id=dimension1,
            date=as.Date(strptime(dateHourMinute,format="%Y%m%d%H%M"),format="%Y-%m-%d"),
            hour=format(strptime(dateHourMinute,format="%Y%m%d%H%M"),format="%H:%M"),
            #country=country,
            browser_version=browserVersion,
            region=region,
            city=city,
            browser=browser,
            operating_system=operatingSystem,
            last_update=Sys.time())



######################################### 
# Upload Usage Report to a Google Sheet #
#########################################

wsName = "Signum Usage 2.0"
# wsKey = "15tFcneYBws17_v8xHTjCDn6-T3GIOJXSYZvIBwVfgzk"
# wb = gs_key(wsKey)
wb = gs_title(wsName)
startDate="2018-01-01"

# backing up current data
presentData = wb %>% gs_read(ws="raw_data") 
write.csv(presentData,
          paste0("~/Desktop/google_analytics/data_backup/",as.Date(Sys.time()),".csv"),
          row.names = FALSE)

# writting new data to Google sheet
write.csv(gaPageReport,file=paste0("~/Desktop/google_analytics/new_reports/",Sys.Date(),".csv"),row.names=FALSE)

# t1=proc.time()

# wsName = "Signum Usage 2.0"
# wb = gs_title(wsName)
# wb %>% gs_edit_cells(ss=.,ws="data", input=gaUsageReport %>% filter(date>=startDate),
#                      anchor="A1",trim=TRUE)
# t2=proc.time()

# (t2-t1)


