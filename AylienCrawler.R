##################
# Aylien Crawler #
##################

## Loading libraries
library(jsonlite)
library(httr)
library(data.table)
library(stringr)
library(rlist)
library(dplyr)


#########################
# ----- FUNCTIONS ----- #
#########################

##########################
# 1. Pagination Crawling #
##########################

aylienPaging <- function(entity,category,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type) {
  ## Pagination. No more pages when cursor = previous pages' cursor
  ## Hard coded cluster=TRUE, cluster.algorithm=lingo, and per_page=100
  ## This function also saves a copy of raw JSON file
  
  # Functions
  aylienApi <- function(APPLICATION_ID, APPLICATION_KEY, endpoint, parameters,type) {
    ## Returns results from a single API call. Limit to 100 posts per page.
    
    url = paste0('https://api.newsapi.aylien.com/api/v1/',endpoint,"?")
    httpHeader = c(Accept="text/json", 'X-AYLIEN-NewsAPI-Application-ID' = APPLICATION_ID, 
                   'X-AYLIEN-NewsAPI-Application-Key'= APPLICATION_KEY, 
                   'Content-Type'="application/x-www-form-urlencoded; charset=UTF-8")
    paramEncode <- URLencode(parameters)
    url <- paste0(url,paramEncode)
    # print(url)
    tryCatch({
      resp <- GET(url, add_headers(httpHeader))
      resp <- content(resp, "text", encoding="UTF-8")
      resp <- jsonlite::fromJSON(resp,simplifyDataFrame = T,flatten = T)
      resp
    },
    error = function(cond){
      message(paste("error reading json:",url))
    },
    warning = function(cond){
      message(paste("error reading json:",url))
    },
    finally = {
      closeAllConnections()
    })
    return(resp)
  }
  
  ## Tests
  # entity="ihop"
  # parameters <- paste0("title=",entity,"&",start,"&",end,"&",language,"&",country,"&cluster=true&cluster.algorithm=lingo&per_page=5")
  # ihop_text_5 <- aylienApi(APPLICATION_ID, APPLICATION_KEY, endpoint, parameters,type)
  
  # entity="ihop"
  # parameters <- paste0("title=",entity,"&",start,"&",end,"&",language,"&",country,"&cluster=true&cluster.algorithm=lingo&per_page=100")
  # ihop_text_100 <- aylienApi(APPLICATION_ID, APPLICATION_KEY, endpoint, parameters,type)
  

  parameters <- paste0("title=",entity,"&",start,"&",end,"&",language,"&",category,"&",country,"&cluster=true&cluster.algorithm=lingo&per_page=100&categories.id=IAB8&categories.taxonomy=iab-qag")

  resp <- list()
  resp[[1]] <- aylienApi(APPLICATION_ID,APPLICATION_KEY,endpoint,parameters=paste0(parameters,"&cursor=*"),type)
  cat("Crawling","page 1 ")
  nextpage <- list()
  nextpage[[1]] <- ""
  
  nextpage[[2]] <- resp[[1]]$next_page_cursor
  i <- 2
  n <- 2
  while(nextpage[[i]]!=nextpage[[i-1]]) {
    cat(paste("page",n, ""))
    Sys.sleep(1)
    raw <- aylienApi(APPLICATION_ID,APPLICATION_KEY,endpoint,parameters=gsub("[+]","%2B",paste0(parameters,"&cursor=",nextpage[[i]])),type)
    resp[[i]] <- raw
    i <- i+1
    nextpage[[i]] <- raw$next_page_cursor
    n <- n+1
  }
  
  ## Writing raw data to a file on disk
  write(toJSON(resp,asIS=T,digits=8,factor ="string"), paste0("~/Desktop/Aylien/raw/",URLdecode(entity),"_",Sys.Date(),".txt"))
  return(resp)
}

# # Tests
# entity="ihop"
# category=NA
# ihop_page=aylienPaging(entity,category,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)

#############
# 2. Parser #
#############

aylienParser <- function(entity,resp){
  
  # Functions
  importanceAssigner <- function(stories,clusters){
    ## Take in stories and clusters from the same query, assign 'Primary' or 'Related' to each story base on Alexa score
    ## Some clusters might share the same 'Primary' story. Such story will only show up once
    primaryId=c()
    
    for(c in 1:length(clusters$id)){
      # print(c)
      clusteredStories=filter(stories,id %in% unlist(clusters$stories[c]))
      
      if(all(sapply(clusteredStories$source.rankings.alexa,dim)==0)){
        next
      }else{
        
        # usAlexaScoreRbind=try(lapply(clusteredStories$source.rankings.alexa,function(x)filter(x,country=="US")[1,]) %>% list.rbind(),silent = TRUE)
        # 
        # if(is(usAlexaScoreRbind, "try-error")){
        #   primaryId[c]=clusteredStories$id[[1]]
        # }else{
        #   usAlexaScore=lapply(clusteredStories$source.rankings.alexa,function(x)filter(x,country=="US")[1,]) %>% bind_rows() 
        #   primaryId[c]=clusteredStories$id[[which.max(usAlexaScore$rank)]]
        # }
        primaryId[c]=clusteredStories$id[[1]]
      }
    }
    
    stories$importance=sapply(stories$id,function(x)ifelse(x %in% primaryId,'Primary','Related'))
    return(stories)
    
  }
  
  labelPicker=function(newsLabels,entity){
    ## Input a list of summary.sentences.Select the summary.sentence that contains search keyword. 
    ## If not, return the first summary.sentence.
    selectedLabels=c()
    for(l in 1:length(newsLabels)){
      # print(paste0("cluster labels ",l))
      
      if(length(newsLabels[[l]])==0){
        selectedLabels[l]= NA
        next}
      
      if(any(sapply(newsLabels[l],function(x)str_detect(x, regex(entity, ignore_case = T))))==TRUE){
        selectedLabels[l]=newsLabels[l][[1]][[min(which(sapply(newsLabels[l],function(x)str_detect(x, regex(entity, ignore_case = T)))==TRUE))]]
      }else{
        selectedLabels[l]=newsLabels[l][[1]][1]
      }
      
      # print(length(selectedLabels))
    }
    
    return(selectedLabels)
  }
  
  news=data.frame(
    externalId=NULL,
    eventType=NULL,
    label=NULL,
    description=NULL,
    startDate=NULL,
    endDate=NULL,
    brand=NULL,
    source=NULL,
    score=NULL
  )
  
  for(i in 1:(length(resp)-1)){
    # i indicate page number
    if(i==1){
      print(paste("Parsing page",i))
    }else{
      print(paste("page",i))
    }
    
    stories <- list.cbind(resp[[i]]$stories) %>% data.table()
    clusters <- list.cbind(resp[[i]]$clusters) %>% data.table()
    if(length(clusters)==0){next}
    allNews=importanceAssigner(stories,clusters) %>% filter(importance=='Primary')
    primaryNews=allNews%>% 
      transmute(externalId=id,
                eventType='News',
                label=unlist(labelPicker(title,entity)),
                description='',
                startDate=as.Date(unlist(published_at),"%Y-%m-%d"),
                endDate=as.Date(unlist(published_at),"%Y-%m-%d"),
                brand=entity,
                source=source.name,
                score='')
    news=rbind(news,primaryNews)
  }
  
  return(news)
}

# # Tests
# ihop_parsed=aylienParser(entity,resp=ihop_page)


##############
# 3. Cralwer #
##############

aylienCrawler=function(entity,category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type){
  resp=aylienPaging(entity,category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
  news=aylienParser(entity,resp)
  return(news)
}

#########################
# ----- EXECUTION ----- #
#########################

## Parameters
APPLICATION_KEY <- "dc9a306e5f401f12793d967c306f6c47"
APPLICATION_ID <- "20c6ec55"
endpoint <- "stories"
type <- "url"

## URL parameters
start <- paste0("published_at.start=NOW-",as.integer(Sys.Date()-as.Date("2018-01-30")),"DAYS")
end <- "published_at.end=NOW"
language <- "language=en"
country <- "source.locations.country%5B%5D=US"

#parameters=paste0(start,end,language,country,collapse = "&")


## 1/30/2018
# ihop=aylienCrawler("IHOP",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# dennys=aylienCrawler("Denny's",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# cracker_barrel=aylienCrawler("%22Cracker+Barrel%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type) %>% 
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="Cracker Barrel",source,score)
# applebees=aylienCrawler("Applebee's",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# olive_garden=aylienCrawler("%22Olive+Garden%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type) %>% 
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="Olive Garden",source,score)
# chilis=aylienCrawler("Chili's",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)


# # 6/12/2018
# arbys=aylienCrawler("%22Arby's%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)%>%
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="Arby's",source,score)
# mcdonalds=aylienCrawler("%22McDonald%27s%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)%>%
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="McDonald's",source,score)
# burgerking=aylienCrawler("%22Burger+King%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type) %>%
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="Burger King",source,score)
# tacobell=aylienCrawler("%22Taco+Bell%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)%>%
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="Taco Bell",source,score)
# subway=aylienCrawler("Subway",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# wendys=aylienCrawler("Wendy's",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# chickfila=aylienCrawler("Chick-fil-A",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)


## 1/30/2018
# ihop=aylienCrawler("IHOP",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# dennys=aylienCrawler("Denny's",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# cracker_barrel=aylienCrawler("%22Cracker+Barrel%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type) %>% 
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="Cracker Barrel",source,score)
# applebees=aylienCrawler("Applebee's",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)
# olive_garden=aylienCrawler("%22Olive+Garden%22",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type) %>% 
#   transmute(externalId,eventType,label,description,startDate,endDate,brand="Olive Garden",source,score)
# chilis=aylienCrawler("Chili's",category=NA,APPLICATION_ID,APPLICATION_KEY,endpoint,parameters,type)

rslt=rbind(arbys,mcdonalds,burgerking,tacobell,subway,wendys,chickfila)
rslt=rslt %>% transmute(
  externalId=unlist(externalId),
  eventType=unlist(eventType),
  label=str_replace_all(label, "[\r\n|\t\"]" , ""),
  description=unlist(description),
  startDate=as.Date(startDate,format="%Y-%m-%d"),
  endDate=as.Date(endDate,format="%Y-%m-%d"),
  brand=unlist(brand),
  source=unlist(source),
  score=unlist(score)
) %>% data.frame()

write.table(rslt,"~/Desktop/arbys_news.txt",sep="|",row.names=FALSE,quote=FALSE,fileEncoding = "UTF-8")

