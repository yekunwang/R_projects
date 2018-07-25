library(shiny)
library(dplyr)
library(plyr)
library(RGA)
library(rmongodb)
library(stringr)
library(lubridate)
library(googlesheets)
library(rjson)
library(ggplot2)
library(magrittr)
library(data.table)
options(warn=0)
prevM <-function(x){7 * floor(as.numeric(x-1+4) / 7) + as.Date(1-4,origin = "1970-01-01")}

server <- function(input, output) {
  
  
  #  #### Get Mongo User Metadata
  # signumUserMongoDb = mongoDbConnect(dbName = "signum_users",host = "mongo-03.dev.quantifind.com")
  # 
  # users = dbGetQuery(signumUserMongoDb,collection = 'users',"{}")
  # users = transmute(users,customer_id = X_customer,
  #                   id = X_id,
  #                   email = email,
  #                   name = sapply(users$name,function(x)paste(fromJSON(x)$first,fromJSON(x)$last)),
  #                   created_date = createdAt,
  #                   activation_date = activationStartAt,
  #                   updated_date = updatedAt)
  # 
  # customers = dbGetQuery(signumUserMongoDb,collection = 'customers',"{}") %>%
  #   transmute(customer_id = X_id,customer_name = name)
  # mongoUserMetaData = left_join(users,customers,by = c("customer_id" = "customer_id"))
  
  mongo=mongo(collection = "users", db = "signum_users", url = "mongodb://prod-product-mongo0.mtv.quantifind.com/?ssl=true",verbose = FALSE, options = ssl_options(weak_cert_validation=TRUE))
  mongo.is.connected(mongo)
  
  withProgress(message = 'Collecting: ', value = 0, {
    
    incProgress(detail = "Mongo data...")
    
    ## create the empty data frame
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
    
    
    
    #### Get Google Analytics Usage Data
    # get access token
    
    #authorize(cache = "~/Desktop/.ga-token.rds")
    #options(rga.cache = "/srv/shiny-server/apps/signumUsage/.ga-token.rds")
    authorize()
    # get a GA profiles
    gaProfiles = list_profiles()
    
    # choose the profile ID by site URL
    id = as.numeric(gaProfiles[grep("http://signum.quantifind.com",gaProfiles$websiteUrl), "id"])
    
    # get date when GA tracking began
    firstDate = firstdate(as.numeric(id))
    
    # User data
    gaUserData = get_ga(id, start.date = firstDate, end.date= "today",
                        dimensions = c("ga:dimension1","ga:date"
                                       #,"ga:region","ga:city","ga:browser","ga:operatingSystem","ga:browserVersion"
                        ),
                        metrics = c("ga:users"))
    
    gaUserReport = left_join(gaUserData,mongoUserMetaData,by=c("dimension1"="user_id")) %>%
      filter(customer_name != "Disabled") %>%
      filter(!(customer_name != "Quantifind" & str_detect(email,"@quantifind.com"))) %>%
      transmute(customer_name=customer_name,
                user_id=dimension1,
                user_name=name,
                date=as.Date(date,"%Y-%m-%d"),
                week=as.Date(prevM(date),"%Y-%m-%d"),
                #region,city,browser,operating_system=operatingSystem,browser_version=browserVersion,
                count=users) %>% data.table()
    
    incProgress(detail = "GA User data...")
    
    # Session data
    gaSessionData = get_ga(id, start.date = firstDate, end.date= "today",
                           dimensions = c("ga:dimension1","ga:date"),
                           metrics = c("ga:uniquePageviews","ga:sessionDuration","ga:pageviewsPerSession","ga:sessions"))
    
    gaSessionReport = left_join(gaSessionData,mongoUserMetaData,by=c("dimension1"="user_id")) %>%
      filter(customer_name != "Disabled") %>%
      filter(!(customer_name != "Quantifind" & str_detect(email,"@quantifind.com"))) %>%
      transmute(customer_name=customer_name,
                user_id=dimension1,
                user_name=name,
                date=date,
                week=prevM(as.Date(date,"%Y-%m-%d")),
                sessions=sessions,
                page_views=uniquePageviews,
                total_time_minutes=sapply(sessionDuration/60,function(x)round(x,digits=5))) %>% data.table()
    
    incProgress(detail = "GA Session data...")
    
    # Page data
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
                week=prevM(as.Date(strptime(dateHourMinute,"%Y%m%d"))),
                product=str_replace_all(pageTitle,"Signum \\| ",""),
                path=pagePath,
                time_on_page_minutes=timeOnPage/60)
    
    incProgress(detail = "GA Page data...")
    
  })
  
  #### UI Params
  output$customer=renderUI({
    selectizeInput(inputId="customer",label="Customer",choices = unique(gaSessionReport$customer_name),multiple = TRUE,selected="Quantifind")
  })
  
  output$user=renderUI({
    selectizeInput(inputId="user",label="User",choices = unique(gaSessionReport[customer_name %in% input$customer,user_name]),multiple = TRUE,selected=NULL)
  })
  
  
  
  #### User Data
  userData <- reactive({
    if(is.null(input$user)){
      gaUserReport %>% filter(date >= as.Date(input$targetDates[1]) & date <= as.Date(input$targetDates[2]) & customer_name %in% input$customer)
    }else{
      gaUserReport %>% filter(date >= as.Date(input$targetDates[1]) & date <= as.Date(input$targetDates[2]) & customer_name %in% input$customer & user_name %in% input$user)
    }
  })
  
  
  output$userPlot = renderPlot({
    
    if(is.null(input$user)){
      if(input$timeGranularity=="day"){
        plot_dat=userData() %>% data.table() %>% .[,.(daily_users=length(unique(user_id))),by=c("date","customer_name")]
        
        ggplot(plot_dat,aes(x = date,y = daily_users, fill = customer_name)) +
          geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
          labs(x="Date",y="Count") +
          ggtitle("Daily Number of Distinct Users")
      }else{
        plot_dat=userData() %>% data.table() %>% .[,.(weekly_users=length(unique(user_id))),by=c("week","customer_name")]
        
        ggplot(plot_dat,aes(x = week,y = weekly_users, fill = customer_name)) +
          geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
          labs(x="Week",y="Count") +
          ggtitle("Weekly Number of Distinct Users")
      }
      
    }else{
      if(input$timeGranularity=="day"){
        plot_dat=userData() %>% data.table() %>% .[,.(daily_users=length(unique(user_id))),by=c("date","user_name")]
        
        ggplot(plot_dat,aes(x = date,y = daily_users, fill = user_name)) +
          geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
          labs(x="Date",y="Count") +
          ggtitle("Daily Number of Distinct Users")
      }else{
        plot_dat=userData() %>% data.table() %>% .[,.(weekly_users=length(unique(user_id))),by=c("week","user_name")]
        
        ggplot(plot_dat,aes(x = week,y = weekly_users, fill = user_name)) +
          geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
          labs(x="week",y="Count") +
          ggtitle("Weekly Number of Distinct Users")
      }
    }
  })
  
  
  #### Session Data 
  sessionData <- reactive({
    if(is.null(input$user)){
      gaSessionReport %>% filter(as.Date(date) >= as.Date(input$targetDates[1]) & as.Date(date) <= as.Date(input$targetDates[2]) &
                                   customer_name %in% input$customer)
    }else{
      gaSessionReport %>% filter(as.Date(date) >= as.Date(input$targetDates[1]) & as.Date(date) <= as.Date(input$targetDates[2]) &
                                   customer_name %in% input$customer & user_name %in% input$user)
    }
  })
  
  output$sessionPlot = renderPlot({
    
    if(is.null(input$user)){
      if(input$timeGranularity=="day"){
        plot_dat=sessionData() %>% data.table() %>% .[,.(total_time_minutes=sum(total_time_minutes)),by=c("date","customer_name")]
        ggplot(plot_dat,aes(x = as.Date(date),y = total_time_minutes, fill = customer_name)) +
          geom_bar(stat="identity",position=position_stack(reverse = TRUE)) +
          labs(x="Date",y="Minutes") +
          ggtitle("Total Time in Product")
        
      }else{
        plot_dat=sessionData() %>% data.table() %>% .[,.(total_time_minutes=sum(total_time_minutes)),by=c("week","customer_name")]
        ggplot(plot_dat,aes(x = as.Date(week),y = total_time_minutes, fill = customer_name)) +
          geom_bar(stat="identity",position=position_stack(reverse = TRUE)) +
          labs(x="Week",y="Minutes") +
          ggtitle("Total Time in Product")
      }
      
    }else{
      if(input$timeGranularity=="day"){
        plot_dat=sessionData() %>% data.table() %>% .[,.(total_time_minutes=sum(total_time_minutes)),by=c("date","user_name")]
        ggplot(plot_dat,aes(x = as.Date(date),y = total_time_minutes, fill = user_name)) +
          geom_bar(stat="identity",position=position_stack(reverse = TRUE)) +
          labs(x="Date",y="Minutes") +
          ggtitle("Total Time in Product")
      }else{
        plot_dat=sessionData() %>% data.table() %>% .[,.(total_time_minutes=sum(total_time_minutes)),by=c("week","user_name")]
        ggplot(plot_dat,aes(x = as.Date(week),y = total_time_minutes, fill = user_name)) +
          geom_bar(stat="identity",position=position_stack(reverse = TRUE)) +
          labs(x="Week",y="Minutes") +
          ggtitle("Total Time in Product")
      }
    }
    
  })
  
  output$sessionTable = renderDataTable({
    if(input$timeGranularity=="day"){
      table_dat=sessionData() %>% data.table() %>%
        .[,.(customer_name,user_name,date,sessions,page_views,total_time_minutes)]%>% 
        dplyr::arrange(desc(as.Date(date)))
    }else{
      table_dat=sessionData() %>% data.table() %>% 
        .[,.(sessions=sum(sessions),page_views=sum(page_views),total_time_minutes=sum(total_time_minutes)),
          by=c("customer_name","user_name","week")] %>%
        dplyr::arrange(desc(as.Date(week)))
    }
  })
  
  
  #### Page Data
  pageData <- reactive({
    if(is.null(input$user)){
      gaPageReport %>% filter(date_time >= as_datetime(input$targetDates[1]) & 
                                date_time <= as_datetime(paste(as.Date(input$targetDates[2]),substr(Sys.time(),12,19)),tz="America/Los_Angeles") & 
                                customer_name %in% input$customer)
      # gaPageReport %>% filter(customer_name %in% input$customer)
    }else{
      gaPageReport %>% filter(date_time >= as_datetime(input$targetDates[1]) & 
                                date_time <= as_datetime(paste(as.Date(input$targetDates[2]),substr(Sys.time(),12,19)),tz="America/Los_Angeles") & 
                                customer_name %in% input$customer & user_name %in% input$user)
      #gaPageReport %>% filter(customer_name %in% input$customer & user_name %in% input$user)
    }
  })
  
  output$pageTable = renderDataTable({
    table_dat=pageData() %>% data.table() %>%
      .[,.(customer_name,user_name,date_time,product,path,time_on_page_minutes)] %>%
      dplyr::arrange(desc(as.Date(date_time)))
  })
  
  
}


ui <- fluidPage(
  headerPanel('Signum Usage'),
  
  sidebarPanel(
    dateRangeInput(inputId="targetDates",label="Select Date Range:",start = as.Date("2017-09-11"),end = format(Sys.time(), tz="America/Los_Angeles",usetz=TRUE)),
    radioButtons("timeGranularity","Plot Time Granularity:",choices = c("Week" = "week", "Day" = "day"),selected = "week", inline = TRUE),
    uiOutput("customer"),
    uiOutput("user")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("User Sessions", plotOutput(outputId = "userPlot"),plotOutput(outputId = "sessionPlot"),
               dataTableOutput(outputId = "sessionTable")), 
      tabPanel("Page Views",dataTableOutput(outputId = "pageTable"))
      #,tabPanel("User Metadata", dataTableOutput(outputId = "userTable"))
    )
  )
)

shinyApp(ui = ui, server = server)
