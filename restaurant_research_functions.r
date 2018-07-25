## Restaurant Reviews Parsing
## Author: Yekun
## Date: 8/22/16
## Description: functions used for cleaning/pasring Restaurant Reviews LTO data.

#########
# Setup #
#########

require(magrittr)
require(dplyr)
require(data.table)
require(stringr)
require(ggplot2)
require(WriteXLS)
require(XLConnect)
require(lubridate)

#####################
# File reformatting #
#####################

# Monthly_Promotions_Database_MasterSheet.xls is the master file that aggregates all monthly LTO offers.
# Columns/rows in this file are months/brands. Every month, they send us an updated "Monthly Promotions Database - [UPDATE TIME].xls" with
# an additional column for last month's data. Follow these instructions for updating Monthly_Promotions_Database_MasterSheet.xls:
#    1. Open the newest file sent by RR, copy the last column to "Monthly_Promotions_Database_MasterSheet.xls".
#    2. Make sure column headers follow chronological order and the number of rows match exactly with number of brands in 
#       previous updates (i.e. no new restaurant has been added).
#    3. Save your edit of "Monthly_Promotions_Database_MasterSheet.xls".
#    4. Run fileLoader function with file path, total # of rows and total # of columns.
#       EX: newFile <- fileLoader(file="~/Desktop/restaurant_reviews/Monthly_Promotions_Database_MasterSheet.xls",endRow = 67,endCol=121)

fileLoader <- function(file,endRow,endCol){
  # Read in raw data. 
  # Note that startRow = 4 to remove extraneous header/logs.
  raw <- as.data.frame(readWorksheet(loadWorkbook(file),sheet=1,startRow=4,startCol=1,endRow=endRow,endCol=endCol), 
                       stringsAsFactors = FALSE)
  
  # Renaming columns and rows, removing meaningless rows
  cnames <- raw[1,]
  new_product_cols <- which(str_detect(cnames,"New Products"))
  raw <- raw[,-c(new_product_cols)]
  
  rnames <- raw[,1]
  rownames(raw) <- c("",rnames[-1])
  agg_category <- which(rownames(raw) %in% c("Sandwich","Sub-Sandwich","Pizza","Chicken","Coffee/Bakery","Casual","Family","Fast Casual"))
  agg_astrid <- which(str_detect(rownames(raw),fixed("*")))
  raw <- raw[-c(agg_category,agg_astrid),]
  
  cnames <- sapply(raw[1,3:ncol(raw)],function(x) format(as.Date(as.character(x)), format="%b-%y"))
  
  raw <- raw[-c(1),-c(1,2)]
  colnames(raw) <- cnames
  
  # Check number of months: Dec-16 55 brands
  print(paste0("number of months: ", ncol(raw)))
  # Lastest month
  print(paste0("lastest month: ", colnames(raw)[ncol(raw)]))
  
  # Check number of brands: Dec-16 109 months
  print(paste0("number of brands: ", nrow(raw)))
  # Last brand
  print(paste0("last brand: ", rownames(raw)[nrow(raw)]))
  
  return(raw)
  
}

################
# Item Parsing #
################

# cellItemizer is a function that scans an individual cell in the data frame and transcribe cell content to 
# "description" (name of this promotion) and "label" (details about this promotion).

cellItemizer <- function(string){
  if(is.na(string)){
    # If cell is empty, replace wtih empty string
    tbl <- data.frame(row.names=NULL,description="",label="")
  }else{
    if(!str_detect(string,"\\(")){
      # No parenthesis, parse by comma
      items <- unlist(strsplit(string,",")) %>% str_trim(.,side="both")
      tbl <- data.frame(row.names=NULL,
                        description=items,
                        label=rep("",n=length(items)),stringsAsFactors = FALSE)
    }else{
      # With parenthesis
      if(sum(str_count("\\("))!=sum(str_count("\\)"))){
        stop("Mismatched parenthesis!")
      }else{
        # parse out () information as label
        labels <- gsub("[\\(\\)]", "", regmatches(string, gregexpr("\\(.*?\\)", string))[[1]],perl = TRUE) %>% str_trim(.,side="both")
        # replace () with place holders
        string <- gsub("\\(.*?\\)","____",string,perl = FALSE)  %>% str_trim(.,side="both")
        #string <- str_replace_all(x,"\n"," ")
        # collect main items
        items <- unlist(str_split(string,",")) %>% str_trim(.,side="both")
        # split into with and without label
        nofill <- items[!str_detect(items,"____")]
        withfill <- items[str_detect(items,"____")] %>% sapply(.,function(x)gsub("____","",x))
        
        tbl <- data.frame(row.names=NULL,
                          description=withfill,
                          label=labels,stringsAsFactors = FALSE) %>% 
          rbind(data.frame(description=nofill,label=rep("",length(nofill))))
        
      }
    }
  }
  return(tbl)
}

###############################
# Identify Promotion Duration #
###############################

# rowParse is a function that takes an entire row (all information on a single brand), "itemize" each cell, and knits together
# a timeline for individual promotion. It produces a table with promotion name, promotion details, start and end date of that
# promotion. Duration of the promotion is defined as follows: 
#     1. If the promotion had lasted for only one month but was discontinued in the next month, then 
#        startDate is the same asd Endate.
#     2. If the promotion first appeared in month 1 and lasted through month 3, startDate is month 1 and endDate is month 3.
#     3. If the promotion first appeared in month 1, discontinued in month 2, reappeared in month 3, it's considered
#        as two separate events and recorded in two separate rows

rowParse <- function(row){
  # If the row is not empty, parse first cell
  if(!all(is.na(row))){
    # Parse the first month as the initial point
    lto <- cellItemizer(row[,1])
    # Form a table for all items in the first month
    all_lto <- lto %>% cbind(.,data.frame(startDate=rep(as.character(colnames(row)[1]),nrow(lto)),
                                          endDate=rep(as.character(colnames(row)[1]),nrow(lto)),
                                          stringsAsFactors=FALSE))
    # De-dupe rows
    all_lto <- all_lto[!duplicated(all_lto),]
    # Set current month as the starting point for next iteration
    lastMonth=colnames(row)[1]
    
    # Iterating through the remaining columns
    for(i in 2:length(row)){
      # Skip the next cell if empty
      if(is.na(row[i])) {next}
      # Iterate through each month: i = month
      thisMonth <- colnames(row)[i]
      lto <- cellItemizer(row[,i])
      # De-dupe rows base on the first two columns
      lto <- lto[!duplicated(lto[,1:2]),]
      #print(paste0("month:", thisMonth,"; column:",i))
      
      for(j in 1:nrow(lto)){
        # Iterate through each offer: j = offer
        offer <- lto[j,]
        # To be considered as a continuation promotion, both "description" and "label" much match
        if(sum(offer$description == all_lto$description & offer$label == all_lto$label) > 0){
          
          # A continuous promotion
          if(all_lto[max(which((all_lto$description==offer$description) & (all_lto$label==offer$label))),"endDate"] == lastMonth){
            # Continuation from last month
            all_lto[max(which((all_lto$description==offer$description) & (all_lto$label==offer$label))),"endDate"] = thisMonth
            all_lto <- all_lto[!duplicated(all_lto),]
            
            # Skipped some periods, a returning LTO but not a continuation
          }else{
            all_lto <- rbind(all_lto,data.frame(description=offer$description,
                                                label=offer$label,
                                                startDate=as.character(thisMonth),
                                                endDate=as.character(thisMonth),stringsAsFactors=FALSE))
            all_lto <- all_lto[!duplicated(all_lto),]
          }
        }else{
          # New offer
          all_lto <- rbind(all_lto,data.frame(description=offer$description,
                                              label=offer$label,
                                              startDate=as.character(thisMonth),
                                              endDate=as.character(thisMonth),stringsAsFactors=FALSE))
          all_lto <- all_lto[!duplicated(all_lto),]
        }
      }
      lastMonth=thisMonth
      #print(all_lto)
    }
    row.names(all_lto) <- seq(1,nrow(all_lto))
    return(all_lto)
  }
}

##############################
# Unit Test Individual Table #
##############################

# brandTester loops through every row and test if script errors at any particular row. If it happens,
# you need to correct the errors in "Monthly_Promotions_Database_MasterSheet.xls", re-run fileLoader
# and run brandTester. brandAggregagtor will only work when all brands pass brandTester.

brandTester <- function(raw){
  brands <- row.names(raw)
  for(i in 1:nrow(raw)){
    print(paste("Brand:",brands[i]))
    tryCatch({
      testBrand <- rowParse(raw[i,]) %>% arrange(description)},
      error=function(e){print(paste("ERROR:",brands[i],"format is incorrect"))})
  }
}

####################
# Brand Aggregator #
####################

# brandAggregator performance the same set of exercises as brandTester, but aggregate all brands to a 
# big table and format it properly to Timeline data standard. 

brandAggregator <- function(raw,fileName){  
  # Get all brands 
  brands <- rownames(raw)
  # Create empty data frame
  agg_df <- data.frame()
  
  # For-loop through all brands
  for(i in 1:nrow(raw)){
    brand <- brands[i]
    print(brand)
    tbl <- cbind(brand,rowParse(raw[i,]))
    agg_df <- rbind(agg_df,tbl)
  }
  
  # Add in additional columns 
  agg_df$externalId <- ""
  agg_df$eventType <- "LTO"
  agg_df$source <- "Restaurant Reviews"
  agg_df$score <- ""
  agg_df$description <- str_replace_all(agg_df$description,"\n"," ") 
  agg_df$label <- str_replace_all(agg_df$label,"\n"," ") 
  agg_df <- agg_df[agg_df$description!="N/a",]
  agg_df <- agg_df[!(agg_df$description=="" & agg_df$label==""),]
  
  # Reformat start and end dates
  agg_df$startDate <- format(as.Date(str_replace_all(agg_df$startDate,"-"," 01 "),"%b %d %y"),"%m/%d/%Y")
  agg_df$endDate <- format(ymd(as.Date(str_replace_all(agg_df$endDate,"-"," 01 "),"%b %d %y"))+months(1)-days(1),"%m/%d/%Y")
  
  # Re-order columns
  agg_df %<>% select(externalId,eventType,description,brand,label,startDate,endDate,source,score)

  # Output excel file
  WriteXLS(agg_df,ExcelFileName=fileName,Encoding="UTF-8")
}
