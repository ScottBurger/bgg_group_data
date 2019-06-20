

library(RCurl)
library(XML)
library(xml2)
library(rvest)
require(plyr)



form <- read.table("clipboard", sep="\t")
names(form) <- "BoardGameGeek Username"

fulldata <- data.frame(matrix(nrow = 1, ncol=16, data = 0))
user_temp_data <- data.frame(matrix(nrow = 1, ncol=16, data = 0))

names(fulldata) <- c(
  "user", 
  "gamenames", 
  "yearpublished", 
  "minplayers", 
  "maxplayers", 
  "playing_time_minutes", 
  "user_rating", 
  "bgg_rating", 
  "num_plays", 
  "own", 
  "wanttoplay",
  "wanttobuy", 
  "wanttotrade", 
  "wantintrade", 
  "wishlist", 
  "comment")

names(user_temp_data) <- c(
  "user", 
  "gamenames", 
  "yearpublished", 
  "minplayers", 
  "maxplayers", 
  "playing_time_minutes", 
  "user_rating", 
  "bgg_rating", 
  "num_plays", 
  "own", 
  "wanttoplay",
  "wanttobuy", 
  "wanttotrade", 
  "wantintrade", 
  "wishlist", 
  "comment")





for(i in form$`BoardGameGeek Username`){
  print(i)
  url <- sprintf("https://boardgamegeek.com/xmlapi2/collection?username=%s&stats=1&excludesubtype=boardgameexpansion", i)
  
  #forcing api pings
  pg <- 0
  xD <- 0
  xL <- 0
  
  pg <- read_xml(url)
  xD <- xmlParse(pg)
  xL <- xmlToList(xD)
  
  
  while(
    #length(xml_nodes(pg, xpath="//name") %>% xml_text()) < 1
    length(xL) <= 1
  ){
    Sys.sleep(30)
    pg <- read_xml(url)
    xD <- xmlParse(pg)
    xL <- xmlToList(xD)
    print(i)
  }
  

  
  #
  # new
  #
  for(j in 1:(length(xL)-1)){

   
    
   test <- data.frame(
     i
     ,xL[[j]]$name$text
     ,if(is.null(xL[[j]]$yearpublished)){"na"} else{xL[[j]]$yearpublished}
     ,xL[[j]]$stats$.attrs[1]
     ,xL[[j]]$stats$.attrs[2]
     ,xL[[j]]$stats$.attrs[5]
     ,xL[[j]]$stats$rating$.attrs
     ,xL[[j]]$stats$rating$bayesaverage
     ,xL[[j]]$numplays
     ,xL[[j]]$status[1] #own game flag
     ,xL[[j]]$status[5] #want to play flag
     ,xL[[j]]$status[7] #wishlisted flag
     ,xL[[j]]$status[6] #want to buy
     ,xL[[j]]$status[3] #available for trade
     ,xL[[j]]$status[4] #want in trade
     ,if(is.null(xL[[j]]$comment)){"na"} else{xL[[j]]$comment}
   )
   
   names(test) <- c("user", "gamenames", "yearpublished", "minplayers", "maxplayers", "playing_time_minutes", "user_rating", "bgg_rating", "num_plays", "own", "wanttoplay","wanttobuy", "wanttotrade", "wantintrade", "wishlist", "comment")
   user_temp_data <- rbind(user_temp_data, test)
    
  }

  fulldata <- rbind(fulldata, user_temp_data)
  
  
  

  
}

#cleanup
fulldata = fulldata[!duplicated(fulldata), ] #manual dedupe until i can figure out whats wrong with the loops

fulldata$yearpublished <- as.numeric(fulldata$yearpublished)
fulldata$minplayers <- as.numeric(fulldata$minplayers)
fulldata$maxplayers <- as.numeric(fulldata$maxplayers)
fulldata$playing_time_minutes <- as.numeric(fulldata$playing_time_minutes)
fulldata$user_rating <- as.numeric(fulldata$user_rating)
fulldata$bgg_rating <- as.numeric(fulldata$bgg_rating)
fulldata$num_plays <- as.numeric(fulldata$num_plays)
fulldata$comment <- gsub("[\r\n]", " ", fulldata$comment)

#remove first row of zero data
fulldata <- fulldata[-1,]


