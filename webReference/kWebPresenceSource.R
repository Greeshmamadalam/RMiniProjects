##################################################################
##################################################################
##################################################################

get.createresultfile <- function(tbl.searchGoogleItems, uuid = 504){
  require(dplyr)
  require(shiny) ## for tags$a
  
  # cat('------------------------------------------')
  # cat('-----inside get.uniqueUrlsTable-----------')
  # 
  # # make a vector of just the url
  # resultsUrls <- sapply(ls.results, function(x) {gsub('\t.*','',x )})
  # ## make a vector of just the matchScore
  # matchScore <- sapply(ls.results, function(x) {gsub('.*\t','',x )})
  # 
  # # put them in a table
  # tbl.webPresenceResults <- data_frame(matchScore = matchScore, resultsUrls = resultsUrls)
  # 
  # ## make resultsUrls into hyperlinks  
  # tbl.uniqueURLs <- data_frame(uniqueURL=unique(tbl.searchGoogleItems$link))
  # 
  # ## make resultsUrls into hyperlinks  
  # tbl.uniqueURLs <- tbl.uniqueURLs %>% rowwise() %>% 
  #   mutate(uniqueURL = paste0(tags$a(uniqueURL, href=uniqueURL, target="_blank")))
  # 
  # 
  # ## add uuid back into the table - This is just for an example
  # tbl.uniqueURLs$uuid <- uuid
  # 
  uniqueURL=unique(tbl.searchGoogleItems$link)
  
  # return(tbl.uniqueURLs)
  file.remove("input.txt")
  file.remove("output.txt")
  urls<- lapply(uniqueURL,function(x){
    write( x, file = "input.txt",append = TRUE)
  })
  cat('------------------------------------------')
  cat('--------Running python scripts------------')
  system("python3.4 webscraper.py")
  
  cat('------------------------------------------')
  cat('----changing the format to tables---------')
  sorted_results <- scan("output.txt", what="", sep="\n")
  resultsUrls <- sapply(sorted_results, function(x) {gsub('\t.*','',x )})
  ## make a vector of just the matchScore
  matchScore <- sapply(sorted_results, function(x) {gsub('.*\t','',x )})

  # put them in a table
  tbl.webPresenceResults <- data_frame(matchScore = matchScore, resultsUrls = resultsUrls)
  tbl.webPresenceResults <- tbl.webPresenceResults %>% rowwise() %>%
    mutate(resultsUrls = paste0(tags$a(resultsUrls, href=resultsUrls, target="_blank")))
  
  
  return(tbl.webPresenceResults)
}
get.webPresenceOutputTable <- function(ls.results, uuid = 504){
  ## this function breaks out the ls.results into matchedUrls, and matchedScore
  ## makes it a dplyr table and uses mutate with paste0(tags$a...) to create an Html hyperlink in Shiny
  ## adds back a dummy uuid - this is just an example
  require(dplyr)
  require(shiny) ## for tags$a
  
  cat('------------------------------------------')
  cat('-----inside get.webPresenceOutputTable----')
  
  ## make a vector of just the url
   resultsUrls <- sapply(ls.results, function(x) {gsub('\t.*','',x )})
  # ## make a vector of just the matchScore
   matchScore <- sapply(ls.results, function(x) {gsub('.*\t','',x )})
   
  ## put them in a table
  tbl.webPresenceResults <- data_frame(matchScore = matchScore, resultsUrls = resultsUrls)
  
  ## make resultsUrls into hyperlinks  
  tbl.webPresenceResults <- tbl.webPresenceResults %>% rowwise() %>% 
    mutate(resultsUrls = paste0(tags$a(resultsUrls, href=resultsUrls, target="_blank")))
  
  ## add uuid back into the table - This is just for an example
  tbl.webPresenceResults$uuid <- uuid
  
  return(tbl.webPresenceResults)
}



get.uniqueUrlsTable <- function(tbl.searchGoogleItems, uuid = 504){
  ## this function breaks out the ls.results into matchedUrls, and matchedScore
  ## makes it a dplyr table and uses mutate with paste0(tags$a...) to create an Html hyperlink in Shiny
  ## adds back a dummy uuid - this is just an example
  require(dplyr)
  require(shiny) ## for tags$a
  
  # cat('------------------------------------------')
  # cat('-----inside get.uniqueUrlsTable-----------')
  # 
  # # make a vector of just the url
  # resultsUrls <- sapply(ls.results, function(x) {gsub('\t.*','',x )})
  # ## make a vector of just the matchScore
  # matchScore <- sapply(ls.results, function(x) {gsub('.*\t','',x )})
  # 
  # # put them in a table
  # tbl.webPresenceResults <- data_frame(matchScore = matchScore, resultsUrls = resultsUrls)

  ## make resultsUrls into hyperlinks
  tbl.uniqueURLs <- data_frame(uniqueURL=unique(tbl.searchGoogleItems$link))

  ## make resultsUrls into hyperlinks
  tbl.uniqueURLs <- tbl.uniqueURLs %>% rowwise() %>%
    mutate(uniqueURL = paste0(tags$a(uniqueURL, href=uniqueURL, target="_blank")))


  ## add uuid back into the table - This is just for an example
  tbl.uniqueURLs$uuid <- uuid

  return(tbl.uniqueURLs)
}

###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
get.searchGoogle <- function(searchQuery){
  require(httr)
  require(jsonlite)
  require(dplyr)
  # # kountable greeshma@kountable.com
   # query=paste0("https://www.googleapis.com/customsearch/v1?key=AIzaSyDyxoDWevK91lz49Cxbg_APvBsGlG55e98&cx=015498628032087398999:ud_5ivbrhck&q=",z,"&start=1")
  # 
  # personal search madalamgreeshma@gmail.com
  # # query=paste0("https://www.googleapis.com/customsearch/v1?key=AIzaSyA2yR9znDYdPk33iGN-7NZsiQ0sKpZEMcU&cx=015636274321488554372:ndgfg0bti1c&q=",z,"&start=1")
  # getRestApi <- GET(paste0("https://www.googleapis.com/customsearch/v1?q=",searchQuery,"&key=AIzaSyDyxoDWevK91lz49Cxbg_APvBsGlG55e9&&cx=015498628032087398999:ud_5ivbrhck&start=1"))
  # getRestApi <- GET(paste0("https://www.googleapis.com/customsearch/v1?q=",searchQuery,"&key=AIzaSyA2yR9znDYdPk33iGN-7NZsiQ0sKpZEMcU&cx=015636274321488554372:ndgfg0bti1c&start=1"))
   getRestApi <- GET(paste0("https://www.googleapis.com/customsearch/v1?q=",searchQuery,"&key=AIzaSyBtTswU1I7Poh5v2yZATjp0EDNLgNe__as&cx=014864116753772675149:wbyoddtfw6g&start=1"))
  # 002409845641706544210:iqkiadrfhb8
  # joe@kountable.com
  # getRestApi <- GET(paste0("https://www.googleapis.com/customsearch/v1?q=",searchQuery,"&key=AIzaSyBytp2mvoFgmzfPagz6KoQbsragsclRUxc&cx=002409845641706544210:iqkiadrfhb8&start=1"))
  # premium
  # getRestApi <- GET(paste0("https://www.googleapis.com/customsearch/v1?q=",searchQuery,"&key=AIzaSyAwOQpWe0QgHULJ5nckhtiSOe9IQIPhwDE&cx=001414228353368749962:9m0orcsoxp8&start=1"))
  
  getRestApiContent <- fromJSON(rawToChar(getRestApi$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  
  
  
  ## if limit exceeded print the error message
  if(is.null(getRestApiContent$error)){
    if(!is.null(getRestApiContent$items$pagemap)){
      cat('+++++++++++++++++++hit++++++++++++++++',searchQuery,'\n')
      
      ## make main table and remove list of tables pagemap
      tbl.searchGoogleItems <- dplyr::select(getRestApiContent$items, -pagemap)
      
      ## work on pagemap
      ## each list item is has the same number of list elements as rows from the tbl.searchGoogleItems
      ## assign each of these into tbl.searchGoogle Items as a new field in the table
      tbl.searchGoogleItems <- bind_cols(tbl.searchGoogleItems,getRestApiContent$items$pagemap)
      return(tbl.searchGoogleItems)
    } else{
      cat('-------------------miss------------',searchQuery,'\n')
      
      return(NULL)
      
    }
    
  }else{
    ## print error message
    totoro <- c('              !         !           \n','             ! !       ! !          \n','            ! . !     ! . !          \n','              ^^^^^^^^^ ^            \n','            ^             ^          \n','          ^  (0)       (0)  ^       \n','         ^        ""         ^       \n','        ^   ***************    ^     \n','      ^   *                 *   ^    \n','    ^   *   /\   /\   /\    *    ^   \n','   ^   *                     *    ^\n','  ^   *   /\   /\   /\   /\   *    ^\n',' ^   *                         *    ^\n',' ^  *                           *   ^\n',' ^  *                           *   ^\n','  ^ *                           *  ^  \n','   ^ *                          * ^ \n','    ^ *                        * ^\n','     ^ *                      * ^\n','      ^  *       ) (         * ^\n','          ^^^^^^^^ ^^^^^^^^^ ')
    cat('\nTotoro says,\n',getRestApiContent$error$message,'\n', totoro)
    getRestApi
    return(NULL)
  }
  
}
###################################################################################################################################################
###################################################################################################################################################

## test code
# searchQuery <- all_words_combos[1]
# xx <- get.searchGoogle(z)

###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
get.googleSearchHits <- function(keywords, helpwords){
  
  require(dplyr)
  ## combine keywords and helpwords into one big list
  keywords <- unique(remove.unknowns(keywords))
  helpwords <- unique(remove.unknowns(helpwords))
  write(unlist(keywords), file = "keywords.txt",append = TRUE, sep = "\n")
  write(unlist(helpwords), file = "helpwords.txt",append = TRUE, sep = "\n") 
  all_words_list <- c(unlist(keywords),unlist(helpwords))
  
  ## do all combinations of n choose r combinations for all r
  all_words_combos <- unlist(lapply(0:(length(all_words_list)-1), function(i){
    sapply( combn(all_words_list,length(all_words_list)-i, simplify = FALSE), function(x){URLencode(gsub("\\s+",' ', gsub(',',' ',paste0(x, collapse = ' '))))})
  }))
  
  ## do the same for the help words subset
  help_words_combos <- unlist(lapply(0:(length(helpwords)-1), function(i){
    sapply( combn(helpwords,length(helpwords)-i, simplify = FALSE), function(x){URLencode(gsub("\\s+",' ', gsub(',',' ',paste0(x, collapse = ' '))))})
  }))
  

  ## use the famouse !( %in%) logic to remove duplicate help_wwords_combos 
  all_words_combos <- all_words_combos[!(all_words_combos %in% help_words_combos)]
  cat('===================================================================\n')
  cat('===length(all_words_combos)=',length(all_words_combos),' ==========\n')
  cat('===================================================================\n')
  
  
  ## now that we have a list of search queries, run them all
  ls.all_words_items <- lapply(all_words_combos, function(x){get.searchGoogle(x)})
  cat('-------Binding Hits---------')
  ## keep the hits and drop the misses using !is.null
  ## takes list of searchQueries and returns a beautiful table
  tbl.googleSearchHits <- tbl_df(bind_rows(ls.all_words_items[sapply(ls.all_words_items, function(x){!is.null(x)})]))
  
  return(tbl.googleSearchHits)
  
}

remove.unknowns <- function(words){
  return (words[!(words %in% list("Unknown",""))])
}

###################################################################################################################################################
###################################################################################################################################################

# keywords <- profileKeywords
# helpwords <- profileHelpwords
# 
# keywords <- c('donald','trump')
# helpwords <- c('NY','Trump Tower')

# uuid <- 504

###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
get.webPresencePagesOutputTable <- function(uuid = 504){
  require(dplyr)
  file.remove("keywords.txt")
  file.remove("helpwords.txt")
 # uuidApiKYC1 <- get.apiKYCCall(uuid)
  frankprofiledata <- get.V2UserProfile(uuid)
  userProjects <- get.V2ProjectsList(uuid)
  countries <- get.V3CountriesDictionary()
  business_types <- get.V2business_types()
  
  businessAll<- get.frankBusinessDataALL(userProjects, business_types, countries)
  if(!is.null(ncol(businessAll))){
    count <- ncol(businessAll)-1
  }else{
    count <- 0
  }
  
  name1 <- paste(frankprofiledata$firstName,frankprofiledata$lastName)
  name2 <- paste(frankprofiledata$lastName,frankprofiledata$firstName)

  profileKeywords <- list(name1,name2,frankprofiledata$primaryEmail)
  # 'firstname lastname','lastname firstname','email'  <=======keywords
  profileHelpwords <- list(frankprofiledata$primaryPhone,frankprofiledata$address,frankprofiledata$city,frankprofiledata$state,frankprofiledata$country)
  # frankprofiledata$data[4],frankprofiledata$data[6],frankprofiledata$data[7],frankprofiledata$data[9],frankprofiledata$data[10]
  ## get hits for profile keywords and helpwords
  tbl.googleSearchHits <- get.googleSearchHits(keywords = profileKeywords, helpwords = profileHelpwords)
  ## assign Persons
  tbl.googleSearchHits$webSearchType <- 'Persons'
  tbl.googleSearchHits$uuid <- strtoi(uuid)

  ## get akk user business
  tbl.userBusinessList <- get.userBusinessList(apiUUID = uuid)
  
  if(!is.null(tbl.userBusinessList)){
    ## Here's the way I put it together
    countries <- get.V3CountriesDictionary()$data
    ## make 0 row
    countries[250,"id"] <- 0
    countries[250,"code"] <- ''
    countries[250,"code2"] <- ''
    countries[250,"name"] <- ' '
    
    tbl.userBusinessList$country_id[tbl.userBusinessList$country_id ==0] <- 250
    ## add country name, country code3, country code2
    tbl.userBusinessList$countryName <- countries$name[tbl.userBusinessList$country_id]
    tbl.userBusinessList$code3 <- countries$code[tbl.userBusinessList$country_id]
    tbl.userBusinessList$code2 <- countries$code2[tbl.userBusinessList$country_id]
    tbl.userBusinessList$businessKeywords <- lapply(1:nrow(tbl.userBusinessList), function(i){c(tbl.userBusinessList$name[i],tbl.userBusinessList$contact[i],tbl.userBusinessList$phone[i])})
    tbl.userBusinessList$businessHelpWords <- lapply(1:nrow(tbl.userBusinessList), function(i){c(tbl.userBusinessList$description[i],tbl.userBusinessList$address[i],tbl.userBusinessList$city[i],tbl.userBusinessList$state[i],tbl.userBusinessList$countryName[i])})
    
    ##
    tbl.googleSearchHitsBusiness <- get.googleSearchHits(keywords = tbl.userBusinessList$businessKeywords[[1]], helpwords = tbl.userBusinessList$businessHelpWords[[1]])
    tbl.googleSearchHitsBusiness$webSearchType <- 'Businesses'
    cat('_________________________________________________\n')
    tbl.googleSearchHitsBusiness$uuid <- strtoi(tbl.userBusinessList$businessId[[1]])
    
    cat('________________________yes_________________________\n')
    ## combine both tables
    tbl.googleaSearchCombined <- bind_rows(tbl.googleSearchHits, tbl.googleSearchHitsBusiness)
    
  } else{
    cat('_________________________________________________\n')
    cat('__________________no business data___________________\n')
    cat('_________________________________________________\n')
    tbl.googleaSearchCombined <- tbl.googleSearchHits
  }

  # write(unlist(profileKeywords), file = "keywords.txt",append = FALSE, sep = "\n")
  # write(unlist(profileHelpwords), file = "helpwords.txt",append = FALSE, sep = "\n")
  # write(unlist(tbl.userBusinessList$businessKeywords[[1]]), file = "keywords.txt",append = TRUE, sep = "\n")
  # write(unlist(tbl.userBusinessList$businessHelpWords[[1]]), file = "helpwords.txt",append = TRUE, sep = "\n") 
  return(tbl.googleaSearchCombined)
}
###################################################################################################################################################
###################################################################################################################################################
# uuid <- 504
# tbl.searchGoogleItems <- tbl.googleaSearchCombined 
# tbl.searchGoogleItems <- get.webPresencePagesOutputTable(uuid)
# tbl.result <- get.createresultfile(tbl.searchGoogleItems,10747)