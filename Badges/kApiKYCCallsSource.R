# # install.packages("tidyjson")
# library("tidyjson", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("httr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("curl", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("curlconverter", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("jsonlite", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("stringi", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("curl", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("lsr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("stringi", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# library("strptimer", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library(RMySQL, pos=4,quietly=TRUE)
library(httr)
library(jsonlite)
# library(stringi)
# library(stringr)
# library(strptimer)
# library(curl)
library(dplyr)
# Use strftime( with formating) to dislay system time in text format
# library(strptimer)
# strftime(Sys.time(),"%Y-%m-%d %H:%M:%S")


# KYCUUID is uuid, kserv options '' for prod, 'dev', 'staging.'; amlPaid = 'true' or 'false  
get.apiKYCCallOld <- function(KYCUUID, kserv='prod.', amlPaid='false'){  
  # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
  attempts <- 0
  
  if (kserv == 'staging.') {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON(rawToChar(getAuthenticationToken$content))
      # for the 'staging' server look for the admin permissions token find the location index with grep 
      adminAuthenticationToken <- AuthenticationTokenGroup$token[ grep('admin',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat(KYCUUID,"-->get.apiKYCCall Staging BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
    }
  } else {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON( rawToChar(getAuthenticationToken$content))
      # for the 'Prod' server look for the person permissions token find the location index with grep 
      personAuthenticationToken <- AuthenticationTokenGroup$token[ grep('person',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat(KYCUUID,"-->get.apiKYCCall BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
      
    }
  }
  
  ## Part 2 call the data using the bearer token from part one, each server has a different token name to use
  attempts <- 0
  ## repeat the call if it 'hangsup'
  if (kserv == 'staging.') {
    bearerToken <-  adminAuthenticationToken
  } else {
    bearerToken <- personAuthenticationToken
  }
  repeat {
    attempts = attempts + 1
    # this Prod website data call with bearer token is
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 
    # 'http://kyc-api.kountable.com/v0/persons/2099?amlPaidCheck=false'
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: cv6yzIsmvMiwXFjit5-wtZHWPGBD-4l-' 
    # 'http://staging.kyc-api.kountable.com/v0/persons/2099?amlFriendsPaidCnt=0&amlFriendsFreeCnt=100'
    #http://prod.kyc-api.kountable.com/v0/doc#!/persons/getUser
    
    apiKYCget <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/persons/', KYCUUID,"?amlFriendsPaidCnt=0&amlFriendsFreeCnt=100", sep = ""),
                     add_headers(
                       "Accept" = "application/json",
                       "KYC-Bearer-Token" = bearerToken
                     ))
    ## How many call attempts
    cat(KYCUUID,"-->get.apiKYCCall attempts-->", attempts, ' Server--> ',kserv, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep="")
    
    # exit if the condition is met
    if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain"
              ,rawToChar(apiKYCget$content))==FALSE){ break} 
  }
  
  x <- fromJSON( rawToChar(apiKYCget$content), simplifyVector = TRUE, simplifyMatrix = TRUE, simplifyDataFrame = TRUE)
  return(x) 
}
# get.apiKYCCall('10395')

get.apiKYCCallNoProfile <- function(KYCUUID, kserv='prod.', amlPaid='false', amlFriendsPaidCnt=0, amlFriendsFreeCnt=100) {  
  require(httr)
  require(jsonlite)
  # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
  # cat(KYCUUID," Called new get.apiKYCCAll(6) -->kserv=",kserv," amlPaid=",amlPaid," amlFriendsPaidCnt=",amlFriendsPaidCnt," amlFriendsFreeCnt=",amlFriendsFreeCnt,"\n",sep = "")
  attempts <- 0
  
  if (kserv == 'staging.') {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON(rawToChar(getAuthenticationToken$content))
      # for the 'staging' server look for the admin permissions token find the location index with grep 
      adminAuthenticationToken <- AuthenticationTokenGroup$token[ grep('admin',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat(KYCUUID,"--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
    }
  } else {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON( rawToChar(getAuthenticationToken$content))
      # for the 'Prod' server look for the person permissions token find the location index with grep 
      personAuthenticationToken <- AuthenticationTokenGroup$token[ grep('person',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      # cat(KYCUUID,"--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
      
    }
  }
  
  ## Part 2 call the data using the bearer token from part one, each server has a different token name to use
  attempts <- 0
  ## repeat the call if it 'hangsup'
  if (kserv == 'staging.') {
    bearerToken <-  adminAuthenticationToken
  } else {
    bearerToken <- personAuthenticationToken
  }
  repeat {
    attempts = attempts + 1
    # this Prod website data call with bearer token is
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 
    # 'http://kyc-api.kountable.com/v0/persons/2099?amlPaidCheck=false'
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: cv6yzIsmvMiwXFjit5-wtZHWPGBD-4l-' 
    # 'http://staging.kyc-api.kountable.com/v0/persons/2099?amlFriendsPaidCnt=0&amlFriendsFreeCnt=100'
    ## october 18, 2016
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 
    # 'http://kyc-api.kountable.com/v0/persons/10747?amlPaidCheck=false&amlFriendsPaidCnt=0&amlFriendsFreeCnt=100'
    
    
    apiKYCget <- GET(paste0('http://',kserv,'kyc-api.kountable.com/v0/persons/', KYCUUID,'?amlPaidCheck=',amlPaid,'&amlFriendsPaidCnt=',amlFriendsPaidCnt,'&amlFriendsFreeCnt=',amlFriendsFreeCnt),
                     add_headers(
                       "Accept" = "application/json",
                       "KYC-Bearer-Token" = bearerToken
                     ))
    ## How many call attempts
    cat(KYCUUID,'--> apiKYCCall TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep="")
    
    # exit if the condition is met
    if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain"
              ,rawToChar(apiKYCget$content))==FALSE){ break} 
  }
  
  x <- fromJSON( rawToChar(apiKYCget$content), simplifyVector = TRUE, simplifyMatrix = TRUE, simplifyDataFrame = TRUE)
  
  ## check to make sure profile has data if not call swagger profile
  if(is.null(x$profile)){
    # cat('================================================================\n')
    # cat('================================================================\n')
    # cat('======================no Profile data===========================\n')
    # cat('================================================================\n')
    # cat('================================================================\n')
    #  x$profile <- get.V2UserProfile(apiUUID = x$uuid)
  }
  
  return(x) 
}

# KYCUUID is uuid, kserv options '' for prod, 'dev', 'staging.'; amlPaid = 'true' or 'false  
# kserv <- 'prod.
get.apiKYCCall <- function(KYCUUID, kserv='prod.', amlPaid='false', amlFriendsPaidCnt=0, amlFriendsFreeCnt=100) {  
  require(httr)
  require(jsonlite)
  # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
  cat(KYCUUID," Called new get.apiKYCCAll(6) -->kserv=",kserv," amlPaid=",amlPaid," amlFriendsPaidCnt=",amlFriendsPaidCnt," amlFriendsFreeCnt=",amlFriendsFreeCnt,"\n",sep = "")
  attempts <- 0
  
  if (kserv == 'staging.') {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON(rawToChar(getAuthenticationToken$content))
      # for the 'staging' server look for the admin permissions token find the location index with grep 
      adminAuthenticationToken <- AuthenticationTokenGroup$token[ grep('admin',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat(KYCUUID,"--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
    }
  } else {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON( rawToChar(getAuthenticationToken$content))
      # for the 'Prod' server look for the person permissions token find the location index with grep 
      personAuthenticationToken <- AuthenticationTokenGroup$token[ grep('person',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat(KYCUUID,"--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
      
    }
  }
  
  ## Part 2 call the data using the bearer token from part one, each server has a different token name to use
  attempts <- 0
  ## repeat the call if it 'hangsup'
  if (kserv == 'staging.') {
    bearerToken <-  adminAuthenticationToken
  } else {
    bearerToken <- personAuthenticationToken
  }
  repeat {
    attempts = attempts + 1
    # this Prod website data call with bearer token is
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 
    # 'http://kyc-api.kountable.com/v0/persons/2099?amlPaidCheck=false'
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: cv6yzIsmvMiwXFjit5-wtZHWPGBD-4l-' 
    # 'http://staging.kyc-api.kountable.com/v0/persons/2099?amlFriendsPaidCnt=0&amlFriendsFreeCnt=100'
    ## october 18, 2016
    # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 
    # 'http://kyc-api.kountable.com/v0/persons/10747?amlPaidCheck=false&amlFriendsPaidCnt=0&amlFriendsFreeCnt=100'
    
    
    apiKYCget <- GET(paste0('http://',kserv,'kyc-api.kountable.com/v0/persons/', KYCUUID,'?amlPaidCheck=',amlPaid,'&amlFriendsPaidCnt=',amlFriendsPaidCnt,'&amlFriendsFreeCnt=',amlFriendsFreeCnt),
                     add_headers(
                       "Accept" = "application/json",
                       "KYC-Bearer-Token" = bearerToken
                     ))
    ## How many call attempts
    cat(KYCUUID,"--> apiKYCCall6 attempts-->", attempts, ' Server--> ',kserv, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep="")
    
    # exit if the condition is met
    if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain"
              ,rawToChar(apiKYCget$content))==FALSE){ break} 
  }
  
  x <- fromJSON( rawToChar(apiKYCget$content), simplifyVector = TRUE, simplifyMatrix = TRUE, simplifyDataFrame = TRUE)
  
  ## check to make sure profile has data if not call swagger profile
  if(is.null(x$profile)){
    cat('================================================================\n')
    cat('================================================================\n')
    cat('======================no Profile data===========================\n')
    cat('================================================================\n')
    cat('================================================================\n')
    x$profile <- get.V2UserProfile(apiUUID = x$uuid)
  }
  
  return(x) 
}


# apikycCall <- get.apiKYCCall('10198')



### we are pulling json formatted text from within a Json format
get.webPresencepages <- function(uuidApiKYC1){
  
  uuidApiWebKYC1 <- lapply(uuidApiKYC1$collectedProfile$web$webpresencePages, fromJSON, simplifyVector = TRUE,simplifyDataFrame = TRUE,simplifyMatrix = TRUE)
  names(uuidApiWebKYC1) <- paste0('url_',sprintf("%03d",1:length(uuidApiWebKYC1)))
  
  ## which bestParts are greater than 0
  ## only keep those with found text in bestParts
  uuidApiWebKYC1 <- uuidApiWebKYC1[which(lapply(uuidApiWebKYC1, function(x) {length(x[['bestParts']])})>0)]
  
  ### now put matchedItems and otherItems into one dataframe per url
  ###Success!!! this goes using the bind_rows(c(i,j)) logic solves it!  the list within solution for dplyr
  uuidApiWebMatchedOtherKYC <- lapply(uuidApiWebKYC1,function(j){lapply(1:length(j[['bestParts']][['relatedness']]), function(x){ bind_rows( c(j[['bestParts']][['matchedItems']][x],j[['bestParts']][['otherItems']][x]))})})
  
  #lapply(uuidApiWebKYC1,function(x){x[['otherMatched']] <- uuidApiWebMatchedOtherKYC[[x]]})
  
  for (i in 1:length(uuidApiWebKYC1)) {
    uuidApiWebKYC1[[i]]$bestParts$otherMatched <- uuidApiWebMatchedOtherKYC[[i]]
    
  }
  return(uuidApiWebKYC1)
}

get.categoryWebSummary <- function(uuidApiWebKYC1,slideRange) {
  ## put categories into a list or table
  # make a tbl_df with the check box results then just keep the 1s to display
  # a min max range on every category airlines .445 - .670
  ## create list of url categories and values
  cat('-->get.categoryWebSummary  TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
  categ1 <- lapply(min(slideRange):max(slideRange), function(i) {uuidApiWebKYC1[[i]]$categories})
  ## turn each urls list of categories into a data.frame use dplyr's bind_rows to turn the list of dataframe categories into a  
  tbl.categories <- bind_rows(lapply(categ1,function(x){data.frame(categories=names(x),values=unlist(x,use.names = FALSE))}))
  cat('-->get.categoryWebSummary  Length of Categories:', length(tbl.categories),' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
  tbl.categories <- tbl.categories %>% group_by(categories) %>% summarise(freq=n(),min=min(values),max=max(values))
  return(tbl.categories)
}
## sample url code for DT

# get.socialMediaData <- function(uuidApiKYC1){
#   ## list of icons used on major providers
#   iconList <-   tagList(
#     tags$img(src = "http://icons.iconarchive.com/icons/fasticon/web-2/48/FaceBook-icon.png", height=25, width=25),
#     tags$img(src = "https://cdn1.iconfinder.com/data/icons/iconza-circle-social/64/697029-twitter-128.png",height=25, width=25),
#     tags$img(src = "http://icons.iconarchive.com/icons/designbolts/free-instagram/128/Active-Instagram-3-icon.png", height=25, width=25),                             
#     tags$img(src = "https://cdn0.iconfinder.com/data/icons/social-network-7/50/9-128.png", height=25, width=25),
#     tags$img(src = "http://icons.iconarchive.com/icons/danleech/simple/128/yahoo-icon.png", height=25, width=25),
#     tags$img(src = "http://icons.iconarchive.com/icons/carlosjj/google-jfk/128/googleplus-icon.png", height=25, width=25),
#     tags$img(src = "http://icons.iconarchive.com/icons/tatice/cristal-intense/128/Apple-grey-icon.png", height=25, width=25),
#     tags$img(src = "http://icons.iconarchive.com/icons/icons8/windows-8/128/Systems-Android-Os-icon.png", height=25, width=25),
#     tags$img(src = "http://icons.iconarchive.com/icons/dtafalonso/android-l/128/Gmail-icon.png", height=25, width=25)
#   )
#   iconList <- lapply(iconList,paste)
#   
#   
#   ## complete list
#   providerNameList <- c("Facebook","Twitter","Instagram","LinkedIn","Yahoo","Google+","iOS mobile app","Android mobile app","Gmail")
#   providerProfileName <- c("Facebook","Twitter","Instagram","LinkedIn","Yahoo","Google+") # these have photo url and profile url
#   nonProviderName <- providerNameList[which(!(providerNameList %in% providerProfileName))] #the not ! before is a nice trick for not in
#   
#   ## add the names to the icons for indexing provider$name
#   names(iconList) <- providerNameList
#   
#   ## use dplyr tbl_df for ease of use with identities$provider which is currently a data.frame
#   tbl.provider <- tbl_df(uuidApiKYC1$identities$provider)
#   tbl.provider$photoUrl <- uuidApiKYC1$identities$photoUrl
#   tbl.provider$profileUrl <- uuidApiKYC1$identities$profileUrl
#   
#   
#   
#   ## we want to only display photos or profiles where they should exist
#   ## if provider and photo display photo, else diplay no 'provider' photo url
#   ## if provider and profile display profile, else display no 'provider' profile url
#   ## if non provider display NA in photo and profile url
#   
#   ##get rid of Gigya
#   tbl.provider <- tbl.provider %>% filter(name != "Gigya")
#   
#   # use filter to make non provider and provider tables
#   # then row_bind them back into original table (dplyr has no in row mutate function)
#   
#   # subset non providers and fix photo and profile url with NA
#   tbl.nonProvider <- tbl.provider %>% filter(name %in% nonProviderName) %>% rowwise() %>% mutate(profileUrl='NA', photoUrl='NA')
#   
#   tbl.providerMatches <- tbl.provider %>% filter(name %in% providerProfileName) %>% rowwise() %>% 
#     mutate(profileUrl=ifelse(profileUrl=='',paste0("No ",paste0(name)," Profile Url"),paste0(tags$a("View ", paste0(name), " Profile", href = profileUrl))), 
#            photoUrl=ifelse(photoUrl=='',paste0("No ",paste0(name)," Photo Url"),paste0(tags$img(src = photoUrl, height=100, width=100))))
#   #glimpse(tbl.providerMatches)
#   
#   tbl.providerModified <- bind_rows(tbl.nonProvider,tbl.providerMatches)
#   #glimpse(tbl.providerModified)
#   
#   ## add icons
#   tbl.providerModified$icon <- unlist(unname(iconList[tbl.providerModified$name]))
#   #glimpse(tbl.providerModified)
#   
#   ## keep only key columns for now
#   tbl.providerKey <- tbl.providerModified %>% select(icon,name,photoUrl,profileUrl)
#   return(tbl.providerKey)
#   
# }
get.socialMediaData <- function(uuidApiKYC1){
  ## list of icons used on major providers
  iconList <-   tagList(
    tags$img(src = "http://icons.iconarchive.com/icons/fasticon/web-2/48/FaceBook-icon.png", height=25, width=25),
    tags$img(src = "https://cdn1.iconfinder.com/data/icons/iconza-circle-social/64/697029-twitter-128.png",height=25, width=25),
    tags$img(src = "http://icons.iconarchive.com/icons/designbolts/free-instagram/128/Active-Instagram-3-icon.png", height=25, width=25),                             
    tags$img(src = "https://cdn0.iconfinder.com/data/icons/social-network-7/50/9-128.png", height=25, width=25),
    tags$img(src = "http://icons.iconarchive.com/icons/danleech/simple/128/yahoo-icon.png", height=25, width=25),
    tags$img(src = "http://icons.iconarchive.com/icons/carlosjj/google-jfk/128/googleplus-icon.png", height=25, width=25),
    tags$img(src = "http://icons.iconarchive.com/icons/tatice/cristal-intense/128/Apple-grey-icon.png", height=25, width=25),
    tags$img(src = "http://icons.iconarchive.com/icons/icons8/windows-8/128/Systems-Android-Os-icon.png", height=25, width=25),
    tags$img(src = "http://icons.iconarchive.com/icons/dtafalonso/android-l/128/Gmail-icon.png", height=25, width=25)
  )
  iconList <- lapply(iconList,paste)
  
  
  ## complete list
  providerNameList <- c("Facebook","Twitter","Instagram","LinkedIn","Yahoo","Google+","iOS mobile app","Android mobile app","Gmail")
  providerProfileName <- c("Facebook","Twitter","Instagram","LinkedIn","Yahoo","Google+") # these have photo url and profile url
  nonProviderName <- providerNameList[which(!(providerNameList %in% providerProfileName))] #the not ! before is a nice trick for not in
  
  ## add the names to the icons for indexing provider$name
  names(iconList) <- providerNameList
  
  ## use dplyr tbl_df for ease of use with identities$provider which is currently a data.frame
  tbl.provider <- tbl_df(uuidApiKYC1$identities$provider)
  tbl.provider$photoUrl <- uuidApiKYC1$identities$photoUrl
  tbl.provider$profileUrl <- uuidApiKYC1$identities$profileUrl
  
  ## first add providerUid to table
  tbl.provider$providerUid <- uuidApiKYC1$identities$providerUid
  
  ##get rid of Gigya
  tbl.provider <- tbl.provider %>% filter(name != "Gigya")
  
  # use filter to make non provider and provider tables
  # subset non providers and fix photo and profile url with NA
  tbl.nonProvider <- tbl.provider %>% filter(name %in% nonProviderName) %>% rowwise() %>% mutate(profileUrl='NA', photoUrl='NA')
  
  ## we want to only display photos or profiles where they should exist
  ## if provider and photo display photo, else diplay no 'provider' photo url
  ## then build the constructedProfileUrl complete with html a href tags by profile name
 ## facebook https://facebook.com/<UID>   https://facebook.com/1001765633264274
  ## twitter https://twitter.com/intent/user?user_id=twitter_id   https://twitter.com/intent/user?user_id=739931893722980360   
  ## instagram 	https://instagram.com/user/?id={USER_ID}1251854138  https://instagram.com/user/?id=1251854138
  ## linkedIn http://www.linkedin.com/profile/qa?id=
  ## Yahoo uri in cassandra yahoo requires oath token, unkown how to construct yahoo profile
  ## googleplus  https://plus.google.com/111030069939436266012
  tbl.providerMatches <- tbl.provider %>% filter(name %in% providerProfileName) %>% rowwise() %>% 
    mutate(photoUrl=ifelse(photoUrl=='',paste0("No ",paste0(name)," Photo Url"),paste0(tags$img(src = photoUrl, height=100, width=100))),
           constructedProfileUrl = 
             ifelse(grepl('FACEBOOK',code), paste0(tags$a("Facebook profile",href = paste0('https://facebook.com/',providerUid),target='_blank',style='float:right')),
                    ifelse(grepl('TWITTER',code), paste0(tags$a("Twitter profile",href = paste0('https://twitter.com/intent/user?user_id=',providerUid),target='_blank',style='float:right')),
                           ifelse(grepl('INSTAGRAM',code), paste0(tags$a("Instagram profile",href = paste0('https://instagram.com/user/?id=',providerUid),target='_blank',style='float:right')),
                                  # ifelse(grepl('LINKEDIN',code), paste0(tags$a("LinkedIn profile",href = paste0('http://www.linkedin.com/profile/qa?id=',providerUid),target='_blank',style='float:right')),
                                  ifelse(grepl('GOOGLEPLUS',code), paste0(tags$a("Googleplus profile",href = paste0('https://plus.google.com/',providerUid),target='_blank',style='float:right')),
                                         "No Profile Url")))))
  
  
  ## Finally, if the original profileUrl field is empty, replace it with constructedProfileUrl
  tbl.providerMatches <-  tbl.providerMatches %>% rowwise() %>% 
    mutate(profileUrl = ifelse(profileUrl == '', constructedProfileUrl, paste0(tags$a(profileUrl, href=profileUrl,target='_blank'))))
  
  
  # then row_bind them back into original table (dplyr has no in row mutate function)
  tbl.providerModified <- bind_rows(tbl.nonProvider,tbl.providerMatches)
  
  ## add icons
  tbl.providerModified$icon <- unlist(unname(iconList[tbl.providerModified$name]))
  #glimpse(tbl.providerModified)
  
  ## keep only key columns for now
  tbl.providerKey <- tbl.providerModified %>% select(icon,name,photoUrl,profileUrl)
  return(tbl.providerKey)
  
}

get.locationsImages <- function(df.locations) {
  require(dplyr)
  require(shiny)
  if (is.null(df.locations)){ return(NULL)}
  ## use sapply on the list of images and sapply within to create a chracter vector equal to the number of rows in the locations table
  ## then join the character vector to the dataframe as images
  #imgChar <- sapply(df.locations$images, function(i) {paste(sapply(i, function(x){paste0(tags$img(src = x, height=100, width=100))}),collapse=', ')})
  imgChar <- paste0(sapply(df.locations$images, function(i) {paste(sapply(i, function(x){paste0(tags$a(tags$img(src = x, height=50, width=100),href=x,target='_blank'))}))}), collapse = '')
  
  df.locations$images <- imgChar
  ##order dataframe columns and convert to tbl(dplyr)
  ## street dissapeared 2016-07-12 
  
  if (is.null(df.locations$street)) {
    df.locations <- tbl_df(df.locations[c("userNote","name", 'images', 'lt','lg','city','state','country')])
  } else {
    df.locations <- tbl_df(df.locations[c("userNote","name", 'images', 'lt','lg','street','city','state','country')])
  }
  
  
  return(df.locations)
}
get.kycDocumentsTbl <- function(kycDocuments){
  require(dplyr)
  require(shiny)
  cat('Called get.kycDocumentsTbl','\n', sep = '')
  ## select all columns but dataframes items and dataframe type
  kycDocs <- tbl_df(kycDocuments %>% select(-items,-type))
  # colnames(kycDocuments) <- paste0("documents.",colnames(kycDocuments))
  
  ## select all columns but dataframes file 
  ## then bind_rows to make table sapply gets rid of the list of 1 in between !!!!!!
  kycDocFiles <- bind_rows(sapply(kycDocuments$items, dplyr::select, file))
  
  ## add file. to filenames and order table by list
  colnames(kycDocFiles) <- paste0('file.',names(kycDocFiles))
  kycDocFiles <- kycDocFiles[ c('file.id','file.name','file.description','file.mimeType','file.lat','file.lng','file.url','file.timestamp')]
  
  ## use mutate to add the hyperlink and image tags to file.url
  kycDocFiles <- kycDocFiles %>% rowwise() %>% mutate(file.url=paste0(tags$a(tags$img(src = file.url, height=300, width=300),href = file.url,target='_blank')))
  
  ## for the non subsets coluns, documentId, id, sortOrd uses lapply, bind_rows will adapt and combine them all
  kycDocItems <- bind_rows(lapply(kycDocuments$items, dplyr::select, documentId,id, sortOrd ))
  colnames(kycDocItems) <- paste0('items.',names(kycDocItems))
  
  ## for the non subsets coluns, documentId, id, sortOrd uses lapply, bind_rows will adapt and combine them all
  kycDocType <- tbl_df(kycDocuments$type)
  colnames(kycDocType) <- paste0('type.',names(kycDocType))
  
  ## ok if you have multipage documents, use table on documentId to identify their frequencies
  ## use rep by their freq convert to integer and use to repeat rownames
  ## to repeate rows in a dataframe accordingly
  multiPageDocs <- as.integer(rep(rownames(kycDocs),table(kycDocItems$items.documentId)))
  
  
  
  ## combine them all together and maker urls images with hyperlinks
  ## use multiPageDocs in case their are multPage file uploads in file on KYC Documents and kyc Doc Type
  tbl.kycDocuments <- data.frame(kycDocs[multiPageDocs,],kycDocItems,kycDocType[multiPageDocs,],kycDocFiles)
  ## combine them all together and maker urls images with hyperlinks
  # tbl.kycDocuments <- data.frame(kycDocs,kycDocItems,kycDocType,kycDocFiles)
  tbl.kycDocuments <- tbl.kycDocuments[c(1,2,11,9,19,17,18,20,3,5,6,13)]
  cat('made it to the end of kycDocumentsTbl with length tbl.kycDocuments = ',length(tbl.kycDocuments),'\n', sep = '')
  # print(tbl.kycDocuments)
  
  return(tbl.kycDocuments)
}

# apiUUID <- 10450

get.V2UserProfile <- function(apiUUID = '504') { 
  ### Returns the search list by userId and name
  require(httr)
  require(dplyr)
  require(jsonlite)
  ##### SWAGGER for project calls from api.kountable.com
  cat("---> Called get.V2UsersList","\n",sep = "")
  
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         "k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  cat("---> BearerToken: ", accessToken$data$access_token,"\n",sep = "")
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/users/',apiUUID),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  ## check for errors in code leave commented out
  # length(getV2Swagger$data)
  # str(getV2Swagger$data)
  # 
  # profileNames <- c("id", "createdAt","updatedAt","createdBy","statusId","firstName","lastName","middleName","primaryEmail","primaryImageFull",
  #   "primaryImageThumb","emailVerified","district","sector","primaryPhone","skype","phoneVerified","avatar","firebaseIdentifier",
  #   "isNew","address","address2","city","state","province","countryId","country","postalCode", "govIdNumber", "passportId", "birthday", "nationalityId")
  # paste0(names(getV2Swagger$data), profileNames)
  
  ## hopefully can delete this
  # if(!is.null( getV2Swagger$data[["referrer_id"]])){getV2Swagger$data[["referrer_id"]] <- NULL}
  # if(!is.null( getV2Swagger$data[["campaign_id"]])){getV2Swagger$data[["campaign_id"]] <- NULL}
  # if(!is.null( getV2Swagger$data[["password_recovery_token"]])){getV2Swagger$data[["password_recovery_token"]] <- NULL}
  # if(is.null( getV2Swagger$data[["primary_email"]])){getV2Swagger$data[["primary_email"]] <- ''}
  # 
  ## fix messed up country id change
  if(!is.null(getV2Swagger$data$country)){
    getV2Swagger$data$country <- getV2Swagger$data$country$name
    
  }
  
  ls.profile <- getV2Swagger$data
  cat('length of ls.profile ',length(ls.profile),'\n')
  
  ## correct to camelcase
  ## use rapportools tocamel() function to convert is_new to isNew
  require(rapportools)
  names(ls.profile) <- tocamel(names(ls.profile))
  # names(ls.profile) <- c("id", "createdAt",
  #                        "updatedAt",
  #                        "createdBy",
  #                        "statusId",
  #                        "firstName",
  #                        "lastName",
  #                        "middleName",
  #                        "primaryEmail",
  #                        "primaryImageFull",
  #                        "primaryImageThumb",
  #                        "emailVerified",
  #                        "district",
  #                        "sector",
  #                        "primaryPhone",
  #                        "skype",
  #                        "phoneVerified",
  #                        "avatar",
  #                        "firebaseIdentifier",
  #                        "isNew",
  #                        "address",
  #                        "address2",
  #                        "city",
  #                        "state",
  #                        "province",
  #                        "countryId",
  #                        "country",
  #                        "postalCode", "govIdNumber", "passportId", "birthday", "nationalityId")
  # 
  
  
  return(ls.profile)
}


get.frankProfileTable <- function(uuidApiKYC1) {
  require(dplyr)
  require(shiny)
  
  ## check to make sure profile has data if not call swagger profile
  if(is.null(uuidApiKYC1$profile)){
    uuidApiKYC1$profile <- get.V2UserProfile(apiUUID = uuidApiKYC1$uuid)
  }
  ## build initial table of 20 fields from profile call
  tbl.profile <- tbl_df(data.frame(profile=names(unlist(uuidApiKYC1$profile)), data = unname(unlist(uuidApiKYC1$profile)), stringsAsFactors = F))
  cat('nrows tbl.profile ->',nrow(tbl.profile),"\n", sep = '')
  # ## add governmentId, passportId and birthDate to table
  # missingProfileFieldsNames <- c('governmentId','passportId','birthDate', 'nationality')
  # missingProfileFieldsData <- c('','','','')
  # tbl.profile <- bind_rows(tbl.profile,data.frame(profile=missingProfileFieldsNames, data = missingProfileFieldsData))
  # cat('nrows tbl.profile add 4 missing->',nrow(tbl.profile),"\n", sep = '')
  # 
  ## keep everything but the reffererId its intermittant and may be only apart of the June 24 update of the app?
  # tbl.profile <- tbl.profile %>% rowwise() %>% filter(profile != 'referrerId')
  # cat('nrows tbl.profile remove referreId missing->',nrow(tbl.profile),"\n", sep = '')
  # print(tbl.profile$profile)
  # ##get profile sort order field sortBy, kActive, kScore
  ##built table using paste_from_clipboard, toJSON, copied and pasted output added single quotes, etc.
  #sortProfileBy <- fromJSON('[{"sortBy":10,"kActive":1,"kScore":2},{"sortBy":19,"kActive":1,"kScore":10},{"sortBy":22,"kActive":1,"kScore":0},{"sortBy":17,"kActive":1,"kScore":2},{"sortBy":5,"kActive":1,"kScore":0},{"sortBy":11,"kActive":1,"kScore":2},{"sortBy":1,"kActive":1,"kScore":10},{"sortBy":13,"kActive":1,"kScore":2},{"sortBy":8,"kActive":1,"kScore":0},{"sortBy":12,"kActive":1,"kScore":2},{"sortBy":4,"kActive":1,"kScore":2},{"sortBy":3,"kActive":1,"kScore":2},{"sortBy":20,"kActive":1,"kScore":10},{"sortBy":23,"kActive":1,"kScore":0},{"sortBy":18,"kActive":1,"kScore":2},{"sortBy":21,"kActive":1,"kScore":0},{"sortBy":9,"kActive":1,"kScore":0},{"sortBy":6,"kActive":1,"kScore":10},{"sortBy":2,"kActive":0,"kScore":0},{"sortBy":7,"kActive":1,"kScore":0},{"sortBy":14,"kActive":0,"kScore":10},{"sortBy":15,"kActive":0,"kScore":10},{"sortBy":16,"kActive":0,"kScore":5}]')
  # if (nrow(tbl.profile)==25) {
  #   sortProfileBy <- fromJSON('[{"sortBy":10,"kActive":1,"kScore":2},{"sortBy":26,"kActive":0,"kScore":0},{"sortBy":20,"kActive":1,"kScore":10},{"sortBy":24,"kActive":1,"kScore":0},{"sortBy":17,"kActive":1,"kScore":0},{"sortBy":5,"kActive":1,"kScore":0},{"sortBy":11,"kActive":1,"kScore":2},{"sortBy":1,"kActive":1,"kScore":10},{"sortBy":13,"kActive":1,"kScore":2},{"sortBy":8,"kActive":1,"kScore":0},{"sortBy":12,"kActive":1,"kScore":2},{"sortBy":4,"kActive":1,"kScore":2},{"sortBy":3,"kActive":1,"kScore":2},{"sortBy":21,"kActive":1,"kScore":10},{"sortBy":25,"kActive":1,"kScore":0},{"sortBy":19,"kActive":1,"kScore":0},{"sortBy":23,"kActive":1,"kScore":0},{"sortBy":9,"kActive":1,"kScore":0},{"sortBy":6,"kActive":1,"kScore":10},{"sortBy":2,"kActive":0,"kScore":0},{"sortBy":7,"kActive":1,"kScore":0},{"sortBy":14,"kActive":0,"kScore":10},{"sortBy":15,"kActive":0,"kScore":10},{"sortBy":16,"kActive":0,"kScore":5},{"sortBy":18,"kActive":0,"kScore":5}]')  
  # }else {
  #   sortProfileBy <- fromJSON('[{"sortBy":10,"kActive":1,"kScore":2},{"sortBy":26,"kActive":0,"kScore":0},{"sortBy":20,"kActive":1,"kScore":10},{"sortBy":24,"kActive":1,"kScore":0},{"sortBy":17,"kActive":1,"kScore":0},{"sortBy":5,"kActive":1,"kScore":0},{"sortBy":11,"kActive":1,"kScore":2},{"sortBy":1,"kActive":1,"kScore":10},{"sortBy":13,"kActive":1,"kScore":2},{"sortBy":8,"kActive":1,"kScore":0},{"sortBy":22,"kActive":0,"kScore":0},{"sortBy":12,"kActive":1,"kScore":2},{"sortBy":4,"kActive":1,"kScore":2},{"sortBy":3,"kActive":1,"kScore":2},{"sortBy":21,"kActive":1,"kScore":10},{"sortBy":25,"kActive":1,"kScore":0},{"sortBy":19,"kActive":1,"kScore":0},{"sortBy":23,"kActive":1,"kScore":0},{"sortBy":9,"kActive":1,"kScore":0},{"sortBy":6,"kActive":1,"kScore":10},{"sortBy":2,"kActive":0,"kScore":0},{"sortBy":7,"kActive":1,"kScore":0},{"sortBy":14,"kActive":0,"kScore":10},{"sortBy":15,"kActive":0,"kScore":10},{"sortBy":16,"kActive":0,"kScore":5},{"sortBy":18,"kActive":0,"kScore":5}]')
  #   
  # }
  #toJSON(sortProfileBy)
  # sortProfileBy <- paste_from_clipboard(sep = "\t", header = T)
  ## add profile sort order to tbl and sort table with arrange(tbl.profile,sortBy) and drop sortBy
  # tbl.profile <- bind_cols(sortProfileBy,tbl.profile)
  # tbl.profile <- tbl.profile %>% arrange(sortBy) %>% select(-sortBy)
  # ## Here is the kscore table version 2 which is the version with key and using dplyr to merge with 
  ## whatever fields are in the current call.  We are attempting to be fluid by scoring only fields we decide before hand
  ## sortPrwhofileBy is made in a numbers spreadsheet
  sortProfileBy <- fromJSON('[{"sortBy":6,"kActive":1,"kScore":2,"profile":"city"},{"sortBy":21,"kActive":1,"kScore":0,"profile":"updatedAt"},{"sortBy":16,"kActive":1,"kScore":10,"profile":"emailVerified"},{"sortBy":22,"kActive":1,"kScore":0,"profile":"avatar"},{"sortBy":11,"kActive":1,"kScore":0,"profile":"primaryEmail"},{"sortBy":23,"kActive":1,"kScore":0,"profile":"middleName"},{"sortBy":7,"kActive":1,"kScore":2,"profile":"state"},{"sortBy":1,"kActive":1,"kScore":10,"profile":"primaryImageFull"},{"sortBy":9,"kActive":1,"kScore":2,"profile":"country"},{"sortBy":24,"kActive":1,"kScore":0,"profile":"sector"},{"sortBy":8,"kActive":1,"kScore":2,"profile":"postalCode"},{"sortBy":3,"kActive":1,"kScore":5,"profile":"lastName"},{"sortBy":2,"kActive":1,"kScore":5,"profile":"firstName"},{"sortBy":17,"kActive":1,"kScore":10,"profile":"phoneVerified"},{"sortBy":25,"kActive":1,"kScore":0,"profile":"statusId"},{"sortBy":10,"kActive":1,"kScore":0,"profile":"primaryPhone"},{"sortBy":20,"kActive":1,"kScore":0,"profile":"createdAt"},{"sortBy":18,"kActive":1,"kScore":2,"profile":"province"},{"sortBy":4,"kActive":1,"kScore":2,"profile":"address"},{"sortBy":26,"kActive":1,"kScore":0,"profile":"primaryImageThumb"},{"sortBy":19,"kActive":1,"kScore":2,"profile":"district"},{"sortBy":5,"kActive":1,"kScore":0,"profile":"address2"},{"sortBy":12,"kActive":1,"kScore":10,"profile":"govIdNumber"},{"sortBy":13,"kActive":1,"kScore":10,"profile":"passportId"},{"sortBy":14,"kActive":1,"kScore":10,"profile":"birthday"},{"sortBy":15,"kActive":1,"kScore":5,"profile":"nationalityId"}]')
  # write.table(tbl.profile, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)
  # sortProfileBy <- paste_from_clipboard(sep='\t', header = T)
  # toJSON(sortProfileBy)
  
  ## full join by Key
  ## arrange by sortBy field all missing should go to the bottom then remove sortBy column
  
  tbl.profile <- dplyr::full_join( tbl.profile, sortProfileBy, by = "profile")
  tbl.profile <- tbl.profile %>% arrange(sortBy) %>% select(-sortBy)
  
  
  ## if kscore or kactive are NA change to 0
  tbl.profile$kActive[which(is.na(tbl.profile$kActive))] <- 0
  tbl.profile$kScore[which(is.na(tbl.profile$kScore))] <- 0
  
  ## calculate Kscore Red by checking for NAN, NA, blank in data field
  naData <- -as.integer(is.na(tbl.profile$data)) 
  nanData <- -as.integer(is.nan(tbl.profile$data))
  blankData <- -as.integer(nchar(tbl.profile$data) < 1)
  ## add the three vectors then add them to the kActive Vector
  emptyData <- naData + nanData + blankData
  ## but first if kActive is not counted or 0 make sure emptyData is also 0 and not negative 1
  emptyData[which(tbl.profile$kActive == 0)] <- 0
  
  tbl.profile$missingData <- tbl.profile$kActive + emptyData
  
  ## email verified and phone verified true/false should be 0/1 in missing data
  tbl.profile$missingData[which(tbl.profile$data %in% c("TRUE","FALSE"))] <- as.integer(as.logical(tbl.profile$data[which(tbl.profile$data %in% c("TRUE","FALSE"))]))
  
  ## make sure NAs are 0 so all calculations work
  tbl.profile$missingData[which(is.na(tbl.profile$missingData))] <- 0
  
  ## multiply by Kscore
  tbl.profile$FrankProfileKScore <- tbl.profile$missingData * tbl.profile$kScore
  
  ## add numericInputs and checkboxes to table
  ## put tags on photos
  
  tbl.profile$data[which(tbl.profile$profile %in% c("primaryImageFull","avatar"))] <- paste0(tags$a(tags$img(src = tbl.profile$data[which(tbl.profile$profile %in% c("primaryImageFull","avatar"))], height=250, width=250),href = tbl.profile$data[which(tbl.profile$profile %in% c("primaryImageFull","avatar"))],target='_blank',style='float:right'))
  
  # tbl.profile <- tbl.profile %>% rowwise() %>% 
  #   mutate(kScore = paste0(tags$div(numericInput(paste0("kNum_",profile),label="",value = kScore, width=75),style='float:center-block')),
  #          kActive = paste0(tags$div(br(),checkboxInput(paste0("kBox_",profile), label="", value = as.logical(as.integer(missingData)), width = 30),style='float:right')),
  #          data = ifelse(regexpr('http',data) == 1, paste0(tags$a(tags$img(src = data, height=250, width=250),href = data,target='_blank',style='float:right')),data))
  # 
  ## now lets get rid of checkboxes and numericInputs for 1, 15-25 rows
  ## tbl.profile[ c(2,5,7,8,9,22:25),c('kActive','kScore')] <- ''
  ## now we'll keep only the rows we want to display
  ## tbl.profile <- tbl.profile[ c(1,3:4,6:22,24),]
  
  
  #View(tbl.profile)
  
  return(tbl.profile)
  
}

get.frankBusinessData <- function(uuidApiKYC1){
  require(dplyr)
  cat('call-> get.frankBusinessData -> any data? length should be 20', length(uuidApiKYC1$businessReputation$information), '\n', sep = "  ")
  if(is.null(uuidApiKYC1$businessReputation)){return(NULL)}
  print(uuidApiKYC1$businessReputation$information)
  ## make it a dyplr table
  ## keep on the first record, the first record appears to be FrankBusiness
  tbl.business <- tbl_df(uuidApiKYC1$businessReputation$information[1,])
  
  tbl.business <- data.frame(profile=names(unlist(tbl.business)), data=unname(unlist(tbl.business)))
  
  sortProfileBy <- fromJSON('[{"sortBy":15,"kActive":1,"kScore":0},{"sortBy":5,"kActive":1,"kScore":2},{"sortBy":1,"kActive":1,"kScore":5},{"sortBy":9,"kActive":1,"kScore":10},{"sortBy":6,"kActive":1,"kScore":2},{"sortBy":2,"kActive":1,"kScore":5},{"sortBy":16,"kActive":1,"kScore":10},{"sortBy":17,"kActive":1,"kScore":2},{"sortBy":7,"kActive":1,"kScore":2},{"sortBy":11,"kActive":1,"kScore":2},{"sortBy":20,"kActive":1,"kScore":0},{"sortBy":10,"kActive":1,"kScore":2},{"sortBy":4,"kActive":1,"kScore":0},{"sortBy":18,"kActive":1,"kScore":0},{"sortBy":13,"kActive":1,"kScore":0},{"sortBy":3,"kActive":1,"kScore":10},{"sortBy":14,"kActive":1,"kScore":2},{"sortBy":19,"kActive":1,"kScore":0},{"sortBy":12,"kActive":0,"kScore":0},{"sortBy":21,"kActive":0,"kScore":0},{"sortBy":8,"kActive":1,"kScore":5}]')
  #sortProfileBy <- paste_from_clipboard(sep='\t', header = T)
  #toJSON(sortProfileBy)
  
  ## add profile sort order to tbl and sort table with arrange(tbl.profile,sortBy) and drop sortBy
  tbl.business <- bind_cols(sortProfileBy,tbl.business)
  tbl.business <- tbl.business %>% arrange(sortBy) %>% select(-sortBy)
  
  ## add numericInputs and checkboxes to table
  ## put tags on photos
  
  tbl.business <- tbl.business %>% rowwise() %>% mutate(kScore = paste0(tags$div(numericInput(paste0("kNum_",profile),label="",value = kScore, width=75),style='float:center-block')),
                                                        kActive = paste0(tags$div(br(),checkboxInput(paste0("kBox_",profile), label="", value = as.logical(as.integer(kActive)), width = 30),style='float:right')))
  
  
  
  # kScore=paste0(paste0(tags$div(numericInput(paste0("kNumB_",profile),label="",value = kScore, width=25),style="float:left"))),
  #                                                     kActive= paste0(tags$div(br(),checkboxInput(paste0("kBoxB_",profile), label="", value = kActive, width = 25), style = "float:left"))))
  # 
  cat('Lets Print tbl.business ->','\n',sep = '')
  print(tbl.business)
  return(tbl.business)
  
  
}




get.V2UsersList <- function(users) { 
  ### Returns the search list by userId and name
  require(httr)
  require(dplyr)
  require(jsonlite)
  ##### SWAGGER for project calls from api.kountable.com
  cat("---> Called get.V2UsersList","\n",sep = "")
  
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         "k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  cat("---> BearerToken: ", accessToken$data$access_token,"\n",sep = "")
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/users'),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  ## create the search field
  tbl.allUsers <- tbl_df(getV2Swagger$data)
  #write.table(tbl.allUsers, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)
  tbl.allUsers <- tbl.allUsers %>% rowwise() %>% 
    mutate(searchName=paste0(id, ' - ',first_name,', ',last_name))
  # mutate(searchName=paste0(id, ' - ',first_name,', ',last_name,'; ', country))
  ## arrange in descending order
  tbl.allUsers <- dplyr::arrange(tbl.allUsers,desc(as.integer(tbl.allUsers$id)))
  
  ## create the id vectorl and add the search names
  user_id <- tbl.allUsers$id
  names(user_id) <- tbl.allUsers$searchName
  return(user_id)
}


get.V2AllProjectsList <- function(){ 
  ##### SWAGGER for project calls from api.kountable.com
  require(httr)
  require(jsonlite)
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         "k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  cat("--> get.V2ProjectsList","\n")
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/projects?'),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  return(getV2Swagger$data)
}




get.V2business_types <- function() { 
  ## get the V2 business types dictionary http://api.kountable.com/api/v2/dictionary/business_types
  require(httr)
  require(dplyr)
  ##### SWAGGER for project calls from api.kountable.com
  ## need jo@kountable.com with admin rights
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         "k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/dictionary/business_types'),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  return(getV2Swagger)
}

get.V2BusinessesList <- function(uuid) { 
  ### Returns the search list by userId and name
  require(httr)
  require(dplyr)
  ##### SWAGGER for project calls from api.kountable.com
  
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         "k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/businesses?ownedby=',uuid),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  return(getV2Swagger)
}

get.V2ProjectsList <- function(uuid){ 
  ##### SWAGGER for project calls from api.kountable.com
  require(httr)
  require(jsonlite)
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         "k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  cat("--> get.V2ProjectsList for ",uuid,"\n")
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/projects?ownedby=',uuid),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  return(getV2Swagger$data)
}




# testProjectsCall <- get.V2ProjectsList('504')
# testProjectsCall$data$payors
# testProjectsCall$data$suppliers
# testProjectsCall$data$id

get.frankBusinessData2 <- function(userProjects, business_types, countries) {
  ## use frank_business data.frame to get unique buisness
  
  require(dplyr)
  cat('call-> get.frankBusinessData -> any data? length should be 25 Actual Length = ', length(userProjects$data$frank_business), '\n', sep = "  ")
  if(is.null(userProjects$data$frank_business)){return(NULL)}
  print(userProjects$data$frank_business)
  ## make it a dyplr table
  ## keep on the first record, the first record appears to be FrankBusiness
  ## assume one unique business for now,
  tbl.business <- tbl_df(unique(userProjects$data$frank_business))
  tbl.business <- tbl.business[1,]
  
  ## add in countries and business type_name
  tbl.business$country <- countries$name[tbl.business$country_id]
  if(tbl.business$legal_registered_country_id > 0){
    tbl.business$legal_registered_country <- countries$name[tbl.business$legal_registered_country_id]
  } else {
    tbl.business$legal_registered_country <- ''
  }
  
  tbl.business$type_name <- business_types$data$name[tbl.business$type_id]
  tbl.business <- data.frame(profile=names(unlist(tbl.business)), data=unname(unlist(tbl.business)), stringsAsFactors = FALSE)
  
  
  
  ## sortProfileBy is made in a numbers spreadsheet
  sortProfileBy <- fromJSON('[{"sortBy":1,"kActive":1,"kScore":0,"profile":"id"},{"sortBy":17,"kActive":1,"kScore":0,"profile":"created_at"},{"sortBy":18,"kActive":1,"kScore":0,"profile":"updated_at"},{"sortBy":19,"kActive":1,"kScore":0,"profile":"created_by"},{"sortBy":2,"kActive":1,"kScore":2,"profile":"name"},{"sortBy":3,"kActive":1,"kScore":2,"profile":"description"},{"sortBy":14,"kActive":1,"kScore":1,"profile":"contact"},{"sortBy":6,"kActive":1,"kScore":2,"profile":"phone"},{"sortBy":16,"kActive":1,"kScore":10,"profile":"registration_num"},{"sortBy":20,"kActive":1,"kScore":0,"profile":"supplier_score"},{"sortBy":21,"kActive":1,"kScore":0,"profile":"payor_score"},{"sortBy":22,"kActive":1,"kScore":0,"profile":"legal_registered_country_id"},{"sortBy":10,"kActive":1,"kScore":2,"profile":"city"},{"sortBy":11,"kActive":1,"kScore":2,"profile":"state"},{"sortBy":26,"kActive":1,"kScore":0,"profile":"country_id"},{"sortBy":12,"kActive":1,"kScore":2,"profile":"postal_code"},{"sortBy":8,"kActive":1,"kScore":2,"profile":"address"},{"sortBy":24,"kActive":1,"kScore":0,"profile":"enabled"},{"sortBy":25,"kActive":1,"kScore":0,"profile":"type_id"},{"sortBy":27,"kActive":1,"kScore":0,"profile":"organization_id"},{"sortBy":23,"kActive":1,"kScore":0,"profile":"owned_by"},{"sortBy":7,"kActive":1,"kScore":5,"profile":"website"},{"sortBy":15,"kActive":1,"kScore":2,"profile":"incorporation_date"},{"sortBy":9,"kActive":1,"kScore":0,"profile":"address2"},{"sortBy":13,"kActive":1,"kScore":2,"profile":"country"},{"sortBy":5,"kActive":1,"kScore":2,"profile":"legal_registered_country"},{"sortBy":4,"kActive":1,"kScore":5,"profile":"type_name"}]')
  # write.table(tbl.business, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)
  # sortProfileBy <- paste_from_clipboard(sep='\t', header = T)
  # toJSON(sortProfileBy)
  
  ## add profile sort order to tbl and sort table with arrange(tbl.profile,sortBy) and drop sortBy
  # tbl.business <- bind_cols(sortProfileBy,tbl.business)
  # tbl.business <- tbl.business %>% arrange(sortBy) %>% select(-sortBy)
  # # View(tbl.business)
  ## Here is the kscore table version 2 which is the version with key and using dplyr to merge with 
  ## whatever fields are in the current call.  We are attempting to be fluid by scoring only fields we decide before hand
  ## sortPrwhofileBy is made in a numbers spreadsheet
  # sortProfileBy <- fromJSON('[{"sortBy":13,"kActive":0,"kScore":0,"key":""},{"sortBy":6,"kActive":1,"kScore":2,"key":"age_range"},{"sortBy":1,"kActive":1,"kScore":5,"key":"avatar_link"},{"sortBy":10,"kActive":0,"kScore":0,"key":"email"},{"sortBy":12,"kActive":0,"kScore":0,"key":"facebook_id"},{"sortBy":3,"kActive":1,"kScore":2,"key":"first_name"},{"sortBy":2,"kActive":1,"kScore":2,"key":"friends_number"},{"sortBy":5,"kActive":1,"kScore":2,"key":"full_name"},{"sortBy":7,"kActive":1,"kScore":1,"key":"gender"},{"sortBy":4,"kActive":1,"kScore":2,"key":"last_name"},{"sortBy":8,"kActive":1,"kScore":1,"key":"locale"},{"sortBy":11,"kActive":1,"kScore":10,"key":"profile_link"},{"sortBy":9,"kActive":1,"kScore":1,"key":"timezone"}]')
  # write.table(tbl.business, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)
  # sortProfileBy <- paste_from_clipboard(sep='\t', header = T)
  # toJSON(sortProfileBy)
  
  ## full join by Key
  ## arrange by sortBy field all missing should go to the bottom then remove sortBy column
  
  tbl.business <- dplyr::full_join( tbl.business, sortProfileBy, by = "profile")
  tbl.business <- tbl.business %>% arrange(sortBy) %>% select(-sortBy)
  
  
  ## calculate Kscore Red by checking for NAN, NA, blank in data field
  naData <- -as.integer(is.na(tbl.business$data)) 
  nanData <- -as.integer(is.nan(tbl.business$data))
  blankData <- -as.integer(nchar(tbl.business$data) < 1)
  ## add the three vectors then add them to the kActive Vector
  emptyData <- naData + nanData + blankData
  tbl.business$missingData <- tbl.business$kActive + emptyData
  
  
  ## make sure NAs are 0 so all calculations work
  tbl.business$missingData[which(is.na(tbl.business$missingData))] <- 0
  
  ## multiply by Kscore
  tbl.business$FrankBusinessKScore <- tbl.business$missingData * tbl.business$kScore
  
  ## add numericInputs and checkboxes to table
  ## put tags on photos
  
  # tbl.business <- tbl.business %>% rowwise() %>% mutate(kScore = paste0(tags$div(numericInput(paste0("kNum_",profile),label="",value = kScore, width=75),style='float:center-block')),
  #                                                       kActive = paste0(tags$div(br(),checkboxInput(paste0("kBox_",profile), label="", value = as.logical(as.integer(missingData)), width = 30),style='float:right')))
  # 
  ## now lets get rid of checkboxes and numericInputs for 1, 15-25 rows
  # tbl.business[ c(1,15:25),c('kActive','kScore')] <- ''
  ## now we'll remove the extra rows 15:25
  # tbl.business <- tbl.business[ c(1:14),]
  
  
  # kScore=paste0(paste0(tags$div(numericInput(paste0("kNumB_",profile),label="",value = kScore, width=25),style="float:left"))),
  #                                                     kActive= paste0(tags$div(br(),checkboxInput(paste0("kBoxB_",profile), label="", value = kActive, width = 25), style = "float:left"))))
  # 
  cat('Lets Print tbl.business ->','\n',sep = '')
  print(tbl.business)
  return(tbl.business)
  
}
#userProjects <- get.V2ProjectsList('10198')
#get.frankBusinessData2(userProjects, business_types, countries)

get.frankBusinessDataALL <- function(userProjects, business_types, countries) {
  ## use frank_business data.frame to get unique buisness
  
  require(dplyr)
  cat('call-> get.frankBusinessData -> any data? length should be 25 Actual Length = ', length(userProjects$data$frank_business), '\n', sep = "  ")
  if(is.null(userProjects$data$frank_business)){
    return(NULL)
  } else {
    print(userProjects$data$frank_business)
    ## make it a dyplr table
    ## keep on the first record, the first record appears to be FrankBusiness
    ## assume one unique business for now,
    tbl.business <- tbl_df(unique(userProjects$data$frank_business))
    cat('There are ', nrow(tbl.business), ' frank businesses')
    
    ## add in countries and business type_name
    countries <- tbl_df(countries$data)
    business_types <- tbl_df(business_types$data)
    ## create country field from legal registered country id
    tbl.business <- tbl.business %>% 
      rowwise() %>% 
      mutate(country = ifelse(legal_registered_country_id > 0, countries$name[legal_registered_country_id], 'Unknown'),
             type_name = ifelse(type_id > 0, business_types$name[type_id], 'Unknown'))
    
    
    
    ## make key businessId columns of data
    require(tidyr)
    tbl.business <- tbl.business %>% group_by(id) %>% tidyr::gather("key","value",2:ncol(tbl.business)) %>% tidyr::spread(id,value)
    
    
    cat('Lets Print tbl.businessall ->','\n',sep = '')
    print(tbl.business)
    return(tbl.business)
    
  }
  
}
#userProjects <- get.V2ProjectsList('10198')
# tbl.businessAll <- get.frankBusinessDataALL(userProjects, business_types, countries)


get.SocialMediaDataKscore <- function(uuidApiKYC1) {
  
  providerNameList <- c("Facebook","Twitter","Instagram","LinkedIn","Yahoo","Google+","Gmail")
  providerKScoreList <- c(8,6,4,10,5,5,0)
  
  
  socialMediaAuthenticationKscore <- sum(providerKScoreList[which(providerNameList %in% uuidApiKYC1$identities$provider$name)])
  return(socialMediaAuthenticationKscore)
}


get.PhoneAppKscore <- function(uuidApiKYC1) {
  
  phoneAppList <- c("iOS mobile app","Android mobile app")
  phoneAppKscoreList <- c(50,50)
  
  
  phoneAppKscore <- phoneAppKscoreList[which(phoneAppList %in% uuidApiKYC1$identities$provider$name)]
  ifelse(length(phoneAppKscore)==2, phoneAppKscore <-sum(phoneAppKscore)/2, phoneAppKscore <- sum(phoneAppKscore))
  
  return(phoneAppKscore)
}

get.userDocumentsTable <- function(projectsList){
  require(dplyr)
  ## KEEP only the non rejected projects
  projectsList$data <- projectsList$data[which(!projectsList$data$state$code=="REJECTED"),]
  
  projectDocumentsAllS <- sapply(projectsList$data$id, get.V2UsersProjectDocuments)
  
  
  ## keep the ones we want to display
  keyDocuments <- c(2,12,13,14,15,16,22)
  # which(projectDocumentsAll[[1]]$data$type_id %in% keyDocuments)
  
  ## keep only dataframes for this next part
  projectDocumentsAllS <- projectDocumentsAllS[sapply(projectDocumentsAllS, function(x){is.data.frame(x)})]
  ## which have the key documents
  projectDocumentsAllS <- lapply(projectDocumentsAllS, function(x){x[which(x$type_id %in% keyDocuments),]})
  ## get rid of 0 row data.frames
  projectDocumentsAllS <- projectDocumentsAllS[sapply(projectDocumentsAllS, function(x){nrow(x)>0})]
  
  ## make the massive talbe and combine
  projectDocumentsAllS <- bind_rows(lapply(projectDocumentsAllS, function(x){get.ProjectsDocumentsTbl( x)}))
  return(projectDocumentsAllS)
}

get.userDocumentsTableAll <- function(projectsList){
  require(dplyr)
  ## KEEP only the non rejected projects
  # projectsList$data <- projectsList$data[which(!projectsList$data$state$code=="REJECTED"),]
  
  projectDocumentsAllS <- sapply(projectsList$data$id, get.V2UsersProjectDocuments)
  
  ## keep the ones we want to display
  keyDocuments <- c(2,12,13,14,15,16,22)
  # which(projectDocumentsAll[[1]]$data$type_id %in% keyDocuments)
  
  ## keep only dataframes for this next part
  projectDocumentsAllS <- projectDocumentsAllS[sapply(projectDocumentsAllS, function(x){is.data.frame(x)})]
  ## which have the key documents
  projectDocumentsAllS <- lapply(projectDocumentsAllS, function(x){x[which(x$type_id %in% keyDocuments),]})
  ## get rid of 0 row data.frames
  projectDocumentsAllS <- projectDocumentsAllS[sapply(projectDocumentsAllS, function(x){nrow(x)>0})]
  
  ## make the massive talbe and combine
  projectDocumentsAllS <- bind_rows(lapply(projectDocumentsAllS, function(x){get.ProjectsDocumentsTbl( x)}))
  return(projectDocumentsAllS)
}
# projectsList <- get.V2ProjectsList('49')
get.userDocumentsTableAllEntered <- function(projectsList){
  require(dplyr)
  ## KEEP only the non rejected projects
  # projectsList$data <- projectsList$data[which(!projectsList$data$state$code=="REJECTED"),]
  
  projectDocumentsAllS <- sapply(projectsList$data$id, get.V2UsersProjectDocuments)
  cat("projectDocumentAllS length",length(projectDocumentsAllS),'\n')
  # print(str(projectDocumentsAllS))
  if (length(projectDocumentsAllS)==0) {
    return(NULL)
  } else {## keep the ones we want to display
    ## keyDocuments <- c(2,12,13,14,15,16,22)
    # which(projectDocumentsAll[[1]]$data$type_id %in% keyDocuments)
    
    ## keep only dataframes for this next part
    projectDocumentsAllS <- projectDocumentsAllS[sapply(projectDocumentsAllS, function(x){is.data.frame(x)})]
    ## which have the key documents
    # projectDocumentsAllS <- lapply(projectDocumentsAllS, function(x){x[which(x$type_id %in% keyDocuments),]})
    ## get rid of 0 row data.frames
    projectDocumentsAllS <- projectDocumentsAllS[sapply(projectDocumentsAllS, function(x){nrow(x)>0})]
    
    ## make the massive talbe and combine
    projectDocumentsAllS <- bind_rows(lapply(projectDocumentsAllS, function(x){get.ProjectsDocumentsTbl( x)}))
    return(projectDocumentsAllS)}
  
}

get.V2UsersProjectDocuments <- function(apiProjectId){ 
  ##### SWAGGER for project calls from api.kountable.com
  require(httr)
  require(jsonlite)
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         "k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  cat("Project Id: ", apiProjectId," --> get.V2UsersProjectDocuemtns","\n")
  getV2Swagger <- GET(paste('http://api.kountable.com/api/v2/projects/',apiProjectId,'/documents', sep = ""),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  return(getV2Swagger)
}

# projectDocuments <- get.V2UsersProjectDocuments('335')
# str(projectDocuments)
# projectDocuments$data$related_entity_id
# projectDocuments <- projectDocumentsAllS[[1]]
# projectDocuments <- projectDocumentsAllS[[2]]
# projectDocuments <- projectDocumentsAllS[[3]]
get.ProjectsDocumentsTbl <- function(projectDocuments){
  require(dplyr)
  require(shiny)
  cat('Called get.kycDocumentsTbl','\n', sep = '')
  # print(projectDocuments)
  ## get the projectId from releated_entity_id
  project_id <- unique(projectDocuments$related_entity_id)
  cat('documents for project_id: ', project_id, '\n')
  
  if(is.null(projectDocuments$items)){
    return(NULL)
  } else{
    ## select all columns but dataframes items and dataframe type
    kycDocs <- tbl_df(projectDocuments %>% select(-items,-type, -organization))
    # colnames(kycDocuments) <- paste0("documents.",colnames(kycDocuments))
    
    
    
    ## select all columns but dataframes file 
    ## then bind_rows to make table sapply gets rid of the list of 1 in between !!!!!!
    kycDocFiles <- bind_rows(sapply(projectDocuments$items, dplyr::select, file))
    
    ## add file. to filenames and order table by list
    colnames(kycDocFiles) <- paste0('file.',names(kycDocFiles))
    kycDocFiles <- kycDocFiles[ c('file.id','file.name','file.description','file.mime_type','file.lat','file.lng','file.url','file.timestamp')]
    
    ## use mutate to add the hyperlink and image tags to file.url
    # kycDocFiles <- kycDocFiles %>% rowwise() %>% mutate(file.url=paste0(tags$a(tags$img(src = file.url, height=300, width=300),href = file.url,target='_blank')))
    kycDocFiles <- kycDocFiles %>% rowwise() %>% mutate(file.url=paste0(tags$a(tags$img(src = file.url),href = file.url,target='_blank')))
    
    ## for the non subsets coluns, documentId, id, sortOrd uses lapply, bind_rows will adapt and combine them all
    kycDocItems <- bind_rows(lapply(projectDocuments$items, dplyr::select, document_id,id, sort_ord ))
    colnames(kycDocItems) <- paste0('items.',names(kycDocItems))
    
    ## for the non subsets coluns, documentId, id, sortOrd uses lapply, bind_rows will adapt and combine them all
    kycDocType <- tbl_df(projectDocuments$type)
    colnames(kycDocType) <- paste0('type.',names(kycDocType))
    
    ## ok if you have multipage documents, use table on documentId to identify their frequencies
    ## use rep by their freq convert to integer and use to repeat rownames
    ## to repeate rows in a dataframe accordingly
    multiPageDocs <- as.integer(rep(rownames(kycDocs),table(kycDocItems$items.document_id)))
    
    
    
    ## combine them all together and maker urls images with hyperlinks
    ## use multiPageDocs in case their are multPage file uploads in file on KYC Documents and kyc Doc Type
    tbl.kycDocuments <- data.frame(kycDocs[multiPageDocs,],kycDocItems,kycDocType[multiPageDocs,],kycDocFiles)
    ## combine them all together and maker urls images with hyperlinks
    # tbl.kycDocuments <- data.frame(kycDocs,kycDocItems,kycDocType,kycDocFiles)
    # tbl.kycDocuments1 <- tbl.kycDocuments[c(5,6,7,20,1,28,27,26,23,24,25,22,2,4,1)]
    ## final step add in project_id
    tbl.kycDocuments$project_id <- project_id
    tbl.kycDocuments <- tbl.kycDocuments[c("project_id","name","description", "file.url", "file.id", "file.name", "file.description","created_at","created_by","type.name","id", "file.lat","file.lng")]
    cat('made it to the end of kycDocumentsTbl with length tbl.kycDocuments = ',length(tbl.kycDocuments),'\n', sep = '')
    # print(tbl.kycDocuments)
    
    return(tbl.kycDocuments)
  }
  
  
  
}
# uuidApiKYC1 <- get.apiKYCCall('10730')
# apiUUID <- '10730'
get.userLoginList <- function(apiUUID, uuidApiKYC1) {
  require(dplyr)
  require(lubridate)
  cat(apiUUID, "--> Calling get.userLoginList",'\n')
  
  ## data entry estimated times in minutes
  ## data frame used for login list/time table display in min
  ls.dataEntryTime <- list(loginTime = dminutes(2), 
                           documentUploadTime = dminutes(3), 
                           createUserTime = dminutes(5), 
                           createBusinessTime = dminutes(3), 
                           creatProjectTime = dminutes(5), 
                           installPhoneAppTime = dminutes(10), 
                           authenticateSocialMediaTime = dminutes(3)
  )
  
  # uuidApiKYC1 <- get.apiKYCCall(apiUUID)
  ## its in T Z format remove the T and Z make timezone UTC
  userCreatedDate <- as.POSIXct( gsub('[A-Z]',' ',uuidApiKYC1$profile$createdAt),tz = 'UTC')
  
  ##dates of events in Frank Data Entry
  ## do it by table?  user, project, business, social media, documents?
  ## When did they download the app?
  ## create action column with event action Installed phone app or authenticated with social media
  tbl.userIdentity <- get.userIdentitiesTable(apiUUID)
  cat(apiUUID, '--> inside get.UserLoginList before tbl.userIdentity check', '\n')
  if(nrow(tbl.userIdentity)!=0) {
    cat(nrow(tbl.userIdentity), '--> nrow of tbl.userIdentity', '\n')
    
    tbl.userIdentity <- tbl.userIdentity %>% 
      rowwise() %>% 
      mutate(created_at = as.POSIXct( gsub('[A-Z]',' ', created_at),tz = 'UTC'),
             entered_by = ifelse(provider_name %in% c('Android mobile app','iOS mobile app'), 
                                 paste0('Frank'), paste0('Frank')),
             entered_by_userId = id,
             action = ifelse(provider_name %in% c('Android mobile app','iOS mobile app'), 
                             paste0('Installed ', provider_name), paste0('Authenticated with ', provider_name)),
             location = c(''),
             dataEntryTime = ifelse(provider_name %in% c('Android mobile app','iOS mobile app'), 
                                    ls.dataEntryTime$installPhoneAppTime, ls.dataEntryTime$authenticateSocialMediaTime)) %>% 
      arrange(created_at)
    
    ## get user login location data
    tbl.user_login_location <- get.userLoginLocatationData(apiUUID)
    if(!is.null(tbl.user_login_location)){
      tbl.user_login_location <- tbl.user_login_location %>% rowwise() %>% 
        mutate(created_at = as.POSIXct( gsub('[A-Z]',' ', created_at),tz = 'UTC'),
               entered_by = c('Frank'),
               entered_by_userId = user_id,
               action =paste0(device, " login"),
               location = paste(latitude, longitude, country_code, country, hostname, sep = ', '),
               dataEntryTime = ls.dataEntryTime$loginTime) %>% 
        arrange(created_at)
      
    }
    
    
    ## projects
    tbl.userProjectsData <- get.V2ProjectsList(apiUUID)
    if(length(tbl.userProjectsData$data)!=0) {
      
      tbl.userProjectsList <- tbl.userProjectsData$data
      ## if no projects kill documents
      tbl.userProjectsList <- tbl.userProjectsList %>% rowwise() %>% 
        mutate(created_at = as.POSIXct( gsub('[A-Z]',' ', created_at),tz = 'UTC'),
               entered_by = ifelse(created_by == apiUUID, paste0('Frank'), paste0('Molly')),
               entered_by_userId = created_by,
               action = paste0("Created project ", id, ' - ', ifelse(name =='', "No Project Name", name)),
               location = c('')) %>% 
        arrange(created_at)
      tbl.userProjectsList$dataEntryTime <- ls.dataEntryTime$creatProjectTime
      
      ## documents
      tbl.userProjectDocuments <- get.userDocumentsTableAllEntered(projectsList = tbl.userProjectsData)
      
      if (nrow(tbl.userProjectDocuments) > 0) {
        tbl.userProjectDocuments <- tbl.userProjectDocuments %>% rowwise() %>% 
          mutate(created_at = as.POSIXct( gsub('[A-Z]',' ', created_at),tz = 'UTC'),
                 entered_by = ifelse(created_by == apiUUID, paste0('Frank'), paste0('Molly')),
                 entered_by_userId = created_by,
                 action = paste0("Document uploaded ", id, '-', type.name, ' ', name),
                 location = paste(file.lat, file.lng, sep = ', ')) %>% 
          arrange(created_at)
        tbl.userProjectDocuments$dataEntryTime <- ls.dataEntryTime$documentUploadTime
      }
    } else {
      tbl.userProjectDocuments <- NULL
      
      
    }
    
    
    
    ## get dates of businesses created
    tbl.userBusinessList <- get.V2BusinessesList(apiUUID)
    if(length(tbl.userBusinessList$data)!=0){
      ## identify role payors, suppliers, frank business
      ## get unique list of suppliers and payors
      frankPayors <- unique(unlist(tbl.userProjectsList$payors))
      frankSuppliers <- unique(unlist(tbl.userProjectsList$suppliers))
      frankBusinesses <- unique(unlist(tbl.userProjectsList$frank_business$id))
      
      ## since a business can be both a supplier and a payor, we need to make sure that we get multiple records if this occurs
      ## if any are null make a table with -99999 id so no matches
      ifelse(is.null(frankPayors), tbl.frankBusinessPayors <- data_frame(id = -99999, frankBusinessRole = 'payor'), 
             tbl.frankBusinessPayors <- data_frame(id =  frankPayors, frankBusinessRole = rep('payor',length(frankPayors))))
      ifelse(is.null(frankSuppliers), tbl.frankBusinessSuppliers <- data_frame(id = -99999, frankBusinessRole = 'supplier'), 
             tbl.frankBusinessSuppliers <- data_frame(id =  frankSuppliers, frankBusinessRole = rep('supplier',length(frankSuppliers))))
      ifelse(is.null(frankBusinesses), tbl.frankBusinesses <- data_frame(id =  c('-99999'), frankBusinessRole = 'frank business'), 
             tbl.frankBusinesses <- data_frame(id =  frankBusinesses, frankBusinessRole = rep('frank business',length(frankBusinesses))))
      
      
      
      ## build a table of frank business Roles of ids and roles
      ## make the construction conditional on whether these exist
      
      tbl.frankBusinessRole <- bind_rows(tbl.frankBusinessPayors,
                                         tbl.frankBusinessSuppliers,
                                         tbl.frankBusinesses)
      
      ##convert to dplyr tbl
      tbl.frankBusinessData <- tbl_df(tbl.userBusinessList$data)
      ## combine business list with business role - if the role exists for that business
      tbl.frankBusinessData <- left_join(tbl.frankBusinessData,tbl.frankBusinessRole, by = 'id')
      ## make N/A roles, unassigned business and create action field of Entered business
      tbl.frankBusinessData <-  tbl.frankBusinessData %>% 
        rowwise() %>%  
        mutate(created_at = as.POSIXct( gsub('[A-Z]',' ', created_at),tz = 'UTC'),
               frankBusinessRole = ifelse(is.na(frankBusinessRole),c('unassigned business'), frankBusinessRole),
               entered_by = ifelse(created_by == apiUUID, paste0('Frank'), paste0('Molly')),
               entered_by_userId = created_by,
               action = paste0("Entered ", frankBusinessRole, ', ', id,' - ', name)
        )
      ## Add data entry time
      tbl.frankBusinessData$dataEntryTime <- ls.dataEntryTime$createBusinessTime 
      
    }
    ## combine with bind rows to create the login action history list
    
    tbl.loginListCore <- bind_rows(data_frame(loginDate = userCreatedDate, 
                                              entered_by = c('Frank'),
                                              entered_by_userId = as.integer(uuidApiKYC1$uuid),
                                              action = c('Created User Account'), 
                                              location = c(''), 
                                              dataEntryTime = ls.dataEntryTime$createUserTime), 
                                   data_frame(loginDate = tbl.userIdentity$created_at, 
                                              entered_by = tbl.userIdentity$entered_by, 
                                              entered_by_userId = tbl.userIdentity$entered_by_userId,
                                              action = tbl.userIdentity$action, 
                                              location = c(''),
                                              dataEntryTime = tbl.userIdentity$dataEntryTime)
    )
    
    if(!is.null(tbl.user_login_location)){
      tbl.loginListCore <- bind_rows(tbl.loginListCore, 
                                     data_frame(loginDate = tbl.user_login_location$created_at, 
                                                entered_by = tbl.user_login_location$entered_by, 
                                                entered_by_userId = tbl.user_login_location$entered_by_userId,
                                                action = tbl.user_login_location$action, 
                                                location = tbl.user_login_location$location,
                                                dataEntryTime =  tbl.user_login_location$dataEntryTime)
      )
    }
    
    if(length(tbl.userBusinessList$data)!=0) {
      tbl.loginListCore <- bind_rows(tbl.loginListCore, 
                                     data_frame(loginDate = tbl.frankBusinessData$created_at, 
                                                entered_by = tbl.frankBusinessData$entered_by, 
                                                entered_by_userId = tbl.frankBusinessData$entered_by_userId,
                                                action = tbl.frankBusinessData$action, 
                                                location = c(''), 
                                                dataEntryTime = tbl.frankBusinessData$dataEntryTime)
      )
    }
    if(length(tbl.userProjectsData$data)!=0) {
      tbl.loginListCore <- bind_rows(tbl.loginListCore, 
                                     data_frame(loginDate = tbl.userProjectsList$created_at, 
                                                entered_by = tbl.userProjectsList$entered_by, 
                                                entered_by_userId = tbl.userProjectsList$entered_by_userId,
                                                action = tbl.userProjectsList$action, 
                                                location = c(''), 
                                                dataEntryTime = tbl.userProjectsList$dataEntryTime)
      )
    }                           
    if(nrow(tbl.userProjectDocuments) > 0) {
      tbl.loginListCore <- bind_rows(tbl.loginListCore, 
                                     data_frame(loginDate = tbl.userProjectDocuments$created_at, 
                                                entered_by = tbl.userProjectDocuments$entered_by, 
                                                entered_by_userId = tbl.userProjectDocuments$entered_by_userId,
                                                action = tbl.userProjectDocuments$action, 
                                                location = tbl.userProjectDocuments$location, 
                                                dataEntryTime = tbl.userProjectDocuments$dataEntryTime)
      )
    }                           
    
    
    
    ## arrange by loginDate default is ascending
    tbl.loginList <- tbl.loginListCore %>% arrange(loginDate)
    tbl.loginList$dataEntryTime <- dseconds(tbl.loginList$dataEntryTime)
    
    ## add App Version 
    ## Version 1.0.4 Oct 8, 2015
    ## Version 1.1.8 Jan 29, 2016
    ## Version 1.2.2 Mar 25, 2016 - jCI
    ## Version 2.0.20 Jun 23, 2016 - Easy Frank
    ## Version 2.0.24 Jul 13, 2016 - Easy Frank Gmail
    tbl.loginList <- tbl.loginList %>% 
      rowwise() %>% 
      mutate(appVersion = ifelse(loginDate < as.POSIXct("2016-01-29 00:00:00", tz="UTC"),c('Version 1.0.4'),
                                 ifelse(loginDate < as.POSIXct("2016-03-25 00:00:00", tz="UTC"),c('Version 1.1.8'),
                                        ifelse(loginDate < as.POSIXct("2016-06-23 00:00:00", tz="UTC"),c('Version 1.2.2(JCI)'),
                                               ifelse(loginDate < as.POSIXct("2016-07-13 00:00:00", tz="UTC"),c('Version 2.0.20(Easy Frank)'),
                                                      ifelse(loginDate < as.POSIXct("2016-08-24 00:00:00", tz="UTC"),c('Version 2.0.24(Easy Frank GMail)'),
                                                             ifelse(loginDate < as.POSIXct("2016-09-01 00:00:00", tz="UTC"),c('Version 2.0.26(Easy Frank Push)'),
                                                                    ifelse(loginDate < as.POSIXct("2016-09-20 00:00:00", tz="UTC"),c('Version 2.0.27(Easy Frank Project Messaging)'),
                                                                           c('Version 2.0.28(Easy Frank Country Integration')))))))))
    ## add frank Country
    tbl.loginList$frankCountry <- uuidApiKYC1$profile$country
    ## order columns
    tbl.loginList <-tbl.loginList[,c(1,2,4,6,7,3,5,8)]
    cat('Print tbl.loginList','\n')
    print(tbl.loginList, '\n')
    return(tbl.loginList)
    
  } else{
    return(NULL)  
  }
  
}

get.userLoginLocatationData <- function(apiUUID) {
  require(dplyr)
  require(httr)
  ## load to manual tables
  # tbl.user_login_locations <- tbl_df(readRDS("/Users/doucetteemail/Documents/KountableFiles/RDevelopment/RMarker/RMarker/data/user_login_location.rds"))
  tbl.user_login_locations <- get.V3UserLoginLocation(apiUUID)
  
  if (nrow(tbl.user_login_locations) == 0) {
    return(NULL)
  } else {
    # tbl.countries <- tbl_df(readRDS("/Users/doucetteemail/Documents/KountableFiles/RDevelopment/RMarker/RMarker/data/country.rds"))
    tbl.countries <- tbl_df(get.V3CountriesDictionary()$data)
    ## get list of providers
    tbl.providers <- tbl_df(get.V2User_Identities_ProvidersDictionary())
    ## clean up countries and rename fields for merge, then merge providers for completed named table
    tbl.countries <- tbl.countries %>% select(country_id = id, country_code = code, country_code2 = code2,country_name = name)
    tbl.user_login_location <- dplyr::left_join(tbl.user_login_locations, tbl.countries, by = c("country_id" = "country_id"))
    tbl.user_login_location <- dplyr::left_join(tbl.user_login_location, tbl.providers, by = c("provider_id" = "provider_id"))
    
    ## now select only the user_id data
    # tbl.user_login_location <- tbl.user_login_location %>% filter(user_id == apiUUID)
    
    return(tbl.user_login_location)
    
  }
  
}

get.userIdentitiesTable <- function(apiUUID) {
  require(dplyr)
  require(httr)
  ## call userIdentities
  tbl.user_identitys <- tbl_df(get.V2UsersIdentities(apiUUID))
  # tbl.countries <- tbl_df(readRDS("/Users/doucetteemail/Documents/KountableFiles/RDevelopment/RMarker/RMarker/data/country.rds"))
  tbl.countries <- tbl_df(get.V3CountriesDictionary()$data)
  ## clean up countries and rename fields for merge, then merge providers for completed named table
  tbl.countries <- tbl.countries %>% select(country_id = id, country_code = code, country_code2 = code2,country_name = name)
  tbl.user_identity <- dplyr::left_join(tbl.user_identitys, tbl.countries, by = c("country_id" = "country_id"))
  
  
  return(tbl.user_identity)
  
}

get.V2User_Identities_ProvidersDictionary <- function(){
  ### Returns the search list by userId and name
  require(httr)
  require(dplyr)
  ##### SWAGGER for project calls from api.kountable.com
  
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         #"k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/dictionary/user_identity_providers'),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        #"k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  getV2Swagger <-  tbl_df(getV2Swagger$data)
  getV2Swagger <- getV2Swagger %>% select(provider_id = id, provider_code = code, provider_name = name, provider_description = description)
  return(getV2Swagger)
}




# curl -X GET -H "Authorization: Bearer yJ0sXjbUhhrJi1TBy0mdWhphxowZ7vhBoDZHwcD1HGs=" -H 
# "Content-Type: application/json" -H "k-app-type: web" -H "k-app-version: 1.0.0" -H 
# "Cache-Control: no-cache" "http://api.kountable.com/api/v3/users/10001/user_login_location"

get.V3UserLoginLocation <- function(apiUUID){
  ### Returns the search list by userId and name
  require(httr)
  require(dplyr)
  ##### SWAGGER for project calls from api.kountable.com
  
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         #"k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v3/users/',apiUUID,'/user_login_location'),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  getV2Swagger <-  tbl_df(getV2Swagger$data)
  # getV2Swagger <- getV2Swagger %>% select(provider_id = id, provider_code = code, provider_name = name, provider_description = description)
  return(getV2Swagger) 
}
## http://api.kountable.com/api/v2/users/10395/identities

get.V2UsersIdentities <- function(apiUUID){
  ### Returns the search list by userId and name
  require(httr)
  require(dplyr)
  require(jsonlite)
  ##### SWAGGER for project calls from api.kountable.com
  
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         #"k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v2/users/',apiUUID,'/identities'),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        "k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  ## first isolate the provider data.frame from the call, rename variables then add back
  ## 
  tbl.provider <- tbl_df(getV2Swagger$data$provider)
  tbl.provider <- tbl.provider %>% select(provider_id = id, provider_code = code, provider_name = name, provider_description = description)
  tbl.data <- tbl_df(subset(getV2Swagger$data, select=-provider))
  tbl.identities <- data.frame(tbl.data, tbl.provider)
  
  return(tbl.identities) 
}
get.V3CountriesDictionary <- function() { 
  ### Returns the search list by userId and name
  require(httr)
  require(dplyr)
  ##### SWAGGER for project calls from api.kountable.com
  
  tSwaggerPost <- POST(paste('http://api.kountable.com/api/v2/user/login', sep = ""),
                       add_headers(
                         "Content-Type" = "application/json",
                         "k-app-type" = "3rd",
                         #"k-app-version" = "1.0.0",
                         "Cache-Control" = "no-cache"),
                       body = '{"primary_email" : "joe@kountable.com", "password" : "ksH-g3u-hkA-VYp"}') 
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  accessToken <- fromJSON(rawToChar(tSwaggerPost$content))
  
  getV2Swagger <- GET(paste0('http://api.kountable.com/api/v3/dictionary/countries'),
                      add_headers(
                        "Authorization" = paste("Bearer ", accessToken$data$access_token, sep=''),
                        "Content-Type" = "application/json", 
                        "k-app-type" = "3rd",
                        #"k-app-version" = "1.0.0",
                        "Cache-Control" = "no-cache"))
  getV2Swagger <- fromJSON(rawToChar(getV2Swagger$content), simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
  
  
  return(getV2Swagger)
}

get.AMLFreeData <- function(uuidApiKYC1){
  ## Frank AML FREE
  require(dplyr)
  require(shiny)
  tbl.foundIDs <- tbl_df(uuidApiKYC1$aml$free$profile$indexFoundData$foundIds)
  ## if no Frank free aml data then 
  if (nrow(tbl.foundIDs) > 0) {
    tbl.foundIDs <- tbl.foundIDs %>% 
      mutate(amlFreeListCode = toupper(gsub('([^_]*)_(.*)','\\1',id)),
             matchedUrl = ifelse(regexec('http',rawData)==1, gsub('(http.*?)(\t.*)','\\1',rawData), "no Url"),
             matchedText = gsub('\\)','', gsub('List\\(','', gsub('\t','; ', gsub('(http.*?)(\t.*?)(.*)','\\3', rawData)))))
    ## build HTML tags for Shiny DT table
    ## color likelihood 
    ## Add hyperlinks to matchedUrl
    tbl.foundIDs <- tbl.foundIDs %>% rowwise() %>% 
      mutate(likelihood = ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                 ifelse(likelihood < 0.50, paste0(tags$span(likelihood,style="color:gold")),
                                        paste0(tags$span(likelihood,style="color:red")))),
             matchedUrl = paste0(tags$a(paste0(matchedUrl), href = matchedUrl,target='_blank')))
    
    ## paste together everything but what is published in table form,  The idea here is to do firstNames: bill, Joe; LastNames: murray, pesci 
    searchQuery <- paste(sapply(names(sapply(uuidApiKYC1$aml$free$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'font-weight:bold'))}), sapply(uuidApiKYC1$aml$free$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    ## add first name, last name, userId
    tbl.foundIDs$userId <- uuidApiKYC1$uuid
    tbl.foundIDs$firstName <- uuidApiKYC1$profile$firstName
    tbl.foundIDs$lastName <- uuidApiKYC1$profile$lastName
    tbl.foundIDs$searchQuery <- searchQuery
    
    return(tbl.foundIDs)
    
    
  } else {
    ## if no rows in found Ids return No Match Table
    cat('_____________________________\n')
    cat('No Rows Found In AMLFreeTable\n')
    cat('_____________________________\n')
    tbl.foundIDs <-  data_frame(amlFreeListCode = paste0(tags$span("No Match",style="color:green")),likelihood = ' ',matchedText = ' ', matchedUrl = ' ', id = ' ' )
    
    ## paste together everything but what is published in table form,  The idea here is to do firstNames: bill, Joe; LastNames: murray, pesci 
    searchQuery <- paste(sapply(names(sapply(uuidApiKYC1$aml$free$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'font-weight:bold'))}), sapply(uuidApiKYC1$aml$free$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    ## add first name, last name, userId
    tbl.foundIDs$userId <- uuidApiKYC1$uuid
    tbl.foundIDs$firstName <- uuidApiKYC1$profile$firstName
    tbl.foundIDs$lastName <- uuidApiKYC1$profile$lastName
    tbl.foundIDs$searchQuery <- searchQuery
    
    
    return(tbl.foundIDs)}
  
}
get.AMLFreeDataOutputList <- function(uuidApiKYC1,shinyUser) {
  require(shiny)
  ## call get.AMLPaidData
  tbl.AMLPaidData <- get.AMLFreeData(uuidApiKYC1 = uuidApiKYC1)
  cat("====================================================\n")
  cat("called get.AMLFreeDataOutputList \n")
  cat("====================================================\n")
  ## make checkbox Review?
  ## tbl.AMLPaidData$amlPaidCheckBoxInputs <- lapply(1:nrow(tbl.AMLPaidData), function(j){paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_", j), label = tags$h6("Reviewed by"), value = 0)))})
  
  ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
  tbl.AMLPaidData$rowCount <-  1:nrow(tbl.AMLPaidData)
  
  # tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px;}")
  
  tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
    mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
           completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "", value = shinyUser, width='250px')))
  
  # my_list_1 <- lapply(1:nrow(tbl.AMLPaidData), function(i) {
  #   
  #   # plotname <- paste("amlPaid", i, sep="") 
  #   # output[[paste0('amlPaid', i)]] <- renderUI({
  #   outputItem <- renderUI({
  #     list(
  #       tags$div(tags$h3("AML Free Check Id: ",tags$span(tags$b(tbl.AMLPaidData$id[i]),style='color:black')),
  #                tags$p(tags$b(tags$span('User Id: ',style='color:black'),tbl.AMLPaidData$userId[[i]],
  #                              tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$firstName[[i]],
  #                              tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$lastName[[i]],
  #                              style = 'color:blue')) ## Use span to change color of labels
  #                , tags$blockquote("Search Query: ",HTML(tbl.AMLPaidData$searchQuery[[i]]))
  #                ),
  #       tags$h4("Details"),
  #       tags$h4('AML Free List Code: ',tags$b(HTML(tbl.AMLPaidData$amlFreeListCode[i]))),
  #       tags$h4("Likelihood:", HTML(tbl.AMLPaidData$likelihood[i])),
  #       tags$h4("Further Information: "), 
  #       tags$p(tbl.AMLPaidData$matchedText[i]),
  #       tags$h4("External Sources: "), 
  #       HTML(tbl.AMLPaidData$matchedUrl[i]),
  #       hr()
  #     )
  #   })
  # })
  # do.call(tagList, my_list_1)
  # return(my_list_1)
  ls.subList <- lapply(1, function(i) {
    outputItem <- renderUI({
      list(
        tags$div(
          tags$h4("AML Free Seach For: "),
          tags$p(tags$b(tags$span('User Id: ',style='color:black'),tbl.AMLPaidData$userId[[i]],
                        tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$firstName[[i]],
                        tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$lastName[[i]],
                        style = 'color:blue')) ## Use span to change color of labels
          , tags$blockquote("Search Query: ", HTML(tbl.AMLPaidData$searchQuery[[i]]))),
        DT::renderDataTable({
          DT::datatable(tbl.AMLPaidData[,c("likelihood","amlFreeListCode","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                        options = list(
                          #order = list(list(2, 'desc'), list(4, 'desc')),
                          # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                          # autoWidth = TRUE,
                          bLengthChange=0, # show/hide records per page dropdown
                          bFilter=0,       # global search box on/off
                          bInfo=0,         #removes # of reccords filter bottom left
                          bPaginate=0,      #removes the page select bottom right
                          columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                            list(className="dt-center", targets=c(0))
                          )
                        )
          ) 
        }),
        tags$h4("Details"),
        lapply(  1:nrow(tbl.AMLPaidData), function(j){
          list(
            tags$h4("AML Free Check Id: ",tags$span(tags$b(tbl.AMLPaidData$id[j]),style='color:black')),
            tags$h5('AML Free List Code: ',tags$b(HTML(tbl.AMLPaidData$amlFreeListCode[j]))),
            tags$h5("Likelihood:", HTML(tbl.AMLPaidData$likelihood[j])),
            # tags$h5("Key Data: "),
            # tags$p(HTML(tbl.AMLPaidData$amlKeyData[j])),
            tags$h5("Further Information: "), 
            tags$p(tbl.AMLPaidData$matchedText[j]),
            tags$h5("External Sources: "), 
            HTML(tbl.AMLPaidData$matchedUrl[j])
          )
        }),
        hr()
      )
    })
  })
  do.call(tagList, ls.subList)
  return(ls.subList) 
}

get.userBusinessList <- function(apiUUID) {
  require(dplyr)
  require(lubridate)
  cat(apiUUID, "--> Calling get.userBusinessList",'\n')
  
  ## data entry estimated times in minutes
  ## data frame used for login list/time table display in min
  
  
  ## projects
  tbl.userProjectsData <- get.V2ProjectsList(apiUUID)
  ## no projects means no businesses
  if(length(tbl.userProjectsData$data)!=0) {
    ## get data table
    tbl.userProjectsList <- tbl.userProjectsData$data
    
    ## get dates of businesses created
    tbl.userBusinessList <- get.V2BusinessesList(apiUUID)
    
    ## no businesses means no business list
    if(length(tbl.userBusinessList$data)!=0){
      ## identify role payors, suppliers, frank business
      ## get unique list of suppliers and payors
      frankPayors <- unique(unlist(tbl.userProjectsList$payors))
      frankSuppliers <- unique(unlist(tbl.userProjectsList$suppliers))
      frankBusinesses <- unique(unlist(tbl.userProjectsList$frank_business$id))
      
      ## since a business can be both a supplier and a payor, we need to make sure that we get multiple records if this occurs
      ## if any are null make a table with -99999 id so no matches
      ifelse(is.null(frankPayors), tbl.frankBusinessPayors <- data_frame(id = -99999, frankBusinessRole = 'payor'), 
             tbl.frankBusinessPayors <- data_frame(id =  frankPayors, frankBusinessRole = rep('payor',length(frankPayors))))
      ifelse(is.null(frankSuppliers), tbl.frankBusinessSuppliers <- data_frame(id = -99999, frankBusinessRole = 'supplier'), 
             tbl.frankBusinessSuppliers <- data_frame(id =  frankSuppliers, frankBusinessRole = rep('supplier',length(frankSuppliers))))
      ifelse(is.null(frankBusinesses), tbl.frankBusinesses <- data_frame(id =  c('-99999'), frankBusinessRole = 'frank business'), 
             tbl.frankBusinesses <- data_frame(id =  frankBusinesses, frankBusinessRole = rep('frank business',length(frankBusinesses))))
      
      ## build a table of frank business Roles of ids and roles
      ## make the construction conditional on whether these exist
      
      tbl.frankBusinessRole <- bind_rows(tbl.frankBusinessPayors,
                                         tbl.frankBusinessSuppliers,
                                         tbl.frankBusinesses)
      
      ##convert to dplyr tbl
      tbl.frankBusinessData <- tbl_df(tbl.userBusinessList$data)
      ## combine business list with business role - if the role exists for that business
      tbl.frankBusinessData <- left_join(tbl.frankBusinessData,tbl.frankBusinessRole, by = 'id')
      
      ## get rid of any NA roles
      tbl.frankBusinessData <-  tbl.frankBusinessData[which(!is.na(tbl.frankBusinessData$frankBusinessRole)),]
      ## rename id field to businessId
      tbl.frankBusinessData <- tbl.frankBusinessData %>% rename(businessId = id)
      
      ## return table
      return(tbl.frankBusinessData)
      
    } else{
      return(NULL)  
    }
    
  } else{
    return(NULL)
  }
  
  
}


get.businessAMLFreeTable <- function(businessKYC1){
  require(dplyr)
  require(shiny)
  cat('call get.businessAMLFreeTable for businessKYC information id = ',businessKYC1$information$id, '\n')
  tbl.foundIDs <- tbl_df(businessKYC1$aml$free$indexFoundData$foundIds)
  ## if no Frank free aml data then 
  if (nrow(tbl.foundIDs) > 0) {
    tbl.foundIDs <- tbl.foundIDs %>% 
      mutate(amlFreeListCode = toupper(gsub('([^_]*)_(.*)','\\1',id)),
             matchedUrl = ifelse(regexec('http',rawData)==1, gsub('(http.*?)(\t.*)','\\1',rawData), "no Url"),
             matchedText = gsub('\\)','', gsub('List\\(','', gsub('\t','; ', gsub('(http.*?)(\t.*?)(.*)','\\3', rawData)))))
    ## build HTML tags for Shiny DT table
    ## color likelihood 
    ## Add hyperlinks to matchedUrl
    tbl.foundIDs <- tbl.foundIDs %>% rowwise() %>% 
      mutate(likelihood = ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                 ifelse(likelihood < 0.50, paste0(tags$span(likelihood,style="color:gold")),
                                        paste0(tags$span(likelihood,style="color:red")))),
             matchedUrl = paste0(tags$a(paste0(matchedUrl), href = matchedUrl,target='_blank')))
    ## assign business ID, business name
    tbl.foundIDs$businessId <- businessKYC1$information$id
    
    
    
    return(tbl.foundIDs)
    
  } else {
    ## if no rows in found Ids return No Match Table
    cat('No Rows Found In frankBusinessAMLFreeTable for businessId:',businessKYC1$information$id,'\n')
    tbl.foundIDs <-  data_frame(amlFreeListCode = paste0(tags$span("No Match",style="color:green")),likelihood = ' ',matchedText = ' ', matchedUrl = ' ', id = ' ' )
    tbl.foundIDs$businessId <- businessKYC1$information$id
    
    return(tbl.foundIDs)}
  
}
## modify this to work for DT 
get.AMLFreeBusinessList <- function(tbl.AMLBusinessData,shinyUser) {
  require(shiny)
  # # Insert plot output objects the list
  # plot_output_list <- lapply(1:nrow(tbl.AMLBusinessData), function(i) {
  #   plotname <- paste("plot", i, sep="")
  #   plot_output_object <- DT::dataTableOutput(plotname, width = "100%") ## plotOutput(plotname, height = 280, width = 250)
  #   plot_output_object <- DT::renderDataTable({
  #     ## 'frankBusinessRole','businessId','name',
  #     DT::datatable(tbl.AMLBusinessData$rawDataTable[[i]][,c('amlFreeListCode','likelihood','matchedText','matchedUrl', 'id')], 
  #                   class='compact stripe', 
  #                   escape = FALSE, 
  #                   rownames = FALSE,
  #                   caption = tags$caption(style = 'caption-side: top; text-align: left;',
  #                                          tags$div(tags$h4("AML Free Business Seach For: ",
  #                                                           tags$p(tags$b(paste0(tbl.AMLBusinessData$frankBusinessRole[[i]],', ',tbl.AMLBusinessData$businessId[[i]],'-',tbl.AMLBusinessData$name[[i]]),style = 'color:blue')))
  #                                                   , tags$blockquote(paste0("Search Query: ", tbl.AMLBusinessData$searchQuery[[i]])))),
  #                   options = list(
  #                     #order = list(list(2, 'desc'), list(4, 'desc')),
  #                     pageLength=nrow(tbl.AMLBusinessData$rawDataTable[[i]]), # needs to match nrow on the table, currently with 4 addins its 24
  #                     autoWidth = TRUE,
  #                     bLengthChange=0, # show/hide records per page dropdown
  #                     bFilter=0,       # global search box on/off
  #                     bInfo=0,         #removes # of reccords filter bottom left
  #                     bPaginate=0,      #removes the page select bottom right
  #                     columnDefs = list(list(width = 50, targets = c(0, 1)),
  #                                       list(width = 400, targets = c(2)),
  #                                       list(className="dt-center", targets=c(0,1)))
  #                   )
  #                   
  #     )
  #   })
  # })
  # 
  # do.call(tagList, plot_output_list) # needed to display properly.
  # 
  # return(plot_output_list)
  
  require(shiny)
  cat("====================================================\n")
  cat("called get.AMLFreeBusinessList \n")
  cat("====================================================\n")
  
  tbl.AMLBusinessData$rawDataTable <- lapply(tbl.AMLBusinessData$rawDataTable, function(x){
    ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
    x$rowCount <-  1:nrow(x)
    x <- x %>% rowwise() %>% 
      mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
             completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "",value = shinyUser, width='250px')))
    
  })
  
  
  
  ls.subList <- lapply(1:nrow(tbl.AMLBusinessData), function(i) {
    outputItem <- renderUI({
      list(
        tags$div(
          tags$h4("AML Free Business Seach For: "),
          tags$p(tags$b(tags$span('Business Role: ',style='color:black'),toupper(tbl.AMLBusinessData$frankBusinessRole[[i]]),
                        tags$span('Business Id: ',style='color:black'),tbl.AMLBusinessData$businessId[[i]],
                        tags$span(' Business Name: ',style='color:black'), tbl.AMLBusinessData$name[[i]],
                        tags$span(' Business Description: ',style='color:black'), tbl.AMLBusinessData$description[[i]],
                        style = 'color:blue')) ## Use span to change color of labels
          
          , tags$blockquote(paste0("Search Query: ", tbl.AMLBusinessData$searchQuery[[i]])) ## Use span to change color of labels
          
        ),
        DT::renderDataTable({
          DT::datatable(tbl.AMLBusinessData$rawDataTable[[i]][,c("likelihood","amlFreeListCode","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                        options = list(
                          #order = list(list(2, 'desc'), list(4, 'desc')),
                          # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                          # autoWidth = TRUE,
                          bLengthChange=0, # show/hide records per page dropdown
                          bFilter=0,       # global search box on/off
                          bInfo=0,         #removes # of reccords filter bottom left
                          bPaginate=0,      #removes the page select bottom right
                          columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                            list(className="dt-center", targets=c(0))
                          )
                        )
          ) 
        }),
        tags$h4("Details"),
        lapply(  tbl.AMLBusinessData$rawDataTable[i], function(x){
          lapply(1:nrow(x),function(j){
            list(
              tags$h4("AML Free Check Id: ",tags$span(tags$b(x$id[j]),style='color:black')),
              tags$h5('AML Free List Code: ',tags$b(HTML(x$amlFreeListCode[j]))),
              tags$h5("Likelihood:", HTML(x$likelihood[j])),
              tags$h5("Further Information: "), 
              tags$p(x$matchedText[j]),
              tags$h5("External Sources: "), 
              HTML(x$matchedUrl[j])
            )
          })
        }),
        hr()
      )
    })
  })
  
  do.call(tagList, ls.subList)
  return(ls.subList)
}


get.AMLBusinessData <- function(apiUUID, amlPaidCheck){
  cat(apiUUID,' ',amlPaidCheck, 'get.amlbusinessdata\n')
  ## first get the user Business List 
  ## this returns a frankBusinessRole field to indentify if its a frankBusiness, Payor Or Supplier
  tbl.userBusinessList <- get.userBusinessList(apiUUID)
  
  ## call kyc for each business
  ls.amlFreeBusiness <- lapply(tbl.userBusinessList$businessId, get.apiKYCBusinessIDCall,kserv='prod.',amlPaidCheck =  amlPaidCheck)
  
  ## add in search query from call 
  # [[20]]
  # [1] "uuid:b8c920b3-6827-4f84-85b2-c71518b93013, countries1:be, countries2:rw, cities:202061, companyNames:agfa healthcare"
  tbl.userBusinessList$searchQuery <- lapply(lapply(ls.amlFreeBusiness, function(x){unlist(x$aml$free$indexFoundData$searchQuery)}), function(x){paste(names(x),x, sep = ':',collapse = ', ')})
  
  ## tbl.businessAMLFreeTable <- bind_rows(lapply(ls.amlFreeBusiness, get.businessAMLFreeTable))
  tbl.userBusinessList$rawDataTable <- lapply(ls.amlFreeBusiness, get.businessAMLFreeTable)
  
  ## add to master business table paidTables from  get.AMLBusinessPaidData Call
  tbl.userBusinessList$paidTable <-  lapply(ls.amlFreeBusiness, get.AMLBusinessPaidData)
  
  # ## we need to add 
  # tbl.businessAMLFreeTable <- dplyr::left_join(tbl.businessAMLFreeTable, tbl.userBusinessList, by ='businessId')
  
  return(tbl.userBusinessList)
}


get.AMLBusinessPaidData <- function(apiBusiness){
  require(shiny)
  require(dplyr)
  cat("called get.AMLPaidData \n")
  ## if no paid data return null
  if (!is.null(apiBusiness$aml$paid$indexFoundData$foundIds$rawData)){
    ### WorldCheck Column Names
    worldCheckColumnNames <- c("UID",
                               "LAST NAME",
                               "FIRST NAME",
                               "ALIASES",
                               "ALTERNATIVE SPELLING",
                               "CATEGORY",
                               "TITLE",
                               "SUB-CATEGORY",
                               "POSITION",
                               "AGE",
                               "DOB",
                               "PLACE OF BIRTH",
                               "DECEASED",
                               "PASSPORTS",
                               "SSN",
                               "LOCATIONS",
                               "COUNTRIES",
                               "COMPANIES",
                               "E/I",
                               "LINKED TO",
                               "FURTHER INFORMATION",
                               "KEYWORDS",
                               "EXTERNAL SOURCES",
                               "ENTERED",
                               "UPDATED",
                               "EDITOR",
                               "AGE DATE (AS OF DATE)")
    
    
    ## ok first lets breakout FURTHER INFORMATION, EXTERNAL SOURCES and KEYWORDS into table elements.
    ## add Key words, likelihood
    ## paste the rest into Caption text
    ## combine together
    ls.amlPaidRawData <- lapply(lapply(apiBusiness$aml$paid$indexFoundData$foundIds$rawData, function(x){strsplit(x, '\t')}), function(x){lapply(x,function(y){setNames(y,worldCheckColumnNames[1:length(y)])})})
    
    
    
    ### EXTERNAL SOURCES
    ## name it right
    externalSources <-  sapply(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){strsplit(j[["EXTERNAL SOURCES"]], ' ')})}),function(y){paste0(sapply(y, function(x){paste0(tags$a(x, href=x, target="_blank"))}),collapse = ', ')})
    
    ### FURTHER INFORMATION
    ## name it right
    furtherInformation <-  sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["FURTHER INFORMATION"]]})})
    
    ### KEYWORDS
    ## name it right
    
    keywords <- gsub('~',', ',toupper(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["KEYWORDS"]]})})))
    
    
    ### Likelihood
    
    likelihood <- sapply(apiBusiness$aml$paid$indexFoundData$foundIds$likelihood, 
                         function(likelihood){ ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                                      ifelse(likelihood < 0.50, tags$span(paste0(likelihood),style="color:gold"),
                                                             tags$span(paste0(likelihood),style="color:red")))})
    
    ### UID
    ##
    # toupper(tbl.apiKYCPersonSearch$id)
    ## sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["UID"]]})})
    
    ## add in search query from call 
    # [[20]]
    # [1] "uuid:b8c920b3-6827-4f84-85b2-c71518b93013, countries1:be, countries2:rw, cities:202061, companyNames:agfa healthcare"
    # searchQuery <- paste(names(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery),
    #                      unlist(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery), sep = ':',collapse = ', ')
    ## paste together everything but what is published in table form,  The idea here is to do firstNames: bill, Joe; LastNames: murray, pesci 
    searchQuery <- paste(sapply(names(sapply(apiBusiness$aml$paid$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'font-weight:bold'))}), sapply(apiBusiness$aml$paid$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    
    
    ## select fields for keyData caption from worldcheck master list
    captionNames <- c("UID","LAST NAME","FIRST NAME","ALIASES","ALTERNATIVE SPELLING","CATEGORY","TITLE","SUB-CATEGORY","POSITION",
                      "AGE","DOB","PLACE OF BIRTH","DECEASED","PASSPORTS","SSN","LOCATIONS","COUNTRIES","COMPANIES","E/I","LINKED TO",
                      "ENTERED","AGE DATE (AS OF DATE)")
    
    # paste(names(unlist(ls.amlPaidRawData[[1]][[1]][captionNames])),unlist(ls.amlPaidRawData[[1]][[1]][captionNames]), sep = ':',collapse = ', ')
    
    ## make keyData
    keyData <- sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){paste(names(unlist(j[captionNames])),unlist(j[captionNames]), sep = ':',collapse = ', ')})})
    
    
    
    
    ## make it a data frame
    tbl.amlPaidProfile <- data_frame(searchQuery,keyData,keywords,likelihood,furtherInformation,externalSources, id = apiBusiness$aml$paid$indexFoundData$foundIds$id)
    
    ## user ID
    tbl.amlPaidProfile$businessId <- apiBusiness$aml$paid$identity$sourceUser
    
    ## business name and business description 
    tbl.amlPaidProfile$businessName <- apiBusiness$information$name 
    tbl.amlPaidProfile$businessDescription <- apiBusiness$information$description 
    
    ## return table
    return(tbl.amlPaidProfile)
  }
  else {
    ## if no rows in found Ids return No Match Table
    cat('No Rows Found In AMLBusinessPaidData for businessId:',apiBusiness$aml$paid$identity$sourceUser,'\n')
    ## paste together everything but what is published in table form,  The idea here is to do firstNames: bill, Joe; LastNames: murray, pesci 
    searchQuery <- paste(sapply(names(sapply(apiBusiness$aml$paid$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'font-weight:bold'))}), sapply(apiBusiness$aml$paid$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    tbl.amlPaidProfile <- data_frame(searchQuery=searchQuery,keyData = paste0(tags$b("No Match",style="color:green")),keywords=paste0(tags$b("No Match",style="color:green")),likelihood=' ',furtherInformation=' ',externalSources= ' ', id  = ' ')
    ## user ID
    tbl.amlPaidProfile$businessId <- apiBusiness$aml$paid$identity$sourceUser
    ## user first name, last name
    tbl.amlPaidProfile$businessName <- apiBusiness$information$name 
    tbl.amlPaidProfile$businessDescription <- apiBusiness$information$description 
    
    return(tbl.amlPaidProfile)
  }
  
  
}
get.AMLBusinessPaidDataOutputList <- function(tbl.AMLBusinessData, amlPaidCheck, shinyUser) {
  require(shiny)
  ## call get.AMLPaidData
  # tbl.AMLPaidData <- tbl.AMLBusinessData$paidTable[[1]]$keyData
  #str(tbl.AMLBusinessData$paidTable[[1]])
  cat("called get.AMLBusinessPaidDataOutputList amlPaidCheck = ",amlPaidCheck, "\n")
  
  #lapply(tbl.AMLBusinessData$paidTable, function(tbl.AMLPaidData){lapply(1:nrow(tbl.AMLPaidData), function(y){tbl.AMLPaidData$likelihood[y]})})
  if(amlPaidCheck == 'true'){
    
    
    #  my_list_1 <- lapply(tbl.AMLBusinessData$paidTable, function(tbl.AMLPaidData){lapply(1:nrow(tbl.AMLPaidData), function(i) {
    #   glimpse(tbl.AMLPaidData)
    #   # plotname <- paste("amlPaid", i, sep="") 
    #   # output[[paste0('amlPaid', i)]] <- renderUI({
    #   outputItem <- renderUI({
    #     list(
    #       tags$div(tags$h3("AML World Check Id: ",tags$span(tags$b(tbl.AMLPaidData$id[i]),style='color:black')),
    #                tags$p(tags$b(tags$span('Business Id: ',style='color:black'),tbl.AMLPaidData$businessId[[i]],
    #                              tags$span(' Business Name: ',style='color:black'), tbl.AMLPaidData$businessName[[i]],
    #                              tags$span(' Business Description: ',style='color:black'), tbl.AMLPaidData$businessDescription[[i]],
    #                              style = 'color:blue')) ## Use span to change color of labels
    #                , tags$blockquote("Search Query: ",HTML(tbl.AMLPaidData$searchQuery[[i]])),
    #                tags$blockquote("Key Data: ",HTML(tbl.AMLPaidData$keyData[i]))),
    #       tags$h4("Details"),
    #       tags$h4('Keywords: ',tags$b(HTML(tbl.AMLPaidData$keywords[i]))),
    #       tags$h4("Likelihood:", HTML(tbl.AMLPaidData$likelihood[i])),
    #       tags$h4("Further Information: "), 
    #       tags$p(tbl.AMLPaidData$furtherInformation[i]),
    #       tags$h4("External Sources: "), 
    #       HTML(tbl.AMLPaidData$externalSources[i]),
    #       hr()
    #     )
    #   })
    # })})
    # do.call(tagList, my_list_1)
    # return(my_list_1) 
    tbl.AMLBusinessData$paidTable <- lapply(tbl.AMLBusinessData$paidTable, function(x){
      ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
      x$rowCount <-  1:nrow(x)
      x <- x %>% rowwise() %>% 
        mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
               completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "",value = shinyUser, width='250px')))
      
    })
    
    ls.subList <- lapply(1:nrow(tbl.AMLBusinessData), function(i) {
      outputItem <- renderUI({
        list(
          tags$div(
            tags$h4("AML Worldcheck Business Seach For: "),
            tags$p(tags$b(tags$span('Business Role: ',style='color:black'),toupper(tbl.AMLBusinessData$frankBusinessRole[[i]]),
                          tags$span('Business Id: ',style='color:black'),tbl.AMLBusinessData$businessId[[i]],
                          tags$span(' Business Name: ',style='color:black'), tbl.AMLBusinessData$name[[i]],
                          tags$span(' Business Description: ',style='color:black'), tbl.AMLBusinessData$description[[i]],
                          style = 'color:blue')) ## Use span to change color of labels
            , tags$blockquote(paste0("Search Query: ", tbl.AMLBusinessData$searchQuery[[i]])) ## Use span to change color of labels
          ),
          DT::renderDataTable({
            DT::datatable(tbl.AMLBusinessData$paidTable[[i]][,c("likelihood","keywords","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                          options = list(
                            #order = list(list(2, 'desc'), list(4, 'desc')),
                            # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                            # autoWidth = TRUE,
                            bLengthChange=0, # show/hide records per page dropdown
                            bFilter=0,       # global search box on/off
                            bInfo=0,         #removes # of reccords filter bottom left
                            bPaginate=0,      #removes the page select bottom right
                            columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                              list(className="dt-center", targets=c(0))
                            )
                          )
            ) 
          }),
          tags$h4("Details"),
          lapply(  tbl.AMLBusinessData$paidTable[i], function(x){
            lapply(1:nrow(x),function(j){
              list(
                tags$h4("AML World Check Id: ",tags$span(tags$b(x$id[j]),style='color:black')),
                tags$h5('Keywords: ',tags$b(HTML(x$keywords[j]))),
                tags$h5("Likelihood:", HTML(x$likelihood[j])),
                tags$h5("Key Data: "),
                tags$p(HTML(x$keyData[j])),
                tags$h5("Further Information: "), 
                tags$p(x$furtherInformation[j]),
                tags$h5("External Sources: "), 
                HTML(x$externalSources[j])
              )
            })
          }),
          hr()
        )
      })
    })
    do.call(tagList, ls.subList)
    return(ls.subList) 
    
  } else{
    tags$p("Worldcheck Not Selected")  
  }
  
}

get.apiKYCBusinessIDCall <- function(businessID='346',kserv='prod.', 
                                     amlPaidCheck='false' 
) {  
  require(httr)
  require(jsonlite)
  # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
  cat('businessID: ',businessID, " Called get.apiKYCBusinessIDCall",'amlPaidCheck =',amlPaidCheck,"\n",sep = "")
  attempts <- 0
  
  if (kserv == 'staging.') {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON(rawToChar(getAuthenticationToken$content))
      # for the 'staging' server look for the admin permissions token find the location index with grep 
      adminAuthenticationToken <- AuthenticationTokenGroup$token[ grep('admin',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat(KYCUUID,"--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
    }
  } else {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON( rawToChar(getAuthenticationToken$content))
      # for the 'Prod' server look for the person permissions token find the location index with grep 
      personAuthenticationToken <- AuthenticationTokenGroup$token[ grep('person',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat("--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
      
    }
  }
  
  ## Part 2 call the data using the bearer token from part one, each server has a different token name to use
  attempts <- 0
  ## repeat the call if it 'hangsup'
  if (kserv == 'staging.') {
    bearerToken <-  adminAuthenticationToken
  } else {
    bearerToken <- personAuthenticationToken
  }
  repeat {
    attempts = attempts + 1
    ## curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: faus5t9yuQYYK8lGE4ZvJuwS4G2Fr3uq' 
    ## 'http://kyc-api.kountable.com/v0/businesses?amlPaidCheck=true&
    ## companyNames=berry%20and%20donaldson%20pty%20limited%20
    ## &contacts=hayden%20fynn%20nikita%20britow%20wayne%20frazer
    ## &cities=captown
    ## &countries=south%20africa
    ## &phones=0219340212
    ## &aliases=bob
    ## &regions=bremen%20close'
    ## curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: slkjdfa;sljdf;alksjdf' 
    ## 'http://kyc-api.kountable.com/v0/businesses/1011?amlPaidCheck=true'
    
    apiKYCget <- GET(paste0('http://',kserv,'kyc-api.kountable.com/v0/businesses/',businessID,'?amlPaidCheck=', amlPaidCheck),
                     add_headers(
                       "Accept" = "application/json",
                       "KYC-Bearer-Token" = bearerToken
                     ))
    ## How many call attempts
    cat("--> get.kycbusiness attempts-->", attempts, ' Server--> ',kserv, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep="")
    
    # exit if the condition is met
    if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain"
              ,rawToChar(apiKYCget$content))==FALSE){ break} 
  }
  
  x <- fromJSON( rawToChar(apiKYCget$content), simplifyVector = TRUE, simplifyMatrix = TRUE, simplifyDataFrame = TRUE)
  ## the business call may not return the information endpoint, this is to put in businessId for the rest of the functions to access the correct 
  ## businessId
  if(is.null(x$information$id)) {
    x$information$id <- businessID
  }
  
  return(x) 
}



get.AMLPaidData <- function(uuidApiKYC){
  require(shiny)
  require(dplyr)
  cat("called get.AMLPaidData \n")
  ## if no paid data return null
  if (!is.null(uuidApiKYC$aml$paid$profile$indexFoundData$foundIds$rawData)){
    ### WorldCheck Column Names
    worldCheckColumnNames <- c("UID",
                               "LAST NAME",
                               "FIRST NAME",
                               "ALIASES",
                               "ALTERNATIVE SPELLING",
                               "CATEGORY",
                               "TITLE",
                               "SUB-CATEGORY",
                               "POSITION",
                               "AGE",
                               "DOB",
                               "PLACE OF BIRTH",
                               "DECEASED",
                               "PASSPORTS",
                               "SSN",
                               "LOCATIONS",
                               "COUNTRIES",
                               "COMPANIES",
                               "E/I",
                               "LINKED TO",
                               "FURTHER INFORMATION",
                               "KEYWORDS",
                               "EXTERNAL SOURCES",
                               "ENTERED",
                               "UPDATED",
                               "EDITOR",
                               "AGE DATE (AS OF DATE)")
    
    
    ## ok first lets breakout FURTHER INFORMATION, EXTERNAL SOURCES and KEYWORDS into table elements.
    ## add Key words, likelihood
    ## paste the rest into Caption text
    ## combine together
    ls.amlPaidRawData <- lapply(lapply(uuidApiKYC$aml$paid$profile$indexFoundData$foundIds$rawData, function(x){strsplit(x, '\t')}), function(x){lapply(x,function(y){setNames(y,worldCheckColumnNames[1:length(y)])})})
    
    
    
    ### EXTERNAL SOURCES
    ## name it right
    externalSources <-  sapply(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){strsplit(j[["EXTERNAL SOURCES"]], ' ')})}),function(y){paste0(sapply(y, function(x){paste0(tags$a(x, href=x, target="_blank"))}),collapse = ', ')})
    
    ### FURTHER INFORMATION
    ## name it right
    furtherInformation <-  sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["FURTHER INFORMATION"]]})})
    
    ### KEYWORDS
    ## name it right
    
    keywords <- gsub('~',', ',toupper(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["KEYWORDS"]]})})))
    
    
    ### Likelihood
    
    likelihood <- sapply(uuidApiKYC$aml$paid$profile$indexFoundData$foundIds$likelihood, 
                         function(likelihood){ ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                                      ifelse(likelihood < 0.50, tags$span(paste0(likelihood),style="color:gold"),
                                                             tags$span(paste0(likelihood),style="color:red")))})
    
    ### UID
    ##
    # toupper(uuidApiKYC$aml$paid$profile$indexFoundData$foundIds$id)
    ## sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["UID"]]})})
    
    ## add in search query from call 
    # [[20]]
    # [1] "uuid:b8c920b3-6827-4f84-85b2-c71518b93013, countries1:be, countries2:rw, cities:202061, companyNames:agfa healthcare"
    # searchQuery <- paste(names(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery),
    #                      unlist(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery), sep = ':',collapse = ', ')
    ## paste together everything but what is published in table form,  The idea here is to do firstNames: bill, Joe; LastNames: murray, pesci 
    searchQuery <- paste(sapply(names(sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'font-weight:bold'))}), sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    
    
    ## select fields for keyData caption from worldcheck master list
    captionNames <- c("UID","LAST NAME","FIRST NAME","ALIASES","ALTERNATIVE SPELLING","CATEGORY","TITLE","SUB-CATEGORY","POSITION",
                      "AGE","DOB","PLACE OF BIRTH","DECEASED","PASSPORTS","SSN","LOCATIONS","COUNTRIES","COMPANIES","E/I","LINKED TO",
                      "ENTERED","AGE DATE (AS OF DATE)")
    
    # paste(names(unlist(ls.amlPaidRawData[[1]][[1]][captionNames])),unlist(ls.amlPaidRawData[[1]][[1]][captionNames]), sep = ':',collapse = ', ')
    
    ## make keyData
    keyData <- sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){paste(names(unlist(j[captionNames])),unlist(j[captionNames]), sep = ':',collapse = ', ')})})
    
    
    
    
    ## make it a data frame
    tbl.amlPaidProfile <- data_frame(searchQuery,keyData,keywords,likelihood,furtherInformation,externalSources, id = uuidApiKYC$aml$paid$profile$indexFoundData$foundIds$id)
    
    ## user ID
    tbl.amlPaidProfile$userId <- uuidApiKYC$aml$paid$profile$identity$sourceUser
    
    ## user first name, last name
    tbl.amlPaidProfile$firstName <- uuidApiKYC$profile$firstName
    tbl.amlPaidProfile$lastName <- uuidApiKYC$profile$lastName
    
    ## return table
    return(tbl.amlPaidProfile)
  }
  else {
    ## if no rows in found Ids return No Match Table
    cat('No Rows Found In AMLPaidData for userId:',uuidApiKYC$aml$paid$profile$identity$sourceUser,'\n')
    searchQuery <- paste(sapply(names(sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'color:black;font-weight:bold'))}), sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    tbl.amlPaidProfile <- data_frame(searchQuery=searchQuery,keyData = paste0(tags$b("No Match",style="color:green")),keywords=paste0(tags$b("No Match",style="color:green")),likelihood=' ',furtherInformation=' ',externalSources= ' ', id  = ' ')
    ## user ID
    tbl.amlPaidProfile$userId <- uuidApiKYC$aml$paid$profile$identity$sourceUser
    ## user first name, last name
    tbl.amlPaidProfile$firstName <- uuidApiKYC$profile$firstName
    tbl.amlPaidProfile$lastName <- uuidApiKYC$profile$lastName
    
    return(tbl.amlPaidProfile)
  }
  
  
}

get.AMLPaidDataOutput <- function(uuidApiKYC) {
  require(shiny)
  ## call get.AMLPaidData
  tbl.AMLPaidData <- get.AMLPaidData(uuidApiKYC = uuidApiKYC)
  cat("called get.AMLPaidDataOutput \n")
  # Insert plot output objects the list
  plot_output_list <- lapply(1:nrow(tbl.AMLPaidData), function(i) {
    plotname <- paste("plot", i, sep="")
    plot_output_object <- DT::dataTableOutput(plotname, width = "100%") ## plotOutput(plotname, height = 280, width = 250)
    plot_output_object <- DT::renderDataTable({
      ## 'frankBusinessRole','businessId','name',
      DT::datatable(tbl.AMLPaidData[i,c('keywords','likelihood','furtherInformation','externalSources','id')], 
                    class='compact stripe', 
                    escape = FALSE, 
                    rownames = FALSE,
                    caption = tags$caption(style = 'caption-side: top; text-align: left;',
                                           tags$div(tags$h4("AML World Check For: ",
                                                            tags$p(tags$b(tags$span('User Id: ',style='color:black'),tbl.AMLPaidData$userId[[i]], 
                                                                          tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$firstName[[i]], 
                                                                          tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$lastName[[i]], 
                                                                          style = 'color:blue'))) ## Use span to change color of labels
                                                    , tags$blockquote("Search Query: ",HTML(tbl.AMLPaidData$searchQuery[[i]])),
                                                    tags$blockquote("Key Data: ",HTML(tbl.AMLPaidData$keyData[i])))),
                    options = list(
                      #order = list(list(2, 'desc'), list(4, 'desc')),
                      pageLength=nrow(tbl.AMLPaidData[i,]), # needs to match nrow on the table, currently with 4 addins its 24
                      # autoWidth = TRUE,
                      bLengthChange=0, # show/hide records per page dropdown
                      bFilter=0,       # global search box on/off
                      bInfo=0,         #removes # of reccords filter bottom left
                      bPaginate=0,      #removes the page select bottom right
                      columnDefs = list(list(width = 50, targets = c(0, 1)),
                                        list(width = 400, targets = c(2,3)),
                                        list(className="dt-center", targets=c(0,1)))
                    )
                    
      )
    })
  })
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}
get.AMLPaidDataOutputList <- function(uuidApiKYC, amlPaid='false', shinyUser) {
  require(shiny)
  ## call get.AMLPaidData
  tbl.AMLPaidData <- get.AMLPaidData(uuidApiKYC = uuidApiKYC)
  cat("====================================================\n")
  cat("called get.AMLPaidDataOutputList \n")
  cat("amlPaid = ",amlPaid,'\n')
  cat("====================================================\n")
  
  ## make checkbox Review?
  ## tbl.AMLPaidData$amlPaidCheckBoxInputs <- lapply(1:nrow(tbl.AMLPaidData), function(j){paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_", j), label = tags$h6("Reviewed by"), value = 0)))})
  
  ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
  tbl.AMLPaidData$rowCount <-  1:nrow(tbl.AMLPaidData)
  
  # tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px;}")
  
  tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
    mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
           completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "", value=shinyUser,  width='250px')))
  
  # Insert plot output objects the list
  # plot_output_list <- lapply(1:nrow(tbl.AMLPaidData), function(i) {
  #   plotname <- paste("plot", i, sep="")
  #   plot_output_object <- DT::dataTableOutput(plotname, width = "100%") ## plotOutput(plotname, height = 280, width = 250)
  #   plot_output_object <- DT::renderDataTable({
  #     ## 'frankBusinessRole','businessId','name',
  #     DT::datatable(tbl.AMLPaidData[i,c('keywords','likelihood','furtherInformation','externalSources','id')], 
  #                   class='compact stripe', 
  #                   escape = FALSE, 
  #                   rownames = FALSE,
  #                   caption = tags$caption(style = 'caption-side: top; text-align: left;',
  #                                          tags$div(tags$h4("AML World Check For: ",
  #                                                           tags$p(tags$b(tags$span('User Id: ',style='color:black'),tbl.AMLPaidData$userId[[i]], 
  #                                                                         tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$firstName[[i]], 
  #                                                                         tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$lastName[[i]], 
  #                                                                         style = 'color:blue'))) ## Use span to change color of labels
  #                                                   , tags$blockquote("Search Query: ",HTML(tbl.AMLPaidData$searchQuery[[i]])),
  #                                                   tags$blockquote("Key Data: ",HTML(tbl.AMLPaidData$keyData[i])))),
  #                   options = list(
  #                     #order = list(list(2, 'desc'), list(4, 'desc')),
  #                     pageLength=nrow(tbl.AMLPaidData[i,]), # needs to match nrow on the table, currently with 4 addins its 24
  #                     # autoWidth = TRUE,
  #                     bLengthChange=0, # show/hide records per page dropdown
  #                     bFilter=0,       # global search box on/off
  #                     bInfo=0,         #removes # of reccords filter bottom left
  #                     bPaginate=0,      #removes the page select bottom right
  #                     columnDefs = list(list(width = 50, targets = c(0, 1)),
  #                                       list(width = 400, targets = c(2,3)),
  #                                       list(className="dt-center", targets=c(0,1)))
  #                   )
  #                   
  #     )
  #   })
  # })
  ## if worldcheck not selected return "worldcheck not selected"
  if(amlPaid == 'true'){
    #   my_list_1 <- lapply(1:nrow(tbl.AMLPaidData), function(i) {
    #     
    #     # plotname <- paste("amlPaid", i, sep="") 
    #     # output[[paste0('amlPaid', i)]] <- renderUI({
    #     outputItem <- renderUI({
    #       list(
    #         tags$div(tags$h3("AML World Check Id: ",tags$span(tags$b(tbl.AMLPaidData$id[i]),style='color:black')),
    #                  tags$p(tags$b(tags$span('User Id: ',style='color:black'),tbl.AMLPaidData$userId[[i]],
    #                                tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$firstName[[i]],
    #                                tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$lastName[[i]],
    #                                style = 'color:blue')) ## Use span to change color of labels
    #                  , tags$blockquote("Search Query: ",HTML(tbl.AMLPaidData$searchQuery[[i]])),
    #                  tags$blockquote("Key Data: ",HTML(tbl.AMLPaidData$keyData[i]))),
    #         tags$h4("Details"),
    #         tags$h4('Keywords: ',tags$b(HTML(tbl.AMLPaidData$keywords[i]))),
    #         tags$h4("Likelihood:", HTML(tbl.AMLPaidData$likelihood[i])),
    #         tags$h4("Further Information: "), 
    #         tags$p(tbl.AMLPaidData$furtherInformation[i]),
    #         tags$h4("External Sources: "), 
    #         HTML(tbl.AMLPaidData$externalSources[i]),
    #         hr()
    #       )
    #     })
    #   })
    #   do.call(tagList, my_list_1)
    #   return(my_list_1)
    ls.subList <- lapply(1, function(i) {
      outputItem <- renderUI({
        list(
          tags$div(
            tags$h4("AML Worldcheck Seach For: ",
                    tags$p(tags$b(tags$span('User Id: ',style='color:black'),tbl.AMLPaidData$userId[[i]],
                                  tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$firstName[[i]],
                                  tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$lastName[[i]],
                                  style = 'color:blue'))) ## Use span to change color of labels
            , tags$blockquote("Search Query: ", HTML(tbl.AMLPaidData$searchQuery[[i]]))),
          DT::renderDataTable({
            DT::datatable(tbl.AMLPaidData[,c("likelihood","keywords","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                          options = list(
                            #order = list(list(2, 'desc'), list(4, 'desc')),
                            # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                            # autoWidth = TRUE,
                            bLengthChange=0, # show/hide records per page dropdown
                            bFilter=0,       # global search box on/off
                            bInfo=0,         #removes # of reccords filter bottom left
                            bPaginate=0,      #removes the page select bottom right
                            columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                              list(className="dt-center", targets=c(0))
                            )
                          )
            ) 
          }),
          tags$h4("Details"),
          lapply(  1:nrow(tbl.AMLPaidData), function(j){
            list(
              tags$h4("AML World Check Id: ",tags$span(tags$b(tbl.AMLPaidData$id[j]),style='color:black')),
              tags$h5('Keywords: ',tags$b(HTML(tbl.AMLPaidData$keywords[j]))),
              tags$h5("Likelihood:", HTML(tbl.AMLPaidData$likelihood[j])),
              tags$h5("Key Data: "),
              tags$p(HTML(tbl.AMLPaidData$keyData[j])),
              tags$h5("Further Information: "), 
              tags$p(tbl.AMLPaidData$furtherInformation[j]),
              tags$h5("External Sources: "), 
              HTML(tbl.AMLPaidData$externalSources[j])
            )
          }),
          hr()
        )
      })
    })
    do.call(tagList, ls.subList)
    return(ls.subList) 
  } else{
    tags$p("Worldcheck Not Selected")  
  }
  
  
}
get.apiKYCPersonSearchCall <- function(kserv='', amlPaidCheck='false', firstNames='paul', lastNames='davis', emails='', cities='', 
                                       regions='', countries='', phones='', birthDays='', birthMonths='',birthYears='',nickName='') {  
  require(httr)
  require(jsonlite)
  # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
  cat(" Called get.apiKYCPersonSearchCall \n Search Query - firstNames:",	firstNames,	
      " lastNames:",	lastNames,	
      "\n emails:",	emails,	
      " cities:",		cities,
      " regions:",	regions,	
      " countries:",	countries,	
      "\n phones:",	phones,	
      " birthDays:",	birthDays,	
      " birthMonths:",	birthMonths,	
      " birthYears:",	birthYears,	
      "\n nickName:",nickName, "\n amlPaidCheck: ",amlPaidCheck,"\n",sep = "")
  attempts <- 0
  
  if (kserv == 'staging.') {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON(rawToChar(getAuthenticationToken$content))
      # for the 'staging' server look for the admin permissions token find the location index with grep 
      adminAuthenticationToken <- AuthenticationTokenGroup$token[ grep('admin',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat(KYCUUID,"--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
    }
  } else {
    repeat {
      ## Starts with 1
      attempts <-  attempts + 1
      # this Prod website bearer token call is
      # curl -X GET --header 'Accept: application/json' 'http://kyc-api.kountable.com/v0/admin/users'
      # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
      getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                    authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                    add_headers(
                                      "Accept" = "application/json")
      )         
      # the content returns a group of token calls in Json format, but first convert from raw to Char format
      AuthenticationTokenGroup <- fromJSON( rawToChar(getAuthenticationToken$content))
      # for the 'Prod' server look for the person permissions token find the location index with grep 
      personAuthenticationToken <- AuthenticationTokenGroup$token[ grep('person',AuthenticationTokenGroup$description)]
      #getAuthenticationToken$status_code
      cat("--> BearerToken attempts-->", attempts, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
      # exit if the condition is met
      if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
                rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
      
    }
  }
  
  ## Part 2 call the data using the bearer token from part one, each server has a different token name to use
  attempts <- 0
  ## repeat the call if it 'hangsup'
  if (kserv == 'staging.') {
    bearerToken <-  adminAuthenticationToken
  } else {
    bearerToken <- personAuthenticationToken
  }
  repeat {
    attempts = attempts + 1
    ## curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 
    ## 'http://kyc-api.kountable.com/v0/persons/aml?searchInPaidIndex=true
    ## &firstNames=Joe
    ## &lastNames=Doucette
    ## &emails=doucetteemail%40yahoo.com
    ## &cities=Keller
    # &regions=Texas
    # &countries=USA
    # &phones=8178769670
    # &birthDays=1
    # &birthMonths=5
    # &birthYears=2000
    # &nickName=Your%20Majesty'
    ### none of the birth fields work
    # paste0('http://',kserv,'kyc-api.kountable.com/v0/persons/aml?searchInPaidIndex=',amlPaidCheck, 
    #        '&firstNames=',URLencode(firstNames, reserved = TRUE),
    #        '&lastNames=',URLencode(lastNames, reserved = TRUE),
    #        '&emails=', URLencode(emails, reserved = TRUE),
    #        '&cities=', URLencode(cities, reserved = TRUE),
    #        '&regions=',URLencode(regions, reserved = TRUE),
    #        '&countries=',URLencode(countries, reserved = TRUE),
    #        '&phones=',URLencode(phones, reserved = TRUE),
    #        '&birthDays=',URLencode(birthDays, reserved = TRUE),
    #        '&birthMonths=', URLencode(birthMonths, reserved = TRUE),
    #        '&birthYears=', URLencode(birthYears, reserved = TRUE),
    #        '&nickName=',URLencode(nickName, reserved = TRUE))
    # # curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 'http://kyc-api.kountable.com/v0/persons/aml?searchInPaidIndex=false&firstNames=Paul&lastNames=Davis&emails=pauldavis%40gmail.com&cities=test&regions=texas&countries=usa&phones=555&nickName=the%20man'
    # paste0('http://',kserv,'kyc-api.kountable.com/v0/persons/aml?searchInPaidIndex=',amlPaidCheck, 
    #        '&firstNames=',URLencode(firstNames, reserved = TRUE),
    #        '&lastNames=',URLencode(lastNames, reserved = TRUE),
    #        '&emails=', URLencode(emails, reserved = TRUE),
    #        '&cities=', URLencode(paste(cities, collapse = ' '), reserved = TRUE),
    #        '&regions=',URLencode(regions, reserved = TRUE),
    #        '&countries=',URLencode(countries, reserved = TRUE),
    #        '&phones=',URLencode(phones, reserved = TRUE),
    #        '&nickName=',URLencode(nickName, reserved = TRUE))
    # '&cities=', URLencode(paste(cities, collapse = ' '), reserved = TRUE)
    cat('+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
    cat(paste0('http://',kserv,'kyc-api.kountable.com/v0/persons/aml?searchInPaidIndex=',amlPaidCheck, 
               '&firstNames=',URLencode(firstNames, reserved = TRUE),
               '&lastNames=',URLencode(lastNames, reserved = TRUE),
               '&emails=', URLencode(emails, reserved = TRUE),
               '&cities=', URLencode(cities, reserved = TRUE),
               '&regions=',URLencode(regions, reserved = TRUE),
               '&countries=',URLencode(countries, reserved = TRUE),
               '&phones=',URLencode(phones, reserved = TRUE),
               '&birthDays=',URLencode(birthDays, reserved = TRUE),
               '&birthMonths=', URLencode(birthMonths, reserved = TRUE),
               '&birthYears=', URLencode(birthYears, reserved = TRUE),
               '&nickName=',URLencode(nickName, reserved = TRUE)),'\n')
    cat('+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
    apiKYCget <- GET(paste0('http://',kserv,'kyc-api.kountable.com/v0/persons/aml?searchInPaidIndex=',amlPaidCheck, 
                            '&firstNames=',URLencode(firstNames, reserved = TRUE),
                            '&lastNames=',URLencode(lastNames, reserved = TRUE),
                            '&emails=', URLencode(emails, reserved = TRUE),
                            '&cities=', URLencode(cities, reserved = TRUE),
                            '&regions=',URLencode(regions, reserved = TRUE),
                            '&countries=',URLencode(countries, reserved = TRUE),
                            '&phones=',URLencode(phones, reserved = TRUE),
                            '&birthDays=',URLencode(birthDays, reserved = TRUE),
                            '&birthMonths=', URLencode(birthMonths, reserved = TRUE),
                            '&birthYears=', URLencode(birthYears, reserved = TRUE),
                            '&nickName=',URLencode(nickName, reserved = TRUE)),
                     add_headers(
                       "Accept" = "application/json",
                       "KYC-Bearer-Token" = bearerToken
                     ))
    ## How many call attempts
    cat("--> get.apiKYCPersonSearchCall attempts-->", attempts, ' Server--> ',kserv, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep="")
    
    # exit if the condition is met
    if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain"
              ,rawToChar(apiKYCget$content))==FALSE){ break} 
  }
  
  x <- fromJSON( rawToChar(apiKYCget$content), simplifyVector = TRUE, simplifyMatrix = TRUE, simplifyDataFrame = TRUE)
  
  print(x)
  ## if nothing is returned i.e. list() then create 'empty' table with included search tearms
  if (length(x) > 0 ){
    cat("______length > 0__________________\n")
    ## clean up the return call
    tbl.apiKYCPersonSearch <- tbl_df(x)
    
    
    ## To check existence ifelse("var2" %in% names(df), var2,NA)) 
    ## unlist, paste0 and collapse on ',' turns "joe" "smith" into "joe, smith"
    tbl.apiKYCPersonSearch <- tbl.apiKYCPersonSearch %>% rowwise() %>% 
      mutate(age = ifelse("age" %in% colnames(tbl.apiKYCPersonSearch), paste0(unlist( age), collapse = ', '),' '),
             birthYears = paste0(unlist( birthYears), collapse = ', '),
             birthDays = paste0(unlist( birthDays), collapse = ', '),
             birthMonths = paste0(unlist( birthMonths), collapse = ', '),
             countries = paste0(unlist( countries), collapse = ', '),
             lastNames = paste0(unlist( lastNames), collapse = ', '),
             firstNames = paste0(unlist( firstNames), collapse = ', '),
             regions = paste0(unlist( regions), collapse = ', '),
             cities = paste0(unlist( cities), collapse = ', '))
    ## add search query back in
    searchQuery <- paste0("First Names:",	firstNames,	
                          "; Last Names:",	lastNames,	
                          "; Emails:",	emails,	
                          "; Cities:",		cities,
                          "; Regions:",	regions,	
                          "; Countries:",	countries,	
                          "; Phones:",	phones,	
                          "; BirthDays:",	birthDays,	
                          "; BirthMonths:",	birthMonths,	
                          "; BirthYears:",	birthYears,	
                          "; NickName:",nickName)
    tbl.apiKYCPersonSearch$searchQuery <- searchQuery  
    tbl.apiKYCPersonSearch$searchFirstNames <- firstNames
    tbl.apiKYCPersonSearch$searchLastNames <- lastNames
    tbl.apiKYCPersonSearch$searchEmails <- emails
    tbl.apiKYCPersonSearch$searchRegions <- regions
    tbl.apiKYCPersonSearch$searchCountries <- countries
    tbl.apiKYCPersonSearch$searchPhones <- phones
    tbl.apiKYCPersonSearch$searchBirthDays <- birthDays
    tbl.apiKYCPersonSearch$searchBirthMonths <- birthMonths
    tbl.apiKYCPersonSearch$searchBirthYears <- birthYears
    tbl.apiKYCPersonSearch$searchNickName <- nickName
    
    ## select fields for keyData caption from worldcheck master list
    captionNames <- c("firstNames", "birthYears", "age", "countries", "birthMonths", "lastNames", "birthDays", "regions", "cities")
    
    
    ## make amlKeyData which combines all of the fields returned by the apiKYCPersonSearchCall
    amlKeyData <- sapply(1:nrow(tbl.apiKYCPersonSearch), function(i){paste(names(unlist(tbl.apiKYCPersonSearch[i,captionNames])),unlist(tbl.apiKYCPersonSearch[i,captionNames]), sep = ':',collapse = ', ')})
    tbl.apiKYCPersonSearch$amlKeyData <- amlKeyData
    return(tbl.apiKYCPersonSearch) 
  } else{
    
    tbl.apiKYCPersonSearch <- data_frame(likelihood=' ',source= ' ',firstNames= ' ',rawData= ' ',birthYears= ' ',age= ' ',countries= ' ',id= ' ',
                                         birthMonths= ' ',lastNames= ' ',birthDays= ' ',regions= ' ',cities=' ' )
    ## add search query back in
    searchQuery <- paste0("First Names:",	firstNames,	
                          "; Last Names:",	lastNames,	
                          "; Emails:",	emails,	
                          "; Cities:",		cities,
                          "; Regions:",	regions,	
                          "; Countries:",	countries,	
                          "; Phones:",	phones,	
                          "; BirthDays:",	birthDays,	
                          "; BirthMonths:",	birthMonths,	
                          "; BirthYears:",	birthYears,	
                          "; NickName:",nickName)
    tbl.apiKYCPersonSearch$searchQuery <- searchQuery  
    tbl.apiKYCPersonSearch$searchFirstNames <- firstNames
    tbl.apiKYCPersonSearch$searchLastNames <- lastNames
    tbl.apiKYCPersonSearch$searchEmails <- emails
    tbl.apiKYCPersonSearch$searchRegions <- regions
    tbl.apiKYCPersonSearch$searchCountries <- countries
    tbl.apiKYCPersonSearch$searchPhones <- phones
    tbl.apiKYCPersonSearch$searchBirthDays <- birthDays
    tbl.apiKYCPersonSearch$searchBirthMonths <- birthMonths
    tbl.apiKYCPersonSearch$searchBirthYears <- birthYears
    tbl.apiKYCPersonSearch$searchNickName <- nickName
    
    ## select fields for keyData caption from worldcheck master list
    captionNames <- c("firstNames", "birthYears", "age", "countries", "birthMonths", "lastNames", "birthDays", "regions", "cities")
    
    
    ## make amlKeyData which combines all of the fields returned by the apiKYCPersonSearchCall
    amlKeyData <- sapply(1:nrow(tbl.apiKYCPersonSearch), function(i){paste(names(unlist(tbl.apiKYCPersonSearch[i,captionNames])),unlist(tbl.apiKYCPersonSearch[i,captionNames]), sep = ':',collapse = ', ')})
    tbl.apiKYCPersonSearch$amlKeyData <- amlKeyData
    
    return(tbl.apiKYCPersonSearch)   
  }
  
  
}

get.searchAMLFreeData <- function(tbl.apiKYCPersonSearch){
  ## Frank AML FREE
  require(dplyr)
  require(shiny)
  
  cat("---------------------------------------------------------------------\n")
  cat("inside get.searchAMLFreeTable\n")
  tbl.foundIDs <- tbl.apiKYCPersonSearch[which(tbl.apiKYCPersonSearch$source == 'freecheck'),]
  cat("---------------------------------------------------------------------\n")
  cat("print tbl.foundIDs --get.searchAMLFreeTable\n")
  print(tbl.foundIDs)
  
  # 
  # ## if no Frank free aml data then 
  # if (nrow(tbl.foundIDs) > 0) {
  #   tbl.foundIDs <- tbl.foundIDs %>% 
  #     mutate(amlFreeListCode = toupper(gsub('([^_]*)_(.*)','\\1',id)),
  #            matchedUrl = ifelse(regexec('http',rawData)==1, gsub('(http.*?)(\t.*)','\\1',rawData), "no Url"),
  #            matchedText = gsub('\\)','', gsub('List\\(','', gsub('\t','; ', gsub('(http.*?)(\t.*?)(.*)','\\3', rawData)))))
  #   ## build HTML tags for Shiny DT table
  #   ## color likelihood 
  #   ## Add hyperlinks to matchedUrl
  #   tbl.foundIDs <- tbl.foundIDs %>% rowwise() %>% 
  #     mutate(likelihood = ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
  #                                ifelse(likelihood < 0.50, tags$span(paste0(likelihood),style="color:gold"),
  #                                       tags$span(paste0(likelihood),style="color:red"))),
  #            matchedUrl = paste0(tags$a(paste0(matchedUrl), href = matchedUrl,target='_blank')))
  #   return(tbl.foundIDs)
  #   
  # } else {
  #   ## if no rows in found Ids return No Match Table
  #   cat('No Rows Found In frankAMLFreeTable')
  #   tbl.foundIDs <-  data_frame(amlFreeListCode = paste0(tags$span("No Match",style="color:green")),likelihood = ' ',matchedText = ' ', matchedUrl = ' ', id = ' ' )
  #   
  #   return(tbl.foundIDs)}
  # 
  ## if no Frank free aml data then 
  if (nrow(tbl.foundIDs) > 0) {
    tbl.foundIDs <- tbl.foundIDs %>% 
      mutate(amlFreeListCode = toupper(gsub('([^_]*)_(.*)','\\1',id)),
             matchedUrl = ifelse(regexec('http',rawData)==1, gsub('(http.*?)(\t.*)','\\1',rawData), "no Url"),
             matchedText = gsub('\\)','', gsub('List\\(','', gsub('\t','; ', gsub('(http.*?)(\t.*?)(.*)','\\3', rawData)))))
    ## build HTML tags for Shiny DT table
    ## color likelihood 
    ## Add hyperlinks to matchedUrl
    tbl.foundIDs <- tbl.foundIDs %>% rowwise() %>% 
      mutate(likelihood = ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                 ifelse(likelihood < 0.50, paste0(tags$span(likelihood,style="color:gold")),
                                        paste0(tags$span(likelihood,style="color:red")))),
             matchedUrl = paste0(tags$a(paste0(matchedUrl), href = matchedUrl,target='_blank')))
    
    
    
    return(tbl.foundIDs)
    
    
  } else {
    ## if no rows in found Ids return No Match Table
    cat('_____________________________\n')
    cat('No Rows Found In AMLFreeTable\n')
    cat('_____________________________\n')
    
    # amlKYCSearchFields <- c("searchQuery","searchFirstNames","searchLastNames","searchEmails","searchRegions","searchCountries","searchPhones","searchBirthDays","searchBirthMonths","searchBirthYears","searchNickName","amlKeyData") 
    # tbl.foundIDs <-  data_frame(amlFreeListCode = paste0(tags$span("No Match",style="color:green")),likelihood = ' ',matchedText = ' ', matchedUrl = ' ', id = ' ', tbl.apiKYCPersonSearch[,amlKYCSearchFields] )
    tbl.apiKYCPersonSearch <- tbl.apiKYCPersonSearch[1,]
    tbl.apiKYCPersonSearch$amlFreeListCode <- paste0(tags$span("No Match",style="color:green"))
    tbl.apiKYCPersonSearch$likelihood <-  ' '
    tbl.apiKYCPersonSearch$matchedText <-  ' '
    tbl.apiKYCPersonSearch$matchedUrl <-  ' '
    tbl.apiKYCPersonSearch$id <-  paste0(tags$span("No Match",style="color:green"))
    return(tbl.apiKYCPersonSearch)}
}
get.searchAMLFreeDataOutputList <- function(tbl.apiKYCPersonSearch, shinyUser) {
  require(shiny)
  ## call get.AMLPaidData
  tbl.AMLPaidData <- get.searchAMLFreeData(tbl.apiKYCPersonSearch = tbl.apiKYCPersonSearch)
  cat("====================================================\n")
  cat("called get.AMLFreeDataOutputList \n")
  cat("====================================================\n")
  ## make checkbox Review?
  ## tbl.AMLPaidData$amlPaidCheckBoxInputs <- lapply(1:nrow(tbl.AMLPaidData), function(j){paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_", j), label = tags$h6("Reviewed by"), value = 0)))})
  
  ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
  tbl.AMLPaidData$rowCount <-  1:nrow(tbl.AMLPaidData)
  
  # tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px;}")
  
  tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
    mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
           completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "",value=shinyUser, width='250px')))
  
  # my_list_1 <- lapply(1:nrow(tbl.AMLPaidData), function(i) {
  #   
  #   # plotname <- paste("amlPaid", i, sep="") 
  #   # output[[paste0('amlPaid', i)]] <- renderUI({
  #   outputItem <- renderUI({
  #     list(
  #       tags$div(tags$h3("AML Free Check Id: ",tags$span(tags$b(tbl.AMLPaidData$id[i]),style='color:black')),
  #                tags$p(tags$b(tags$span('User Id: ',style='color:black'),tbl.AMLPaidData$userId[[i]],
  #                              tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$firstName[[i]],
  #                              tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$lastName[[i]],
  #                              style = 'color:blue')) ## Use span to change color of labels
  #                , tags$blockquote("Search Query: ",HTML(tbl.AMLPaidData$searchQuery[[i]]))
  #       ),
  #       tags$h4("Details"),
  #       tags$h4('AML Free List Code: ',tags$b(HTML(tbl.AMLPaidData$amlFreeListCode[i]))),
  #       tags$h4("Likelihood:", HTML(tbl.AMLPaidData$likelihood[i])),
  #       tags$h4("Further Information: "), 
  #       tags$p(tbl.AMLPaidData$matchedText[i]),
  #       tags$h4("External Sources: "), 
  #       HTML(tbl.AMLPaidData$matchedUrl[i]),
  #       hr()
  #     )
  #   })
  # })
  # do.call(tagList, my_list_1)
  # return(my_list_1)
  ls.subList <- lapply(1, function(i) {
    outputItem <- renderUI({
      list(
        tags$div(
          tags$h4("AML Free Seach For: "),
          tags$p(tags$b(tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$searchFirstNames[[i]],
                        tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$searchLastNames[[i]],
                        style = 'color:blue')) ## Use span to change color of labels
          , tags$blockquote("Search Query: ", HTML(tbl.AMLPaidData$searchQuery[[i]]))), 
        DT::renderDataTable({
          DT::datatable(tbl.AMLPaidData[,c("likelihood","amlFreeListCode","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                        options = list(
                          #order = list(list(2, 'desc'), list(4, 'desc')),
                          # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                          # autoWidth = TRUE,
                          bLengthChange=0, # show/hide records per page dropdown
                          bFilter=0,       # global search box on/off
                          bInfo=0,         #removes # of reccords filter bottom left
                          bPaginate=0,      #removes the page select bottom right
                          columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                            list(className="dt-center", targets=c(0))
                          )
                        )
          ) 
        }),
        tags$h4("Details"),
        lapply(  1:nrow(tbl.AMLPaidData), function(j){
          list(
            tags$h4('AML Free List Code: ',tags$b(HTML(tbl.AMLPaidData$amlFreeListCode[j]))),
            tags$h5("AML Free Check Id: ",tags$span(tags$b(HTML(tbl.AMLPaidData$id[j])),style='color:black')),
            tags$h5("Likelihood:", HTML(tbl.AMLPaidData$likelihood[j])),
            tags$h5("Key Data: "),
            tags$p(HTML(tbl.AMLPaidData$amlKeyData[j])),
            tags$h5("Further Information: "), 
            tags$p(tbl.AMLPaidData$matchedText[j]),
            tags$h5("External Sources: "), 
            HTML(tbl.AMLPaidData$matchedUrl[j])
          )
        }),
        hr()
      )
    })
  })
  do.call(tagList, ls.subList)
  return(ls.subList) 
}


get.searchAMLPaidData <- function(tbl.apiKYCPersonSearch){
  require(shiny)
  require(dplyr)
  cat("++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("called get.searchAMLPaidData \n")
  
  tbl.searchAMLPaidData <- tbl.apiKYCPersonSearch[which(tbl.apiKYCPersonSearch$source == 'worldcheck'),]
  cat("++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("Print tbl.searchAMLPaidData\n")
  print(tbl.searchAMLPaidData)
  ## if no paid data return null
  if (length(tbl.searchAMLPaidData$rawData)>0){
    ### WorldCheck Column Names
    cat('======================Inside tbl.searchAMLPaidData$rawData >0===============')
    worldCheckColumnNames <- c("UID",
                               "LAST NAME",
                               "FIRST NAME",
                               "ALIASES",
                               "ALTERNATIVE SPELLING",
                               "CATEGORY",
                               "TITLE",
                               "SUB-CATEGORY",
                               "POSITION",
                               "AGE",
                               "DOB",
                               "PLACE OF BIRTH",
                               "DECEASED",
                               "PASSPORTS",
                               "SSN",
                               "LOCATIONS",
                               "COUNTRIES",
                               "COMPANIES",
                               "E/I",
                               "LINKED TO",
                               "FURTHER INFORMATION",
                               "KEYWORDS",
                               "EXTERNAL SOURCES",
                               "ENTERED",
                               "UPDATED",
                               "EDITOR",
                               "AGE DATE (AS OF DATE)")
    
    
    ## ok first lets breakout FURTHER INFORMATION, EXTERNAL SOURCES and KEYWORDS into table elements.
    ## add Key words, likelihood
    ## paste the rest into Caption text
    ## combine together
    ls.amlPaidRawData <- lapply(lapply(tbl.searchAMLPaidData$rawData, function(x){strsplit(x, '\t')}), function(x){lapply(x,function(y){setNames(y,worldCheckColumnNames[1:length(y)])})})
    
    
    
    ### EXTERNAL SOURCES
    ## name it right
    externalSources <-  sapply(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){strsplit(j[["EXTERNAL SOURCES"]], ' ')})}),function(y){paste0(sapply(y, function(x){paste0(tags$a(x, href=x, target="_blank"))}),collapse = ', ')})
    
    ### FURTHER INFORMATION
    ## name it right
    furtherInformation <-  sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["FURTHER INFORMATION"]]})})
    
    ### KEYWORDS
    ## name it right
    
    keywords <- gsub('~',', ',toupper(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["KEYWORDS"]]})})))
    
    
    ### Likelihood
    
    likelihood <- sapply(tbl.searchAMLPaidData$likelihood, 
                         function(likelihood){ ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                                      ifelse(likelihood < 0.50, tags$span(paste0(likelihood),style="color:gold"),
                                                             tags$span(paste0(likelihood),style="color:red")))})
    
    ### UID
    ##
    # toupper(tbl.apiKYCPersonSearch$id)
    ## sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["UID"]]})})
    
    ## add in search query from call 
    # [[20]]
    # [1] "uuid:b8c920b3-6827-4f84-85b2-c71518b93013, countries1:be, countries2:rw, cities:202061, companyNames:agfa healthcare"
    # searchQuery <- paste(names(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery),
    #                      unlist(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery), sep = ':',collapse = ', ')
    ## paste together everything but what is published in table form,  The idea here is to do firstNames: bill, Joe; LastNames: murray, pesci 
    # searchQuery <- paste(sapply(names(sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'font-weight:bold'))}), sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    
    
    ## select fields for keyData caption from worldcheck master list
    captionNames <- c("UID","LAST NAME","FIRST NAME","ALIASES","ALTERNATIVE SPELLING","CATEGORY","TITLE","SUB-CATEGORY","POSITION",
                      "AGE","DOB","PLACE OF BIRTH","DECEASED","PASSPORTS","SSN","LOCATIONS","COUNTRIES","COMPANIES","E/I","LINKED TO",
                      "ENTERED","AGE DATE (AS OF DATE)")
    
    # paste(names(unlist(ls.amlPaidRawData[[1]][[1]][captionNames])),unlist(ls.amlPaidRawData[[1]][[1]][captionNames]), sep = ':',collapse = ', ')
    
    ## make keyData
    keyData <- sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){paste(names(unlist(j[captionNames])),unlist(j[captionNames]), sep = ':',collapse = ', ')})})
    
    
    
    tbl.searchAMLPaidData$keyData <- keyData
    tbl.searchAMLPaidData$keywords <- keywords
    tbl.searchAMLPaidData$likelihood <- likelihood
    tbl.searchAMLPaidData$furtherInformation <- furtherInformation
    tbl.searchAMLPaidData$externalSources <- externalSources
    
    ## user ID
    # tbl.amlPaidProfile$userId <- uuidApiKYC$aml$paid$profile$identity$sourceUser
    
    ## user first name, last name
    # tbl.amlPaidProfile$firstName <- uuidApiKYC$profile$firstName
    # tbl.amlPaidProfile$lastName <- uuidApiKYC$profile$lastName
    # 
    ## return table
    return(tbl.searchAMLPaidData)
  }
  else {
    ## if no rows in found Ids return No Match Table
    cat('No Rows Found In searchAMLPaidData for search:','\n')
    ##  
    
    tbl.apiKYCPersonSearch$id <-  paste0(tags$span("No Match",style="color:green"))
    tbl.apiKYCPersonSearch$keyData <- ' '
    tbl.apiKYCPersonSearch$keywords <- paste0(tags$span("No Match",style="color:green"))
    tbl.apiKYCPersonSearch$likelihood <- ' '
    tbl.apiKYCPersonSearch$furtherInformation <- ' '
    tbl.apiKYCPersonSearch$externalSources <- ' '
    
    return(tbl.apiKYCPersonSearch)
  }
  
  
}

get.searchAMLPaidDataOutputList <- function(tbl.apiKYCPersonSearch, amlPaidCheck='false',shinyUser) {
  require(shiny)
  ## call get.AMLPaidData
  tbl.AMLPaidData <- get.searchAMLPaidData(tbl.apiKYCPersonSearch = tbl.apiKYCPersonSearch)
  
  ## make checkbox Review?
  ## tbl.AMLPaidData$amlPaidCheckBoxInputs <- lapply(1:nrow(tbl.AMLPaidData), function(j){paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_", j), label = tags$h6("Reviewed by"), value = 0)))})
  
  ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
  tbl.AMLPaidData$rowCount <-  1:nrow(tbl.AMLPaidData)
  
  # tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px;}")
  
  tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
    mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
           completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "", value=shinyUser,  width='250px')))
  # tbl.AMLPaidData$completedBy
  # tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
  #   mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
  #          completedBy =paste0(tags$div(textInput(paste0("paidTextInputSignature_",rowCount), label = "", width=250),tags$style(type="text/css", paste0("#paidTextInputSignature_",rowCount, "{ font-size:20px; height:35px;}")))))
  # 
  # tbl.AMLPaidData$completedBy
  # tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
  #   mutate(amlPaidCheckBoxInputs = paste0(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="Reviewed By: ", value = FALSE, width = 150), textInput(paste0("paidTextInputSignature_",rowCount), label = "", width=50)))
  # 
  # tbl.AMLPaidData %>% rowwise() %>% mutate(amlPaidCheckBoxInputs = )
  cat("called get.AMLPaidDataOutputList \n")
  if(amlPaidCheck == 'true'){
    ls.subList <- lapply(1, function(i) {
      outputItem <- renderUI({
        list(
          tags$div(
            tags$h4("AML Worldcheck Seach For: "),
            tags$p(tags$b(tags$span(' First Name: ',style='color:black'), tbl.AMLPaidData$searchFirstNames[[i]],
                          tags$span(' Last Name: ',style='color:black'), tbl.AMLPaidData$searchLastNames[[i]],
                          style = 'color:blue')) ## Use span to change color of labels
            , tags$blockquote("Search Query: ", HTML(tbl.AMLPaidData$searchQuery[[i]]))),
          DT::renderDataTable({
            DT::datatable(tbl.AMLPaidData[,c("likelihood","keywords","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                          options = list(
                            #order = list(list(2, 'desc'), list(4, 'desc')),
                            # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                            # autoWidth = TRUE,
                            bLengthChange=0, # show/hide records per page dropdown
                            bFilter=0,       # global search box on/off
                            bInfo=0,         #removes # of reccords filter bottom left
                            bPaginate=0,      #removes the page select bottom right
                            columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                              list(className="dt-center", targets=c(0))
                            )
                          )
            ) 
          }),
          tags$h4("Details"),
          lapply(  1:nrow(tbl.AMLPaidData), function(j){
            list(
              # checkboxInput(paste0("checkbox_input_", j), label = tags$h6("Reviewed", tags$span(tags$b(HTML(tbl.AMLPaidData$id[j])),style='color:black')), value = 0),
              tags$h4("AML Worldcheck Id: ",tags$span(tags$b(HTML(tbl.AMLPaidData$id[j])),style='color:black')),
              tags$h4('Keywords: ',tags$b(HTML(tbl.AMLPaidData$keywords[j]))),
              tags$h5("Likelihood:", HTML(tbl.AMLPaidData$likelihood[j])),
              tags$h5("Key Data: "),
              tags$p(HTML(tbl.AMLPaidData$keyData[j])),
              tags$h5("Further Information: "), 
              tags$p(tbl.AMLPaidData$furtherInformation[j]),
              tags$h5("External Sources: "), 
              HTML(tbl.AMLPaidData$externalSources[j])
            )
          }),
          hr()
        )
      })
    })
    do.call(tagList, ls.subList)
    return(ls.subList) 
  } else{
    tags$p("Worldcheck Not Selected")  
  }
}


get.apiKYCBusinesSearchCall <- function(kserv='', amlPaidCheck='false', companyNames='', 
                                        contacts='',
                                        cities='',
                                        countries='',
                                        phones='',
                                        aliases='',
                                        regions='') {  
  require(httr)
  require(jsonlite)
  # curl -X GET --header 'Accept: application/json' 'http://staging.kyc-api.kountable.com/v0/admin/users'
  cat(" Called get.apiKYCBusinesSearchCall \n Search Query - \n",
      ' companyNames: ', companyNames,
      '\n contacts: ', contacts,
      '\n cities: ', cities,
      '\n countries: ', countries,
      '\n phones: ', phones,
      '\n aliases: ', aliases,
      '\n regions: ', regions, "\n amlPaidCheck: ",amlPaidCheck,"\n",sep = "")
  
  
  
  # this Prod website bearer token call is
  # curl -X GET --header 'Accept: application/json' 'http://kyc-api.kountable.com/v0/admin/users'
  # needs user name and password authentication 'admin', 'RzpvnDXv8fVjpu3h'
  getAuthenticationToken <- GET(paste('http://',kserv,'kyc-api.kountable.com/v0/admin/users', sep = ""),
                                authenticate('admin', 'RzpvnDXv8fVjpu3h', type = "basic"),
                                add_headers(
                                  "Accept" = "application/json"))         
  # the content returns a group of token calls in Json format, but first convert from raw to Char format
  AuthenticationTokenGroup <- fromJSON( rawToChar(getAuthenticationToken$content))
  # for the 'Prod' server look for the person permissions token find the location index with grep 
  personAuthenticationToken <- AuthenticationTokenGroup$token[ grep('person',AuthenticationTokenGroup$description)]
  #getAuthenticationToken$status_code
  cat("--> BearerToken  on Prod-->", ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep = "")
  # exit if the condition is met
  # if (grepl("Resource representation is only available with these Content-Types:\ntext/plain; charset=UTF-8\ntext/plain", 
  #           rawToChar(getAuthenticationToken$content)) == FALSE) {break} 
  # 
  
  
  
  ## Part 2 call the data using the bearer token from part one, each server has a different token name to use
  
  bearerToken <- personAuthenticationToken
  ########### build 
  ## curl -X GET --header 'Accept: application/json' --header 'KYC-Bearer-Token: oJLXXlf4Q24FZgCnYeYR3ps2oIXtVqzZ' 
  ## 'http://kyc-api.kountable.com/v0/businesses/aml
  ## ?amlPaidCheck=false
  ## &companyNames=Biopharmacia%20limited
  ## &contacts=bill%20bradley
  ## &cities=louisville
  ## &countries=Rwanda
  ## &phones=555
  ## &aliases=biomurder
  ## &regions=kigali'
  cat('We Got Here Joe \n')
  cat(paste0('http://',kserv,'kyc-api.kountable.com/v0/businesses/aml?amlPaidCheck=',amlPaidCheck,
             '&companyNames=',URLencode(companyNames, reserved = TRUE),
             '&contacts=',URLencode(contacts, reserved = TRUE),
             '&cities=', URLencode(cities, reserved = TRUE),
             '&countries=',URLencode(countries, reserved = TRUE),
             '&phones=',URLencode(phones, reserved = TRUE),
             '&aliases=',URLencode(aliases, reserved = TRUE),
             '&regions=',URLencode(regions, reserved = TRUE)),'\n')
  cat('+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
  
  apiKYCget <- GET(paste0('http://',kserv,'kyc-api.kountable.com/v0/businesses/aml?amlPaidCheck=',amlPaidCheck, 
                          '&companyNames=',URLencode(companyNames, reserved = TRUE),
                          '&contacts=',URLencode(contacts, reserved = TRUE),
                          '&cities=', URLencode(cities, reserved = TRUE),
                          '&countries=',URLencode(countries, reserved = TRUE),
                          '&phones=',URLencode(phones, reserved = TRUE),
                          '&aliases=',URLencode(aliases, reserved = TRUE),
                          '&regions=',URLencode(regions, reserved = TRUE)),
                   add_headers(
                     "Accept" = "application/json",
                     "KYC-Bearer-Token" = bearerToken
                   ))
  ## How many call attempts
  cat("--> get.apiKYCBusinesSearchCall attempts-->", ' Server--> ',kserv, ' TimeStamp: ',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n", sep="")
  
  
  
  x <- fromJSON( rawToChar(apiKYCget$content), simplifyVector = TRUE, simplifyMatrix = TRUE, simplifyDataFrame = TRUE)
  
  print(x)
  
  ## if nothing is returned i.e. list() then create 'empty' table with included search tearms
  if (length(x) > 0 ){
    cat("______length > 0__________________\n")
    ## clean up the return call
    tbl.apiKYCBusinessSearch <- tbl_df(x)
    
    ## To check existence ifelse("var2" %in% names(df), var2,NA)) 
    ## unlist, paste0 and collapse on ',' turns "joe" "smith" into "joe, smith"
    ## age = ifelse("age" %in% colnames(tbl.apiKYCBusinessSearch), paste0(unlist( age), collapse = ', '),' '),
    tbl.apiKYCBusinessSearch <- tbl.apiKYCBusinessSearch %>% rowwise() %>% 
      mutate(companyNames = paste0(unlist( companyNames), collapse = ', '),
             countries = paste0(unlist( countries), collapse = ', '),
             regions = paste0(unlist( regions), collapse = ', '),
             cities = paste0(unlist( cities), collapse = ', '),
             aliases = paste0(unlist( aliases), collapse = ', '))
    
    
    
    ## add search query back in
    # 'companyNames'
    # 'contacts'
    # 'cities'
    # 'countries'
    # 'phones'
    # 'aliases'
    # 'regions'
    
    searchQuery <- paste0("Company Names:",	companyNames,	
                          "; Contacts:",	contacts,	
                          "; Cities:",	cities,	
                          "; Countries:",		countries,
                          "; Phones:",	phones,	
                          "; Aliases:",	aliases,	
                          "; Regions:",	regions)
    tbl.apiKYCBusinessSearch$searchQuery <- searchQuery  
    tbl.apiKYCBusinessSearch$searchCompanyNames <- companyNames
    tbl.apiKYCBusinessSearch$searchContacts <- contacts
    tbl.apiKYCBusinessSearch$searchCities <- cities
    tbl.apiKYCBusinessSearch$searchCountries <- countries
    tbl.apiKYCBusinessSearch$searchPhones <- phones
    tbl.apiKYCBusinessSearch$searchAliases <- aliases
    tbl.apiKYCBusinessSearch$searchRegions <- regions
    
    ## select fields for keyData caption from worldcheck master list
    captionNames <- c("companyNames", "countries", "regions", "cities", "aliases")
    
    ## make amlKeyData which combines all of the fields returned by the apiKYCBusinessSearchCall
    amlKeyData <- sapply(1:nrow(tbl.apiKYCBusinessSearch), function(i){paste(names(unlist(tbl.apiKYCBusinessSearch[i,captionNames])),unlist(tbl.apiKYCBusinessSearch[i,captionNames]), sep = ':',collapse = ', ')})
    tbl.apiKYCBusinessSearch$amlKeyData <- amlKeyData
    return(tbl.apiKYCBusinessSearch) 
  } else{
    
    tbl.apiKYCBusinessSearch <- data_frame(id=' ',companyNames= ' ',likelihood= ' ',source= ' ',countries= ' ',rawData= ' ',regions= ' ',cities= ' ',
                                           aliases= ' ')
    
    ## add search query back in
    searchQuery <- paste0("Company Names:",	companyNames,	
                          "; Contacts:",	contacts,	
                          "; Cities:",	cities,	
                          "; Countries:",		countries,
                          "; Phones:",	phones,	
                          "; Aliases:",	aliases,	
                          "; Regions:",	regions)
    tbl.apiKYCBusinessSearch$searchQuery <- searchQuery  
    tbl.apiKYCBusinessSearch$searchCompanyNames <- companyNames
    tbl.apiKYCBusinessSearch$searchContacts <- contacts
    tbl.apiKYCBusinessSearch$searchCities <- cities
    tbl.apiKYCBusinessSearch$searchCountries <- countries
    tbl.apiKYCBusinessSearch$searchPhones <- phones
    tbl.apiKYCBusinessSearch$searchAliases <- aliases
    tbl.apiKYCBusinessSearch$searchRegions <- regions
    
    captionNames <- c("companyNames", "countries", "regions", "cities", "aliases")
    
    ## make amlKeyData which combines all of the fields returned by the apiKYCBusinessSearchCall
    amlKeyData <- sapply(1:nrow(tbl.apiKYCBusinessSearch), function(i){paste(names(unlist(tbl.apiKYCBusinessSearch[i,captionNames])),unlist(tbl.apiKYCBusinessSearch[i,captionNames]), sep = ':',collapse = ', ')})
    tbl.apiKYCBusinessSearch$amlKeyData <- amlKeyData
    return(tbl.apiKYCBusinessSearch)   
  }
  
  
}

get.searchAMLBusinessFreeData <- function(tbl.apiKYCBusinessSearch){
  ## Frank AML FREE
  require(dplyr)
  require(shiny)
  
  cat("---------------------------------------------------------------------\n")
  cat("inside get.searchAMLBusinessFreeData\n")
  tbl.foundIDs <- tbl.apiKYCBusinessSearch[which(tbl.apiKYCBusinessSearch$source == 'freecheck'),]
  cat("---------------------------------------------------------------------\n")
  cat("print tbl.foundIDs --get.searchAMLBusinessFreeData\n")
  print(tbl.foundIDs)
  
  
  ## if no Frank free aml data then 
  if (nrow(tbl.foundIDs) > 0) {
    tbl.foundIDs <- tbl.foundIDs %>% 
      mutate(amlFreeListCode = toupper(gsub('([^_]*)_(.*)','\\1',id)),
             matchedUrl = ifelse(regexec('http',rawData)==1, gsub('(http.*?)(\t.*)','\\1',rawData), "no Url"),
             matchedText = gsub('\\)','', gsub('List\\(','', gsub('\t','; ', gsub('(http.*?)(\t.*?)(.*)','\\3', rawData)))))
    
    ## url search link for adborg
    ## http://lnadbg4.adb.org/oga0009p.nsf/sancALL1P?SearchView&Query=grasco%20company&Count=30
    tbl.foundIDs <- tbl.foundIDs %>% rowwise() %>% 
      mutate(matchedUrl = ifelse(amlFreeListCode == 'ADBORG', 
                                 paste0('http://lnadbg4.adb.org/oga0009p.nsf/sancALL1P?SearchView&Query=',
                                        URLencode(companyNames, reserved = TRUE)), matchedUrl))
    
    ## build HTML tags for Shiny DT table
    ## color likelihood 
    ## Add hyperlinks to matchedUrl
    tbl.foundIDs <- tbl.foundIDs %>% rowwise() %>% 
      mutate(likelihood = ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                 ifelse(likelihood < 0.50, paste0(tags$span(likelihood,style="color:gold")),
                                        paste0(tags$span(likelihood,style="color:red")))),
             matchedUrl = paste0(tags$a(paste0(matchedUrl), href = matchedUrl,target='_blank')))
    
    
    
    return(tbl.foundIDs)
    
    
  } else {
    ## if no rows in found Ids return No Match Table
    cat('_____________________________\n')
    cat('No Rows Found In searchAMLBusinessFreeTable\n')
    cat('_____________________________\n')
    
    # amlKYCSearchFields <- c("searchQuery","searchFirstNames","searchLastNames","searchEmails","searchRegions","searchCountries","searchPhones","searchBirthDays","searchBirthMonths","searchBirthYears","searchNickName","amlKeyData") 
    # tbl.foundIDs <-  data_frame(amlFreeListCode = paste0(tags$span("No Match",style="color:green")),likelihood = ' ',matchedText = ' ', matchedUrl = ' ', id = ' ', tbl.apiKYCPersonSearch[,amlKYCSearchFields] )
    tbl.apiKYCBusinessSearch <- tbl.apiKYCBusinessSearch[1,]
    tbl.apiKYCBusinessSearch$amlFreeListCode <- paste0(tags$span("No Match",style="color:green"))
    tbl.apiKYCBusinessSearch$likelihood <-  ' '
    tbl.apiKYCBusinessSearch$matchedText <-  ' '
    tbl.apiKYCBusinessSearch$matchedUrl <-  ' '
    tbl.apiKYCBusinessSearch$id <-  paste0(tags$span("No Match",style="color:green"))
    return(tbl.apiKYCBusinessSearch)}
}
get.searchAMLBusinessFreeDataOutputList <- function(tbl.apiKYCBusinessSearch, shinyUser) {
  require(shiny)
  ## call get.AMLPaidData
  tbl.AMLPaidData <- get.searchAMLBusinessFreeData(tbl.apiKYCBusinessSearch = tbl.apiKYCBusinessSearch)
  cat("====================================================\n")
  cat("called get.searchAMLBusinessFreeDataOutputList \n")
  cat("====================================================\n")
  ## make checkbox Review?
  ## tbl.AMLPaidData$amlPaidCheckBoxInputs <- lapply(1:nrow(tbl.AMLPaidData), function(j){paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_", j), label = tags$h6("Reviewed by"), value = 0)))})
  
  ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
  tbl.AMLPaidData$rowCount <-  1:nrow(tbl.AMLPaidData)
  
  # tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px;}")
  
  tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
    mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
           completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "", value = shinyUser, width='250px')))
  
  ls.subList <- lapply(1, function(i) {
    outputItem <- renderUI({
      list(
        tags$div(
          tags$h4("AML Business Free Seach For: "),
          tags$p(tags$b(tags$span(' Company Names: ',style='color:black'), tbl.AMLPaidData$searchCompanyNames[[i]],
                        style = 'color:blue')) ## Use span to change color of labels
          , tags$blockquote("Search Query: ", HTML(tbl.AMLPaidData$searchQuery[[i]]))),
        DT::renderDataTable({
          DT::datatable(tbl.AMLPaidData[,c("likelihood","amlFreeListCode","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                        options = list(
                          #order = list(list(2, 'desc'), list(4, 'desc')),
                          # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                          # autoWidth = TRUE,
                          bLengthChange=0, # show/hide records per page dropdown
                          bFilter=0,       # global search box on/off
                          bInfo=0,         #removes # of reccords filter bottom left
                          bPaginate=0,      #removes the page select bottom right
                          columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                            list(className="dt-center", targets=c(0))
                          )
                        )
          ) 
        }),
        tags$h4("Details"),
        lapply(  1:nrow(tbl.AMLPaidData), function(j){
          list(
            tags$h4('AML Free List Code: ',tags$b(HTML(tbl.AMLPaidData$amlFreeListCode[j]))),
            tags$h5("AML Free Check Id: ",tags$span(tags$b(HTML(tbl.AMLPaidData$id[j])),style='color:black')),
            tags$h5("Likelihood:", HTML(tbl.AMLPaidData$likelihood[j])),
            tags$h5("Key Data: "),
            tags$p(HTML(tbl.AMLPaidData$amlKeyData[j])),
            tags$h5("Further Information: "), 
            tags$p(tbl.AMLPaidData$matchedText[j]),
            tags$h5("External Sources: "), 
            HTML(tbl.AMLPaidData$matchedUrl[j])
          )
        }),
        hr()
      )
    })
  })
  do.call(tagList, ls.subList)
  return(ls.subList) 
}


get.searchAMLBusinessPaidData <- function(tbl.apiKYCBusinessSearch){
  require(shiny)
  require(dplyr)
  cat("++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("called get.searchAMLBusinessPaidData \n")
  
  tbl.searchAMLPaidData <- tbl.apiKYCBusinessSearch[which(tbl.apiKYCBusinessSearch$source == 'worldcheck'),]
  cat("++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("Print tbl.searchAMLBusinessPaidData\n")
  print(tbl.searchAMLPaidData)
  ## if no paid data return null
  if (length(tbl.searchAMLPaidData$rawData)>0){
    ### WorldCheck Column Names
    cat('======================Inside tbl.searchAMLBusinessPaidData$rawData >0===============')
    worldCheckColumnNames <- c("UID",
                               "LAST NAME",
                               "FIRST NAME",
                               "ALIASES",
                               "ALTERNATIVE SPELLING",
                               "CATEGORY",
                               "TITLE",
                               "SUB-CATEGORY",
                               "POSITION",
                               "AGE",
                               "DOB",
                               "PLACE OF BIRTH",
                               "DECEASED",
                               "PASSPORTS",
                               "SSN",
                               "LOCATIONS",
                               "COUNTRIES",
                               "COMPANIES",
                               "E/I",
                               "LINKED TO",
                               "FURTHER INFORMATION",
                               "KEYWORDS",
                               "EXTERNAL SOURCES",
                               "ENTERED",
                               "UPDATED",
                               "EDITOR",
                               "AGE DATE (AS OF DATE)")
    
    
    ## ok first lets breakout FURTHER INFORMATION, EXTERNAL SOURCES and KEYWORDS into table elements.
    ## add Key words, likelihood
    ## paste the rest into Caption text
    ## combine together
    ls.amlPaidRawData <- lapply(lapply(tbl.searchAMLPaidData$rawData, function(x){strsplit(x, '\t')}), function(x){lapply(x,function(y){setNames(y,worldCheckColumnNames[1:length(y)])})})
    
    
    
    ### EXTERNAL SOURCES
    ## name it right
    externalSources <-  sapply(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){strsplit(j[["EXTERNAL SOURCES"]], ' ')})}),function(y){paste0(sapply(y, function(x){paste0(tags$a(x, href=x, target="_blank"))}),collapse = ', ')})
    
    ### FURTHER INFORMATION
    ## name it right
    furtherInformation <-  sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["FURTHER INFORMATION"]]})})
    
    ### KEYWORDS
    ## name it right
    
    keywords <- gsub('~',', ',toupper(sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["KEYWORDS"]]})})))
    
    
    ### Likelihood
    
    likelihood <- sapply(tbl.searchAMLPaidData$likelihood, 
                         function(likelihood){ ifelse(likelihood < 0.25, paste0(tags$span(likelihood,style="color:green")), 
                                                      ifelse(likelihood < 0.50, paste0(tags$span(likelihood,style="color:gold")),
                                                             paste0(tags$span(likelihood,style="color:red"))))})
    
    ### UID
    ##
    # toupper(tbl.apiKYCPersonSearch$id)
    ## sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){j[["UID"]]})})
    
    ## add in search query from call 
    # [[20]]
    # [1] "uuid:b8c920b3-6827-4f84-85b2-c71518b93013, countries1:be, countries2:rw, cities:202061, companyNames:agfa healthcare"
    # searchQuery <- paste(names(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery),
    #                      unlist(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery), sep = ':',collapse = ', ')
    ## paste together everything but what is published in table form,  The idea here is to do firstNames: bill, Joe; LastNames: murray, pesci 
    # searchQuery <- paste(sapply(names(sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')})), function(x){paste(tags$span(x,style = 'font-weight:bold'))}), sapply(uuidApiKYC$aml$paid$profile$indexFoundData$searchQuery, function(x){ paste(x,collapse = ', ')}), sep = ': ',collapse = '; ')
    
    
    
    ## select fields for keyData caption from worldcheck master list
    captionNames <- c("UID","LAST NAME","FIRST NAME","ALIASES","ALTERNATIVE SPELLING","CATEGORY","TITLE","SUB-CATEGORY","POSITION",
                      "AGE","DOB","PLACE OF BIRTH","DECEASED","PASSPORTS","SSN","LOCATIONS","COUNTRIES","COMPANIES","E/I","LINKED TO",
                      "ENTERED","AGE DATE (AS OF DATE)")
    
    # paste(names(unlist(ls.amlPaidRawData[[1]][[1]][captionNames])),unlist(ls.amlPaidRawData[[1]][[1]][captionNames]), sep = ':',collapse = ', ')
    
    ## make keyData
    keyData <- sapply(ls.amlPaidRawData, function(i){sapply(i, function(j){paste(names(unlist(j[captionNames])),unlist(j[captionNames]), sep = ':',collapse = ', ')})})
    
    
    
    tbl.searchAMLPaidData$keyData <- keyData
    tbl.searchAMLPaidData$keywords <- keywords
    tbl.searchAMLPaidData$likelihood <- likelihood
    tbl.searchAMLPaidData$furtherInformation <- furtherInformation
    tbl.searchAMLPaidData$externalSources <- externalSources
    
    ## user ID
    # tbl.amlPaidProfile$userId <- uuidApiKYC$aml$paid$profile$identity$sourceUser
    
    ## user first name, last name
    # tbl.amlPaidProfile$firstName <- uuidApiKYC$profile$firstName
    # tbl.amlPaidProfile$lastName <- uuidApiKYC$profile$lastName
    # 
    ## return table
    return(tbl.searchAMLPaidData)
  }
  else {
    ## if no rows in found Ids return No Match Table
    cat('No Rows Found In searchAMLBusinessPaidData for search:','\n')
    ##  
    
    tbl.apiKYCBusinessSearch$id <-  paste0(tags$span("No Match",style="color:green"))
    tbl.apiKYCBusinessSearch$keyData <- ' '
    tbl.apiKYCBusinessSearch$keywords <- paste0(tags$span("No Match",style="color:green"))
    tbl.apiKYCBusinessSearch$likelihood <- ' '
    tbl.apiKYCBusinessSearch$furtherInformation <- ' '
    tbl.apiKYCBusinessSearch$externalSources <- ' '
    
    return(tbl.apiKYCBusinessSearch)
  }
  
  
}

get.searchAMLBusinessPaidDataOutputList <- function(tbl.apiKYCBusinessSearch, amlPaidCheck='false',shinyUser) {
  require(shiny)
  ## call get.AMLPaidData
  tbl.AMLPaidData <- get.searchAMLBusinessPaidData(tbl.apiKYCBusinessSearch = tbl.apiKYCBusinessSearch)
  ## make checkbox Review?
  ## tbl.AMLPaidData$amlPaidCheckBoxInputs <- lapply(1:nrow(tbl.AMLPaidData), function(j){paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_", j), label = tags$h6("Reviewed by"), value = 0)))})
  
  ## add rowCount which is a row number.  Use this number on mutate to create the checkboxes and signature fields
  tbl.AMLPaidData$rowCount <-  1:nrow(tbl.AMLPaidData)
  
  # tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px;}")
  
  tbl.AMLPaidData <- tbl.AMLPaidData %>% rowwise() %>% 
    mutate(reviewCompleted = paste0(tags$div(checkboxInput(paste0("paidCheckbox_input_",rowCount), label="", value = FALSE, width = 25), style = "float:right")),
           completedBy =paste0(textInput(paste0("paidTextInputSignature_",rowCount), label = "", value = shinyUser, width='250px')))
  
  cat("called get.AMLBusinessPaidDataOutputList \n")
  if (amlPaidCheck == 'true'){
    ## build list in shiny
    ls.subList <- lapply(1, function(i) {
      outputItem <- renderUI({
        list(
          tags$div(
            tags$h4("AML Worldcheck Business Seach For: "),
            tags$p(tags$b(tags$span(' Company Names: ',style='color:black'), tbl.AMLPaidData$searchCompanyNames[[i]],
                          style = 'color:blue')) ## Use span to change color of labels
            , tags$blockquote("Search Query: ", HTML(tbl.AMLPaidData$searchQuery[[i]]))),
          DT::renderDataTable({
            DT::datatable(tbl.AMLPaidData[,c("likelihood","keywords","id",'reviewCompleted','completedBy')],class='compact stripe', escape = FALSE, rownames = FALSE,
                          options = list(
                            #order = list(list(2, 'desc'), list(4, 'desc')),
                            # pageLength=nrow(AMLFreeBusinessData()), # needs to match nrow on the table, currently with 4 addins its 24
                            # autoWidth = TRUE,
                            bLengthChange=0, # show/hide records per page dropdown
                            bFilter=0,       # global search box on/off
                            bInfo=0,         #removes # of reccords filter bottom left
                            bPaginate=0,      #removes the page select bottom right
                            columnDefs = list(list(width = 100, targets = c(0, 1, 2, 3)),
                                              list(className="dt-center", targets=c(0))
                            )
                          )
            ) 
          }),
          tags$h4("Details"),
          lapply(  1:nrow(tbl.AMLPaidData), function(j){
            list(
              tags$h4("AML Worldcheck Id: ",tags$span(tags$b(HTML(tbl.AMLPaidData$id[j])),style='color:black')),
              tags$h4('Keywords: ',tags$b(HTML(tbl.AMLPaidData$keywords[j]))),
              tags$h5("Likelihood:", HTML(tbl.AMLPaidData$likelihood[j])),
              tags$h5("Key Data: "),
              tags$p(HTML(tbl.AMLPaidData$keyData[j])),
              tags$h5("Further Information: "), 
              tags$p(tbl.AMLPaidData$furtherInformation[j]),
              tags$h5("External Sources: "), 
              HTML(tbl.AMLPaidData$externalSources[j])
            )
          }),
          hr()
        )
      })
    })
    do.call(tagList, ls.subList)
    return(ls.subList)   
  }else{
    tags$p("Worldcheck Not Selected")  
  }
  
}
get.citiesCodeNames <- function(tbl.requests){
  ## read in cities codes and make rds file
  ## cities_codes <- tbl_df(readRDS("/Users/doucetteemail/Documents/KountableFiles/RDevelopment/RMarker/RMarker/data/cities_codes.rds"))
  cities_codes <- tbl_df(readRDS("data/cities_codes.rds"))
  ## replace V1, V2 with cities, code
  colnames(cities_codes) <- c("cities","cityCodes")
  
  ## paste0(sapply(unlist(strsplit(tbl.requests$cities[19],', ')), function(x){paste0(cities_codes$cities[which(cities_codes$code == x)],collapse = ', ')}), collapse = '; ')
  
  ## unpack city codes and mark with row id cIndex
  ## combine into one table
  tbl.cities <- bind_rows(
    lapply(1:length(tbl.requests$cities), function(i){
      if (length(tbl.requests$cities[[i]])!=0){
        data_frame(cityCodes = tbl.requests$cities[[i]], cIndex=i)
      } else {
        data_frame(cityCodes = '', cIndex = i)
      }
    }))
  ## make cityCodes int to merge with cities_codes by cityCodes
  tbl.cities <- tbl.cities %>% mutate(cityCodes = as.integer(cityCodes))
  tbl.cities <- dplyr::left_join(tbl.cities,cities_codes, by = 'cityCodes')
  
  ## now pack up the rows into 
  ## final creation is "202061: kygaly, kigalis, kiqali, kgl, ji jia li, kigali, kigale, khikali, kigari, kigaly, gorad kigali, kinkali, kyghaly, kigalo, kigalli, kikali"
  tbl.cities <- tbl.cities %>% group_by(cIndex, cityCodes) %>% summarise(cityNames=paste(cities, collapse=", "))
  tbl.cities <- tbl.cities %>% rowwise() %>% mutate(cityCodeNames = paste0(cityCodes,': ', cityNames))
  tbl.cities <- tbl.cities %>% group_by(cIndex) %>% summarise(cityCodeNames = paste(cityCodeNames, collapse="; "))
  
  ## cleanup "NA: NA" and make blank
  tbl.cities <- tbl.cities %>% rowwise() %>% mutate(cityCodeNames = ifelse(cityCodeNames == "NA: NA","",cityCodeNames))
  ## final step merge back in columns
  
  tbl.requests$cityCodeNames <- tbl.cities$cityCodeNames
  return(tbl.requests)  
}
get.regionsCodeNames <- function(tbl.requests){
  ## read in cities codes and make rds file
  ## regions_codes <- tbl_df(readRDS("/Users/doucetteemail/Documents/KountableFiles/RDevelopment/RMarker/RMarker/data/regions_codes.rds"))
  regions_codes <- tbl_df(readRDS("data/regions_codes.rds"))
  ## replace V1, V2 with regions, code
  colnames(regions_codes) <- c("regions","regionsCodes")
  
  ## paste0(sapply(unlist(strsplit(tbl.requests$regions[19],', ')), function(x){paste0(regions_codes$regions[which(regions_codes$code == x)],collapse = ', ')}), collapse = '; ')
  
  ## unpack city codes and mark with row id cIndex
  ## combine into one table
  tbl.regions <- bind_rows(
    lapply(1:length(tbl.requests$regions), function(i){
      if (length(tbl.requests$regions[[i]])!=0){
        data_frame(regionsCodes = tbl.requests$regions[[i]], cIndex=i)
      } else {
        data_frame(regionsCodes = '', cIndex = i)
      }
    }))
  ## make cityCodes int to merge with regions_codes by cityCodes
  tbl.regions <- tbl.regions %>% mutate(regionsCodes = as.integer(regionsCodes))
  tbl.regions <- dplyr::left_join(tbl.regions,regions_codes, by = 'regionsCodes')
  
  ## now pack up the rows into 
  ## final creation is "202061: kygaly, kigalis, kiqali, kgl, ji jia li, kigali, kigale, khikali, kigari, kigaly, gorad kigali, kinkali, kyghaly, kigalo, kigalli, kikali"
  tbl.regions <- tbl.regions %>% group_by(cIndex, regionsCodes) %>% summarise(regionsNames=paste(regions, collapse=", "))
  tbl.regions <- tbl.regions %>% rowwise() %>% mutate(regionsCodeNames = paste0(regionsCodes,': ', regionsNames))
  tbl.regions <- tbl.regions %>% group_by(cIndex) %>% summarise(regionsCodeNames = paste(regionsCodeNames, collapse="; "))
  
  ## cleanup "NA: NA" and make blank
  tbl.regions <- tbl.regions %>% rowwise() %>% mutate(regionsCodeNames = ifelse(regionsCodeNames == "NA: NA","",regionsCodeNames))
  
  
  ## final step merge back in columns
  
  tbl.requests$regionsCodeNames <- tbl.regions$regionsCodeNames
  return(tbl.requests)  
}

