##################################################################
##################################################################
##################################################################

get.createresultfile <- function(tbl.searchGoogleItems, uuid = 504){
  require(dplyr)
  require(shiny)
  resultsUrls=unique(tbl.searchGoogleItems$link)
  tbl.webPresenceResults <- data_frame(matchScore = 0, resultsUrls = resultsUrls)
  tbl.webPresenceResults <- tbl.webPresenceResults %>% rowwise() %>%
    mutate(resultsUrls = paste0(tags$a(resultsUrls, href=resultsUrls, target="_blank")))
  return(tbl.webPresenceResults)
}

get.webPresencePagesOutputTable <- function(uuid = 504){
  require(dplyr)
  tAnalyticsPost <- POST(paste('http://analytics-prod-proxy.cluster.kountable.com:6000/research/websearch', sep = ""),
                         add_headers("Content-Type" = "application/json"),
                         body = paste0('{"source_user" : ',uuid , ',"type" : "user"}') )
  tContent <- fromJSON(rawToChar(tAnalyticsPost$content))
  
  tbl.keywords <- data_frame(keywords=tContent$keywords)
  tbl.helpwords <- data_frame(helpwords=tContent$helpwords)
  tbl.links <- data_frame(links=tContent$results$links,combinations=tContent$results$keywordsCombination)
  #
  
  
  #
  return(tbl.googleaSearchCombined)
}
