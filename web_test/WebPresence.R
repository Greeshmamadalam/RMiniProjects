get.createresultfile <- function(ls.urls, uuid = 504){
  require(dplyr)
  require(shiny)
  urls <- unlist(urls)
  tbl.webPresenceResults <- data_frame(urls = urls)
  tbl.webPresenceResults <- tbl.webPresenceResults %>% rowwise() %>%
    mutate(urls = paste0(tags$a(urls, href=urls, target="_blank")))
  return(tbl.webPresenceResults)
}
# uuid <- 504
# 
#test <- get.WebPresence(uuid = 10877)
get.WebPresence <- function(uuid){
  
  require(dplyr)
  require(shiny)
  require(httr)
  require(jsonlite)
  
  cat('--------------------------------------------------------------------------------\n')
  cat('get.WebPresence ---> ',paste('http://analytics-prod-proxy.cluster.kountable.com:6000/research/websearch', sep = ""),'\n')
  cat('--------------------------------------------------------------------------------\n')
  
  tAnalyticsPost <- POST(paste('http://analytics-prod-proxy.cluster.kountable.com:6000/research/websearch', sep = ""),
                         add_headers("Content-Type" = "application/json"),
                         body = paste0('{"source_user" : ',uuid , ',"type" : "user"}') )
  tContent <- fromJSON(rawToChar(tAnalyticsPost$content))
  tbl.links <- data_frame(links=tContent$results$links,combinations=tContent$results$keywordsCombination)
  urls <- lapply(tbl.links$links, scrape.urls)
  urls <- unlist(urls)
  
  cat('----------------------- ## make it a dplyr table------------------------\n')
  ## make it a dplyr table
  tbl.urls <- data_frame(urls) %>% rowwise %>% mutate(urls = paste0(tags$a(urls, href=urls, target="_blank")))
  ## add in uuid
  ls.urls <- list(uuid = uuid,
                  tbl.urls = tbl.urls)
  str(ls.urls)
  return(ls.urls)
}


scrape.urls <- function(urljunk){
  # tbl.urls <- data.frame(urls=urljunk$url)
  return(urljunk$url)
}



