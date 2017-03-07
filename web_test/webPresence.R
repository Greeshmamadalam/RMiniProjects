library(httr)
library(jsonlite)
library(dplyr)

apiUUID <- 10877
tAnalyticsPost <- POST(paste('http://analytics-prod-proxy.cluster.kountable.com:6000/research/websearch', sep = ""),
                       add_headers("Content-Type" = "application/json"),
                       body = paste0('{"source_user" : ',apiUUID , ',"type" : "user"}') )
tContent <- fromJSON(rawToChar(tAnalyticsPost$content))

tbl.keywords <- data_frame(keywords=tContent$keywords)
tbl.helpwords <- data_frame(helpwords=tContent$helpwords)
tbl.links <- data_frame(links=tContent$results$links,combinations=tContent$results$keywordsCombination)

