library("rvest")
require(dplyr)
url <- "https://www.geolounge.com/fortune-1000-companies-list-2016/"
fortune1000 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="tablepress-28"]') %>% 
  html_table()
tbl.fortune100 <- data_frame(Company = fortune1000[[1]]$Company,
                           '2016' = fortune1000[[1]]$'2016',
                           '2015' = fortune1000[[1]]$'2015',
                           city = fortune1000[[1]]$City,
                           state = fortune1000[[1]]$State,
                           yearsOnList = fortune1000[[1]]$'Years On List (Fortune 500 only)')

# tbl.fortune100 %>% mutate(id = row_number())

url <- "http://analytics-prod-proxy.cluster.kountable.com:3000/franks/profiles/"+uuid+"?mode=rb"

xyz <- lapply(tbl.kscoreV2Summaries$uuid, function(uuid){
  url <- paste0("http://analytics-prod-proxy.cluster.kountable.com:3000/franks/profiles/",uuid,"?mode=rb")
  read_html(url)
})