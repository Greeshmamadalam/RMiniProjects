require(RMySQL)
require(dplyr)
kRsocket <- "/var/run/mysql/mysql.sock"
kRUser <- "kdevdbuser"
kRUserPwd <- "t00rYFWOiRF"
kRDatabase <- "kountable_francine"
kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
kRDBPort <- 3306

conMySql<- dbConnect(dbDriver("MySQL"), user=kRUser, password=kRUserPwd, dbname=kRDatabase, host=kRHost, port=kRDBPort,
                     unix.socket=socket, client.flag=CLIENT_MULTI_STATEMENTS)
db.src_mysql <- src_mysql(kRDatabase, host = kRHost, port = kRDBPort, user = kRUser, password = kRUserPwd)

kRsocket <- "/var/run/mysql/mysql.sock"
kRUser <- "Analyst1"
kRUserPwd <- "L3tM3w0rk"
kRDatabase <- "kountableBackend_Dev"
#kRHost <- "173.74.198.235"
kRHost <- "47.190.45.73"
kRDBPort <- 3306
socket <- NULL

conMySql<- dbConnect(dbDriver("MySQL"), user=kRUser, password=kRUserPwd, dbname=kRDatabase, host=kRHost, port=kRDBPort,
                     unix.socket=socket, client.flag=CLIENT_MULTI_STATEMENTS)
db.src_mysql <- src_mysql(kRDatabase, host = kRHost, port = kRDBPort, user = kRUser, password = kRUserPwd)


get.mySQLTable <- function(db.src_mysql = NULL, tableName="tradeDeals") {
  require(dplyr)
  ## read with dplyr
  # The code with the %>%  is the long way both versions work
  # db.src_mysql <- src_mysql(dbname, host = host, port = port, user = user, password = password)
  tbl.tradeNames <- collect(tbl(db.src_mysql,tableName))
  # tbl.tradeTransactions <- db.src_mysql %>%
  #   tbl(table) %>%
  #   filter(transDealName == Trade) %>%
  #   collect()
  
  return(tbl.tradeNames)
}

tbl.tradeDeals <- get.mySQLTable(db.src_mysql = db.src_mysql, tableName = "tradeDeals")

## test case select just one field to change and keep the id field
tbl.tradeDeals2 <- tbl.tradeDeals %>% select(deal_id, ClientTelephone)
## use mutate to get rid of nas
tbl.tradeDeals2 <- tbl.tradeDeals2 %>% mutate(ClientTelephone = ifelse(is.na(ClientTelephone), '867-5309', ClientTelephone)) %>% rename(ClientTelephone2 = ClientTelephone)

## merge back
tbl.tradeDeals <- dplyr::left_join(tbl.tradeDeals, tbl.tradeDeals2, by = 'deal_id')

tbl.tradeDeals <- tbl.tradeDeals %>% mutate(ClientTelephone =  ifelse(is.na(ClientTelephone), ClientTelephone2, ClientTelephone)) %>% select(-ClientTelephone2)

## clean up charachter NA values that break dplyr and make sure numeric NAs are NAs
for(ci in colnames(tbl.tradeDeals)) {
  napos <- is.na(tbl.tradeDeals[[ci]])
  if(any(napos)) {
    if(is.numeric(tbl.tradeDeals[[ci]])) {
      tbl.tradeDeals[[ci]][napos] <- NA
    }
    if(is.character(tbl.tradeDeals[[ci]])) {
      tbl.tradeDeals[[ci]][napos] <- ''
    }
  }
}

## Drop old table and create new table
## drop old table using RMySQL's dbSendQuery
cat('+++++++++++++++++++++++++++ DROP TABLE tradeDeals +++++++++++++++++++++++\n')
dbSendQuery(conMySql, "DROP TABLE tradeDeals")

# create table with dyplyr field formats using copy_to
cat('+++++++++++++++++++++++++++ copy_to tradeDeals +++++++++++++++++++++++\n')
copy_to(db.src_mysql,tbl.tradeDeals, name="tradeDeals", temporary = FALSE)