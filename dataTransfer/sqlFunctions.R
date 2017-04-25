koubel_franscine_connect_dplyr <- function(){
  kRsocket <- "/var/run/mysql/mysql.sock"
  kRUser <- "kdevdbuser"
  kRUserPwd <- "t00rYFWOiRF"
  kRDatabase <- "kountable_francine"
  kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
  kRDBPort <- 3306
  conMySql<- dbConnect(dbDriver("MySQL"), user=kRUser, password=kRUserPwd, dbname=kRDatabase, host=kRHost, port=kRDBPort,
                       unix.socket=kRsocket, client.flag=CLIENT_MULTI_STATEMENTS)
  db.src_mysql <- src_mysql(kRDatabase, host = kRHost, port = kRDBPort, user = kRUser, password = kRUserPwd)
  return (db.src_mysql)
}
koubel_connect_dplyr <- function(){
  kRsocket <- "/var/run/mysql/mysql.sock"
  kRUser <- "kdevdbuser"
  kRUserPwd <- "t00rYFWOiRF"
  kRDatabase <- "kountable"
  kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
  kRDBPort <- 3306
  conMySql<- dbConnect(dbDriver("MySQL"), user=kRUser, password=kRUserPwd, dbname=kRDatabase, host=kRHost, port=kRDBPort,
                       unix.socket=kRsocket, client.flag=CLIENT_MULTI_STATEMENTS)
  db.src_mysql <- src_mysql(kRDatabase, host = kRHost, port = kRDBPort, user = kRUser, password = kRUserPwd)
  return (db.src_mysql)
}

koubel_franscine_connect_Rmysql <- function(){
  kRsocket <- "/var/run/mysql/mysql.sock"
  kRUser <- "kdevdbuser"
  kRUserPwd <- "t00rYFWOiRF"
  kRDatabase <- "kountable_francine"
  kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
  kRDBPort <- 3306
  conMySql<- dbConnect(dbDriver("MySQL"), user=kRUser, password=kRUserPwd, dbname=kRDatabase, host=kRHost, port=kRDBPort,
                       unix.socket=kRsocket, client.flag=CLIENT_MULTI_STATEMENTS)
  return (conMySql)
}
koubel_connect_Rmysql <- function(){
  kRsocket <- "/var/run/mysql/mysql.sock"
  kRUser <- "kdevdbuser"
  kRUserPwd <- "t00rYFWOiRF"
  kRDatabase <- "kountable"
  kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
  kRDBPort <- 3306
  conMySql<- dbConnect(dbDriver("MySQL"), user=kRUser, password=kRUserPwd, dbname=kRDatabase, host=kRHost, port=kRDBPort,
                       unix.socket=kRsocket, client.flag=CLIENT_MULTI_STATEMENTS)
  return (conMySql)
}

formatTables <- function(tbl.table){
  for(ci in colnames(tbl.table)) {
    napos <- is.na(tbl.table[[ci]])
    if(any(napos)) {
      if(is.numeric(tbl.table[[ci]])) {
        tbl.table[[ci]][napos] <- NA
      }
      if(is.character(tbl.table[[ci]])) {
        tbl.table[[ci]][napos] <- ''
      }
    }
  }
  return(tbl.table)
}
drop_in_koubel_franscine <- function(tablename){
  kRsocket <- "/var/run/mysql/mysql.sock"
  kRUser <- "kdevdbuser"
  kRUserPwd <- "t00rYFWOiRF"
  kRDatabase <- "kountable_francine"
  kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
  kRDBPort <- 3306
  conMySql<- dbConnect(dbDriver("MySQL"), user=kRUser, password=kRUserPwd, dbname=kRDatabase, host=kRHost, port=kRDBPort,
                       unix.socket=kRsocket, client.flag=CLIENT_MULTI_STATEMENTS)
  
  ## Drop old table and create new table
  ## drop old table using RMySQL's dbSendQuery
  cat('+++++++++++++++++++++++++++ DROP TABLE tradeDeals +++++++++++++++++++++++\n')
  dbSendQuery(conMySql, paste0("DROP TABLE ",tablename))
}
createtable <- function(dbConnection_dplyr,tbl.table,tablename){
  # create table with dyplyr field formats using copy_to
  cat('+++++++++++++++++++++++++++ copy_to tradeDeals +++++++++++++++++++++++\n')
  copy_to(dbConnection_dplyr,formatTables(tbl.table), name=tablename, temporary = FALSE)
  
}