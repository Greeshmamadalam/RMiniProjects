require(dplyr)
require(RMySQL)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


get.koubeldata <- function(projectId){
  cat(projectId,"\n")
con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                 password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
userQuery <- paste0("SELECT
                    project.id AS deal_id,
                    project.`name` AS TradeName1,
                    business.`name` AS ClientBusinessName1,
                    business.contact AS ClientContactName1,
                    project.description as productDescription1,
                    project.frank_business_id as VARID1,
                    'Kountable Trading Limited' as KEntity1,
                    'Lynden John' as KSignor1,
                    '89 Nexus Way' as KBusinessStreet1,
                    'Camana Bay, Grand Cayman KY1-9007, Cayman Islands' as KBusinessCityStateCountry1,
                    project.fx_reserve/100 AS fxReserve1,
                    project.min_orig_fee AS minOrigFee1,
                    project.origination_fee_rate/100 AS origFeeRate1,
                    project.servicing_fee/100 AS servicingFee1,
                    project.trade_margin/100 AS tradeMargin1,
                    business.registration_num AS ClientBusinesRegistrationCompanyCodeTIN1,
                    business.city AS ClientPrincipalBusinessCityStateCountry1,
                    business.address AS ClientRegisteredAddress1,
                    business.contact AS ClientContact1,
                    business.phone AS ClientTelephone1,
                    project.fx_rate AS initialFXSpotRate1,
                    project.due_date AS DueDate1,
                    country.name AS FrankCountry1,
                    currency.code AS InvoiceCurrency1
                    FROM
                    project
                    LEFT JOIN project_state ON project.state_id = project_state.id
                    LEFT JOIN project_status ON project.status_id = project_status.id
                    LEFT JOIN project_type ON project.type_id = project_type.id
                    LEFT JOIN business ON project.frank_business_id = business.id
                    LEFT JOIN product ON project.product_id = product.id
                    LEFT JOIN currency ON project.invoice_currency_id = currency.id,country
                    where business.legal_registered_country_id=country.id AND project.id=",projectId,"
                    ");
tbl.koubel <- con %>% dbGetQuery(userQuery)
userQuery <- paste0("SELECT
                    business.dli_credit_rating AS ProcuringEntityTier,
                    country.name AS ProcuringEntityCountry,
                    business.`name` AS ProcuringEntity
                    FROM
                    project_business_assoc,business,country
                    where business.id = project_business_assoc.business_id AND business.legal_registered_country_id=country.id AND relationship='payor' AND project_business_assoc.project_id=",projectId," limit 1;");
tbl.payor <- con %>% dbGetQuery(userQuery)
currencyConversion <- list()
currencyConversion[44] <- 1.0526
currencyConversion[119] <- 0.00116
currencyConversion[147] <- 1
userQuery <- paste0("SELECT
                    payable.amount,
                    payable.currency_id,
                    payable.project_id
                    FROM
                    payable
                    where payable.project_id=",projectId,";");
tbl.payables <- con %>% dbGetQuery(userQuery)
tbl.payables <- tbl.payables %>%
  group_by(project_id) %>%
  summarise(amount=sum(amount*unlist(currencyConversion[currency_id])))
tbl.koubel <- tbl.koubel %>% mutate(ProcuringEntityTier1 = ifelse(exists('ProcuringEntityTier', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntityTier),0,as.numeric(tbl.payor$ProcuringEntityTier)),0),
                            ProcuringEntity1=ifelse(exists('ProcuringEntity', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntity),"",tbl.payor$ProcuringEntity),""),
                            ProcuringEntityCountry1=ifelse(exists('ProcuringEntityCountry', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntityCountry),"",tbl.payor$ProcuringEntityCountry),""),
                            payableUSD1 = ifelse(exists('amount',where=tbl.payables),tbl.payables$amount,0))
dbDisconnect(con)
return (tbl.koubel)
}
get.tradeDealsData <- function(){
con <- dbConnect(MySQL(),dbname = "kountable_francine", user = "kdevdbuser", 
                 password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
userQuery <- paste0("SELECT * from tradeDeals_copy;")
tbl.tradeDeals_copy <- con %>% dbGetQuery(userQuery)
dbDisconnect(con)
return(tbl.tradeDeals_copy)
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


projectIds <- list(169,335,581,10011,10013,
                   10014,10016,10051,10060,
                   10078,10083,10084,10096,
                   10108,10155,10194,10200,
                   10203,10209,10221,10227,
                   10253,10319,10539,10563,
                   10564,10664,10805,10822,
                   10881,10882,10883)
tbl.tradeDeals_copy1 <- bind_rows(lapply(projectIds, get.koubeldata))
tbl.tradeDeals_copy <- get.tradeDealsData()
tbl.tradeDeals_copy <- dplyr::left_join(tbl.tradeDeals_copy, tbl.tradeDeals_copy1, by = 'deal_id')
# tbl.tradeDeals_copy <- tbl.tradeDeals_copy %>% mutate(fxReserve = ifelse(is.na(fxReserve),fxReserve,fxReserve/100),
#                                                       origFeeRate =  ifelse(is.na(origFeeRate),origFeeRate,origFeeRate/100),
#                                                       servicingFee = ifelse(is.na(servicingFee),servicingFee,servicingFee/100),
#                                                       tradeMargin = ifelse(is.na(tradeMargin),tradeMargin,tradeMargin/100))
tbl.tradeDeals_copy <- tbl.tradeDeals_copy %>% mutate(TradeName = ifelse(is.na(TradeName),TradeName1,TradeName),
                                                      ClientBusinessName = ifelse(is.na(ClientBusinessName),ClientBusinessName1,ClientBusinessName),
                                                      ClientContactName = ifelse(is.na(ClientContactName),ClientContactName1,ClientContactName),
                                                      productDescription = ifelse(is.na(productDescription),productDescription1,productDescription),
                                                      VARID = ifelse(is.na(VARID),VARID1,VARID),
                                                      KEntity = ifelse(is.na(KEntity),KEntity1,KEntity),
                                                      KSignor = ifelse(is.na(KSignor),KSignor1,KSignor),
                                                      KBusinessStreet = ifelse(is.na(KBusinessStreet),KBusinessStreet1,KBusinessStreet),
                                                      KBusinessCityStateCountry = ifelse(is.na(KBusinessCityStateCountry),KBusinessCityStateCountry1,KBusinessCityStateCountry),
                                                      fxReserve = ifelse(is.na(fxReserve),fxReserve1,fxReserve),
                                                      minOrigFee = ifelse(is.na(minOrigFee),minOrigFee1,minOrigFee),
                                                      origFeeRate =  ifelse(is.na(origFeeRate),origFeeRate1,origFeeRate),
                                                      servicingFee = ifelse(is.na(servicingFee),servicingFee1,servicingFee),
                                                      tradeMargin = ifelse(is.na(tradeMargin),tradeMargin1,tradeMargin),
                                                      ClientBusinesRegistrationCompanyCodeTIN = ifelse(is.na(ClientBusinesRegistrationCompanyCodeTIN),ClientBusinesRegistrationCompanyCodeTIN1,ClientBusinesRegistrationCompanyCodeTIN),
                                                      ClientPrincipalBusinessCityStateCountry = ifelse(is.na(ClientPrincipalBusinessCityStateCountry),ClientPrincipalBusinessCityStateCountry1,ClientPrincipalBusinessCityStateCountry),
                                                      ClientRegisteredAddress = ifelse(is.na(ClientRegisteredAddress),ClientRegisteredAddress1,ClientRegisteredAddress),
                                                      ClientContact = ifelse(is.na(ClientContact),ClientContact1,ClientContact),
                                                      ClientTelephone = ifelse(is.na(ClientTelephone),ClientTelephone1,ClientTelephone),
                                                      initialFXSpotRate = ifelse(is.na(initialFXSpotRate),initialFXSpotRate1,initialFXSpotRate),
                                                      DueDate = ifelse(is.na(DueDate),DueDate1,DueDate),
                                                      FrankCountry = ifelse(is.na(FrankCountry),FrankCountry1,FrankCountry),
                                                      InvoiceCurrency = ifelse(is.na(InvoiceCurrency),InvoiceCurrency1,InvoiceCurrency),
                                                      ProcuringEntityTier = ifelse(is.na(ProcuringEntityTier),ProcuringEntityTier1,ProcuringEntityTier),                                    
                                                      ProcuringEntity=ifelse(is.na(ProcuringEntity),ProcuringEntity1,ProcuringEntity),
                                                      ProcuringEntityCountry= ifelse(is.na(ProcuringEntityCountry),ProcuringEntityCountry1,ProcuringEntityCountry),
                                                      payableUSD = ifelse(is.na(payableUSD),payableUSD1,payableUSD)) 
                                                      

tbl.tradeDeals_copy <- tbl.tradeDeals_copy %>% select(-TradeName1,-ClientBusinessName1,-ClientContactName1,-productDescription1,-VARID1,-KEntity1,
                                                      -KSignor1,-KBusinessStreet1,-KBusinessCityStateCountry1,-fxReserve1,-minOrigFee1,
                                                      -origFeeRate1,-servicingFee1,-tradeMargin1,-ClientBusinesRegistrationCompanyCodeTIN1,
                                                      -ClientPrincipalBusinessCityStateCountry1,-ClientRegisteredAddress1,-ClientContact1,
                                                      -ClientTelephone1,-initialFXSpotRate1,-DueDate1,-FrankCountry1,-InvoiceCurrency1,
                                                      -ProcuringEntityTier1,-ProcuringEntity1,-ProcuringEntityCountry1,-payableUSD1)
drop_in_koubel_franscine("tradeDeals_copy")
koubel_franscine_dplyr <- koubel_franscine_connect_dplyr()
createtable(koubel_franscine_dplyr,tbl.tradeDeals_copy,"tradeDeals_copy")
