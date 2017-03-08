write.transaction <-  function(conMySql, db.src_mysql, tbl.cashFlows){
  require(dplyr, quietly = TRUE)
  require(RMySQL, quietly = TRUE)
  ## these are the field names for the tradeCFcustomerBal table
  ## They are stored in a json blob use jsonlite package to read them
  require(jsonlite, quietly = TRUE)
  tradeCFcustomerbalNames <- fromJSON('["RecID","dealID","dealName","AdvDate","Date","RawBal","PmtOut","PmtIn","PmtInAtSpot","PmtExp","Margin","FXRes","Orig","BalRepay","MarRepay","OfeeRepay","FXRepay","CumOrig","CumMarg","CumFXRes","Total","Servicing","finalNonCashWriteOffAmount","finalOverpayment","finalDoubtfulOverpayment","nonCashWriteOffAmount","nonCashWriteOffRawBalanceAmount","nonCashWriteOffOriginationFeeAmount","nonCashWriteOffFXReserveAmount","nonCashWriteOffTradeMarginAmount","doubtfulPaymentExpected","finalNonCashDoubtfulWriteOffAmount","DoubtfulMargin","DoubtfulFXRes","DoubtfulCumMarg","DoubtfulCumFXRes","DoubtfulTotal","nonCashDoubtfulWriteOffAmount","DoubtfulMarRepay","DoubtfulFXRepay","nonCashWriteOffDoubtfulFXReserveAmount","nonCashWriteOffDoubtfulTradeMarginAmount","DoubtfulServicing","RecTime","accrualStopFlag","doubtfulAccrualFlag"]')
  colnames(tbl.cashFlows) <- tradeCFcustomerbalNames
  ## create unique record identifier index
  ## read with dplyr
  tbl.tradeCFcustomerbal <- collect(tbl(db.src_mysql,"tradeCFcustomerbal"))
  ## now get the max recID from the entire table
  tbl.cashFlows$RecID <- 1:nrow(tbl.cashFlows)
  tbl.cashFlows$RecID <- tbl.cashFlows$RecID + max(tbl.tradeCFcustomerbal$RecID)
  # ## make timezone UTC
  tbl.cashFlows$RecTime <- as.POSIXct(format(Sys.time(), tz = "UTC"))
  
  
  ## destroy existing data in tradeCFcustomerbal table for the particular dealName
  ## If no dealName exists it will not throw an error
  dbSendQuery(conn = conMySql, build_sql('DELETE FROM tradeCFcustomerbal WHERE dealName = ', tbl.cashFlows$dealName[1]))
  ## use RMySQL's dbWriteTable append
  dbWriteTable(conn = conMySql, name = 'tradeCFcustomerbal', tbl.cashFlows, append = TRUE, row.names = FALSE)
  return(tbl.cashFlows$RecID)
}