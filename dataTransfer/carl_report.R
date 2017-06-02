library(dplyr)
library(RMySQL)
library(data.table)
source('sqlFunctions.R')
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
koubel_connect_Rmysql <- koubel_connect_Rmysql()
# koubel_franscine_connect_Rmysql <- koubel_franscine_connect_Rmysql()

fill_table <- function(projectIds,tbl.milestones){
  tbl.deal <- data.frame(project_id=projectIds,project_name=NA,PO_contract=NA, expected_delivery_date=NA, actual_delivery_date=NA,expected_final_invoice_to_payor=NA
                          ,actual_final_invoice_to_payor=NA,due_date=NA)
  tbl.deal$project_name=tbl.milestones[tbl.milestones$project_id==projectIds,]$project_name[1]
  tbl.deal$expected_delivery_date=tbl.milestones[tbl.milestones$project_id==projectIds & tbl.milestones$milestone_name=="SME Kitting/Manufacturing/Delivery",]$planned_completion_date[1]
  tbl.deal$actual_delivery_date=tbl.milestones[tbl.milestones$project_id==projectIds & tbl.milestones$milestone_name=="SME Kitting/Manufacturing/Delivery",]$completion_date[1]
  tbl.deal$expected_final_invoice_to_payor=tbl.milestones[tbl.milestones$project_id==projectIds & tbl.milestones$milestone_name=="Payor Invoice",]$planned_completion_date[1]
  tbl.deal$actual_final_invoice_to_payor=tbl.milestones[tbl.milestones$project_id==projectIds & tbl.milestones$milestone_name=="Payor Invoice",]$completion_date[1]
  tbl.deal$due_date=tbl.milestones[tbl.milestones$project_id==projectIds & tbl.milestones$milestone_name=="Paid in full",]$planned_completion_date[1]
  return(tbl.deal)
}


projects <-  c(10319,10539,10563,10805,10822,10881,10882,10883,11028,11032,11104,11049,11072,11178,11179)

userQuery <- paste0("SELECT m.project_id,p.name project_name,m.name milestone_name,m.description,
                    m.planned_start_date,m.start_date,m.planned_completion_date,m.completion_date
                    FROM milestone m,project p where p.id=m.project_id;");
tbl.milestones <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
tbl.milestones <- tbl.milestones %>% filter(tbl.milestones$project_id %in% projects)
distinct_df <- unlist(tbl.milestones %>% distinct(project_id))
tbl.deals <- bind_rows(lapply(distinct_df,fill_table,tbl.milestones))

getwd()
write.csv(tbl.deals, file = "tbl.carl_report_project.csv",row.names=FALSE, na="")

get.advances <- function(projectId                   = 11028){
  kRsocket <- "/var/run/mysql/mysql.sock"
  kRUser <- "kdevdbuser"
  kRUserPwd <- "t00rYFWOiRF"
  kRDatabase <- "kountable"
  kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
  kRDBPort <- 3306
  conMySql_kountable     <- dbConnect(drv         = dbDriver("MySQL"), 
                                      user        = kRUser, 
                                      password    = kRUserPwd, 
                                      dbname      = kRDatabase, 
                                      host        = kRHost, 
                                      port        = kRDBPort,
                                      unix.socket = kRsocket, 
                                      client.flag = CLIENT_MULTI_STATEMENTS)
  kRsocket <- "/var/run/mysql/mysql.sock"
  kRUser <- "kdevdbuser"
  kRUserPwd <- "t00rYFWOiRF"
  kRDatabase <- "kountable_francine"
  kRHost <- "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com"
  kRDBPort <- 3306
  conMySql_kountable_francine     <- dbConnect(drv         = dbDriver("MySQL"), 
                                               user        = kRUser, 
                                               password    = kRUserPwd, 
                                               dbname      = kRDatabase, 
                                               host        = kRHost, 
                                               port        = kRDBPort,
                                               unix.socket = kRsocket, 
                                               client.flag = CLIENT_MULTI_STATEMENTS)
  userQuery <- paste0("SELECT
                      p.id projectId,
                      p.name ProjectName,
                      b.id SupplierID,
                      b.name SupplierName,
                      ps.code state,
                      pt.paid_at,
                      pay.planned_start_date PlannedTransDate,
                      m.planned_completion_date expectedDateVARreceivedGoods,
                      m.completion_date actualDateVARreceivedGoods, 
                      pc.code TransCurrency,
                      pt.fx_to_usd FXforCashFlowFrankBalance,
                      pa.amount as TotalAmount,
                      -pt.amount_total TransAmount,
                      -pt.usd_equivalent AdvFeeUSDEqv,
                      '  ' as Notes
                      FROM payable pa
                      LEFT JOIN project p ON pa.project_id= p.id
                      LEFT JOIN payment_transaction pt ON pt.payable_id=pa.id
                      LEFT JOIN currency pc ON pc.id= pa.currency_id
                      LEFT JOIN business b ON pa.business_id=b.id
                      LEFT JOIN 
                      (select m.id,m.name,m.planned_completion_date,m.completion_date,pm.payable_id from milestone m 
                      INNER JOIN payable_milestone_assoc pm ON pm.milestone_id=m.id 
                      where m.name='In Transit' and m.deleted_at is null and pm.deleted_at is null) as m ON m.payable_id=pa.id
                      LEFT JOIN 
                      (select m.id,m.name,m.planned_start_date,m.start_date,pm.payable_id from milestone m 
                      INNER JOIN payable_milestone_assoc pm ON pm.milestone_id=m.id 
                      where m.name='Payments' and m.deleted_at is null and pm.deleted_at is null) as pay ON pay.payable_id=pa.id
                      LEFT JOIN project_state ps ON p.state_id=ps.id
                      where pa.deleted_at is null and pa.project_id=",projectId,"
                      ");
  # cat('userQuery\n')
  # print(userQuery)
  tbl.Advances <- conMySql_kountable %>% dbGetQuery(userQuery)
  tbl.Advances <- tbl_df(tbl.Advances)
  tbl.status <- conMySql_kountable_francine %>% dbGetQuery("select dealOwner,koubelProjectID from tradeCFInput")
  tbl.status <- tbl_df(tbl.status) %>% mutate(koubelProjectID = as.integer(koubelProjectID))
  tbl.Advances <- dplyr::left_join(tbl.Advances,tbl.status,by=c("projectId"="koubelProjectID"))

  
  
  
    # tbl.Advances <- dplyr::left_join(tbl.Advances,tbl.status,by=c("projectId"="koubelProjectID"))
  dbDisconnect(conMySql_kountable)
  dbDisconnect(conMySql_kountable_francine)
  return(tbl.Advances)
}


projects <-  c(10319,10539,10563,10805,10822,10881,10882,10883,11028,11032,11104,11049,11072,11178,11179)

tbl.Advances <- bind_rows(lapply(projects, function(x){
                                 cat('---------------- projectId =',x,'-----------\n')
                                 get.advances(projectId = x)
}))


write.csv(tbl.Advances, file = "tbl.carl_report_advance.csv",row.names=FALSE, na="")
