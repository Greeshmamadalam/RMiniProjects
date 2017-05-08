library(dplyr)
library(RMySQL)
library(data.table)
source('sqlFunctions.R')
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
koubel_connect_Rmysql <- koubel_connect_Rmysql()

## c(10319,10539,10822,10563,10883,10881,11028,11032)



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


projects <-  c(10319,10539,10563,10805,10822,10881,10882,10883,11028,11032)
userQuery <- paste0("SELECT m.project_id,p.name project_name,m.name milestone_name,m.description,
                    m.planned_start_date,m.start_date,m.planned_completion_date,m.completion_date
                    FROM milestone m,project p where p.id=m.project_id;");
tbl.milestones <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
tbl.milestones <- tbl.milestones %>% filter(tbl.milestones$project_id %in% projects)
distinct_df <- unlist(tbl.milestones %>% distinct(project_id))
tbl.deals <- bind_rows(lapply(distinct_df,fill_table,tbl.milestones))

getwd()
write.csv(tbl.deals, file = "tbl.cargo_audit_project.csv",row.names=FALSE, na="")

projects <-  c(11028,11104,11049,11072,11178,11179)

tbl.Advances <- bind_rows(lapply(projects,function(projectId){
  userQuery <- paste0("SELECT
                      p.id projectId,
                      p.name ProjectName,
                      '  ' as CurrentDeal,
                      b.id SupplierID,
                      b.name SupplierName,
                      pt.paid_at TransDate,
                      m.name,
                      m.planned_completion_date expectedDateVARreceivedGoods,
                      m.completion_date actualDateVARreceivedGoods, 
                      pc.code TransCurrency,
                      pt.fx_to_usd FXforCashFlowFrankBalance,
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
                      where pa.deleted_at is null and pa.project_id=",projectId,"
                      ");
  tbl.Advances <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
  return(tbl.Advances)
}))

write.csv(tbl.Advances, file = "tbl.carl_report_advance.csv",row.names=FALSE, na="")

