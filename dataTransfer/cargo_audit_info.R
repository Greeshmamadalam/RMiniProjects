require(dplyr)
require(RMySQL)
source('sqlFunctions.R')
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

koubel_franscine_connect_dplyr <- koubel_franscine_connect_dplyr()
koubel_connect_Rmysql <- koubel_connect_Rmysql()
koubel_connect_dplyr <- koubel_connect_dplyr()

projectIds <- list(
  169,335,581,10011,10013,
  10014,10016,10051,10060,
  10078,10083,10084,10096,
  10108,10119,10155,10194,
  10200,10203,10209,10221,
  10227,10253,10319,10539,
  10563,10564,10664,10805,
  10822,10881,10882,10883,
  10943,11003,11028,11032,
  11049,11072,11104
)
get.cargo_audit_info <- function(projectId,koubel_connect_Rmysql){
  userQuery <- paste0("SELECT
                      project.id as projectId,
                      CONCAT(project.name,',',user.first_name,',',user.last_name) as EntrepreneurOrderInfo,
                      payable.shipping_desc AS vesselConveyanceType, 
                      payable.shipping_starting_city AS insuredTripFrom,
                      payable.shipping_ending_city AS InsuredTripTo,
                      ' ' AS GoodsInsured,
                      CONCAT(payable.amount,' ',pc.code) as ShipmentValue,
                      CONCAT(payable.collateral_desc,',',payable.shipping_desc,',',payable.incoterm_desc) AS MiscInfo
                      FROM project
                      LEFT JOIN user ON user.id= project.owned_by 
                      LEFT JOIN payable ON payable.project_id= project.id 
                      LEFT JOIN currency pc ON pc.id= payable.currency_id
                      where project.id=",projectId,"
                      ");

 tbl.cargo_audit_info <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
return(tbl.cargo_audit_info)
}


tbl.cargo_audit_info <- bind_rows(lapply(projectIds,get.cargo_audit_info,koubel_connect_Rmysql))
drop_in_koubel_franscine("dw_CargoAuditInfo")
createtable(koubel_franscine_connect_dplyr,tbl.cargo_audit_info,"dw_CargoAuditInfo")

## write.csv will write to the working dir use getwd() to find out where or look in the Console window title bar
getwd()

write.csv(tbl.cargo_audit_info, file = "tbl.cargo_audit_info.csv",row.names=FALSE, na="")



