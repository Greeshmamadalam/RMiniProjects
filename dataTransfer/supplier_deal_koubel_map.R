koubel_franscine_connect_Rmysql <- koubel_franscine_connect_Rmysql()

projectIds <- list(11049)
get.supplier <- function(projectId,koubel_franscine_connect_Rmysql){
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

tbl.supplier <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
return(tbl.supplier)
}
tbl.supplier <- bind_rows(lapply(projectIds,get.supplier))