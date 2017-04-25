library(dplyr)
library(RMySQL)
library(data.table)
source('sqlFunctions.R')
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
append.hedge_fx <- function(koubel_connect_Rmysql,koubel_franscine_connect_Rmysql){

userQuery <- paste0("SELECT
                    tradePenaltyFX.dateFrom as start_date,
                    tradePenaltyFX.dateTo as end_date,
                    tradePenaltyFX.USDtoFXcur as usd_to_fx_cur,
                    project_deal_association.project_id as project_id,
                    NOW() as created_at,
                    NOW() as updated_at,
                    10198 as created_by,
                    10198 as updated_by
                    FROM
                    tradePenaltyFX, project_deal_association
                    where project_deal_association.deal_id=tradePenaltyFX.dealId
                    ");
tbl.penalty_fx <- koubel_franscine_connect_Rmysql %>% dbGetQuery(userQuery)

## use RMySQL's dbWriteTable append
dbWriteTable(conn = koubel_connect_Rmysql, name = 'hedged_fx', tbl.penalty_fx, append = TRUE, row.names = FALSE)
dbDisconnect(koubel_franscine_connect_Rmysql)
dbDisconnect(koubel_connect_Rmysql)
}
koubel_franscine_connect_Rmysql <- koubel_franscine_connect_Rmysql()
koubel_connect_Rmysql <- koubel_connect_Rmysql()
append.hedge_fx(koubel_connect_Rmysql,koubel_franscine_connect_Rmysql)