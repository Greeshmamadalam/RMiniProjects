library(dplyr)
library(RMySQL)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
source('sqlFunctions.R')
koubel_connect_Rmysql <- koubel_connect_Rmysql()
get.transactions <- function(koubel_connect_Rmysql){
  
userQuery <- paste0("SELECT pt.*,p.name,py.business_id
                    FROM payment_transaction as pt 
                    Left join project as p on pt.project_id=p.id
                    Left join payable as py on pt.payable_id=py.id 
                    where pt.deleted_at is null;");
  tbl.transactions <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
  tbl.transactions <- tbl.transactions %>% mutate(supplierId= business_id,
                                                  transCurrency=ifelse(currency_id == 147,'USD',ifelse(currency_id == 119,'RWF',ifelse(currency_id == 44,'EUR',0))),
                                                  transBankAccount=if_else(bank_account_id==10002,'KCB RWF',if_else(bank_account_id==10004,'KCB USD',if_else(bank_account_id==10003,'KCB RWF Ops',
                                                                          if_else(bank_account_id==10001,'BK RWF-37',if_else(bank_account_id==10006,'RWF-35',if_else(bank_account_id==10010,'USD-Inc 8967',
                                                                          if_else(bank_account_id==10005,'RWF-10',if_else(bank_account_id==10008,'USD-39',if_else(bank_account_id==10009,'USD-Inc 5868',
                                                                          if_else(bank_account_id==10007,'USD-11',if_else(bank_account_id==10010,'USD-INC 8967',if_else(bank_account_id==10011,'WF-INC 21',
                                                                          if_else(bank_account_id==10012,'C-KTL','none'))))))))))))),
                                                  fx_to_usd=ifelse(is.na(fx_to_usd) | fx_to_usd<=0,fx_to_usd,1/fx_to_usd),
                                                  fx_to_usd_spot=ifelse(is.na(fx_to_usd_spot) | fx_to_usd_spot<=0,fx_to_usd_spot,1/fx_to_usd_spot),
                                                  transDesc2=if_else(type_id== 2,'Advance',if_else(type_id== 3,'Repayment',if_else(type_id==1,'Fee',if_else(type_id==4,'Advance','none')))),
                                                  transDate=paid_at,
                                                  transDesc=description,
                                                  transAmt=amount_total,
                                                  transUSDEquiv=usd_equivalent,
                                                  transUSDEquivSpot=usd_equivalent_spot,
                                                  transFXtoUSD=fx_to_usd,
                                                  transFXtoUSDSpot=fx_to_usd_spot,
                                                  transDealName=name,
                                                  insertDate=created_at,
                                                  updateDate=updated_at
  )
  tbl.payables_done <-  tbl.transactions %>% distinct(payable_id) %>% filter(!is.na(payable_id))
  userQuery <- paste0("SELECT pa.*,p.name as transDealName from payable pa,project p where p.id=pa.project_id and pa.deleted_at is null;")
  tbl.payables <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
  tbl.payables <- tbl.payables %>% filter(! (id %in% tbl.payables_done$payable_id) )
  tbl.payables <- tbl.payables %>% mutate(transAmt=amount,
                                          transCurrency=ifelse(currency_id == 147,'USD',ifelse(currency_id == 119,'RWF',ifelse(currency_id == 44,'EUR',0))),
                                          supplierId=business_id,
                                          transDesc2='payables',
                                          transDate=due_date,
                                          insertDate=created_at,
                                          updateDate=updated_at
  )
  tbl.payables <- tbl.payables %>% select(-id,-deleted_at,-amount,-currency_id,-business_id,-due_date,-created_at,-updated_at,-bank_account_name,-bank_account_type_id,-account_contact_name,-account_bank_country_id,
                                          -account_number,-account_routing_number,-account_swift_code,-account_contact_email,-collateral_rating,-collateral_desc,-incoterm_desc,
                                          -shipping_company,-shipping_desc,-shipping_starting_city,-shipping_starting_city,-shipping_starting_country_id,-shipping_ending_country_id,
                                          -shipping_ending_city,-importer_of_record,-clearing_agent_name)
  tbl.transactions <- tbl.transactions %>% select(-bank_account_id,-currency_id,-type_id,-paid_at,-description,-amount_total,-usd_equivalent,-usd_equivalent_spot,
                                                  -deleted_at,-fx_to_usd,-fx_to_usd_spot,-created_at,-updated_at,-name,-business_id,-payable_id,-known_usd_equivalent,-known_currency_to_usd) 
  tbl.transactions <-  bind_rows(list(a = tbl.transactions, b = tbl.payables))
  return(tbl.transactions)
}


tbl.transactions <- get.transactions(koubel_connect_Rmysql)
drop_in_koubel_franscine("dw_tradeTransactions_koubel")
koubel_franscine_dplyr <- koubel_franscine_connect_dplyr()
createtable(koubel_franscine_dplyr,tbl.transactions,"dw_tradeTransactions_koubel")