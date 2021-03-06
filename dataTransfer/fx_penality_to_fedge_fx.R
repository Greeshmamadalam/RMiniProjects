library(dplyr)
library(RMySQL)
library(data.table)
# append.hedge_fx <- function(projectId){
# lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
# conKoubel <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
#                  password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
# conKoubel_francine <- dbConnect(MySQL(),dbname = "kountable_francine", user = "kdevdbuser", 
#                        password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
# userQuery <- paste0("SELECT
#                     tradePenaltyFX.dateFrom as start_date,
#                     tradePenaltyFX.dateTo as end_date,
#                     tradePenaltyFX.USDtoFXcur as usd_to_fx_cur,
#                     project_deal_association.project_id as project_id,
#                     NOW() as created_at,
#                     NOW() as updated_at,
#                     10198 as created_by,
#                     10198 as updated_by
#                     FROM
#                     tradePenaltyFX, project_deal_association
#                     where project_deal_association.deal_id=tradePenaltyFX.dealId AND project_deal_association.project_id =",projectId,"
#                     ");
# tbl.penalty_fx <- conKoubel_francine %>% dbGetQuery(userQuery)
# 
# ## use RMySQL's dbWriteTable append
# dbWriteTable(conn = conKoubel, name = 'hedged_fx', tbl.penalty_fx, append = TRUE, row.names = FALSE)
# dbDisconnect(conKoubel_francine)
# dbDisconnect(conKoubel)
# }
# 
# # projectIds <- list(169,335,581,10011,10012,10013,
# #                    10014,10016,10035,10051,
# #                    10054,10060,10071,10072,
# #                    10078,10079,10080,10081,
# #                    10083,10084,10108,10155,10194,10200,
# #                    10108,10209,10221,10223,10227,
# #                    10253,10259
# #                    )
# 
# lapply(projectIds, append.hedge_fx)
# 
# 
# 
# 
# 
# lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
# conKoubel <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
#                        password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
# conKoubel_francine <- dbConnect(MySQL(),dbname = "kountable_francine", user = "kdevdbuser", 
#                                 password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
# userQuery <- paste0("SELECT
#                     project_deal_association.project_id,
#                     tradeTransactions_copy.transDate paid_at,
#                     tradeTransactions_copy.transAmt amount_total,
#                     tradeTransactions_copy.transBankAccount bank,
#                     tradeTransactions_copy.transCurrency currency_id,
#                     tradeTransactions_copy.transDesc description,
#                     tradeTransactions_copy.transDesc2 type_id,
#                     tradeTransactions_copy.transUSDEquiv usd_equivalent,
#                     tradeTransactions_copy.transUSDEquivSpot usd_equivalent_spot,
#                     tradeTransactions_copy.transFXtoUSD fx_to_usd,
#                     tradeTransactions_copy.transFXtoUSDSpot fx_to_usd_spot,
#                     NOW() as created_at,
#                     NOW() as updated_at,
#                     10198 as created_by,
#                     10198 as updated_by
#                     FROM
#                     tradeTransactions_copy, project_deal_association
#                     where project_deal_association.deal_id=tradeTransactions_copy.transDealID AND tradeTransactions_copy.transDesc2 like '%Repayment%'
#                     ");
# tbl.repayments <- conKoubel_francine %>% dbGetQuery(userQuery)
# 
# 
# 
# tbl.repayments <- tbl.repayments %>% mutate(currency_id=ifelse(currency_id == 'USD',147,ifelse(currency_id == 'RWF',119,ifelse(currency_id == 'EUR',44,0))),
#                                             type_id=if_else(grepl('passthrough', description) & amount_total<0 , 4,if_else(grepl('Commission Paid', description) & amount_total<0 ,1,3)),
#                                             amount_total=if_else(amount_total<0,0-amount_total,amount_total),
#                                             bank_account_id=if_else(bank=='KCB RWF',10002,if_else(bank=='KCB USD',10004,if_else(bank=='KCB RWF Ops',10003,
#                                             if_else(bank=='BK RWF-37',10001,if_else(bank=='RWF-35',10006,if_else(bank=='USD-Inc 8967',10010,
#                                             if_else(bank=='RWF-10',10005,if_else(bank=='USD-39',10008,if_else(bank=='USD-Inc 5868',10009,
#                                             if_else(bank=='USD-11',10007,if_else(bank=='USD-INC 8967',10010,0))))))))))),
#                                             fx_to_usd=ifelse(is.na(fx_to_usd) | fx_to_usd<=0,fx_to_usd,1/fx_to_usd),
#                                             fx_to_usd_spot=ifelse(is.na(fx_to_usd_spot) | fx_to_usd_spot<=0,fx_to_usd_spot,1/fx_to_usd_spot))
# tbl.repayments <- tbl.repayments %>% select(-bank)
# is.numeric(tbl.repayments$fx_to_usd)
# ## use RMySQL's dbWriteTable append
# dbWriteTable(conn = conKoubel, name = 'payment_transaction', tbl.repayments, append = TRUE, row.names = FALSE)
# dbDisconnect(conKoubel_francine)
# dbDisconnect(conKoubel)
# 
# 
# 





## code for fee data transfer





lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
conKoubel <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                       password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
conKoubel_francine <- dbConnect(MySQL(),dbname = "kountable_francine", user = "kdevdbuser", 
                                password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
userQuery <- paste0("SELECT
                    project_deal_association.project_id,
                    tradeTransactions_copy.transDate paid_at,
                    tradeTransactions_copy.transAmt amount_total,
                    tradeTransactions_copy.transBankAccount bank,
                    tradeTransactions_copy.transCurrency currency_id,
                    tradeTransactions_copy.transDesc description,
                    tradeTransactions_copy.transDesc2 type_id,
                    tradeTransactions_copy.transUSDEquiv usd_equivalent,
                    tradeTransactions_copy.transUSDEquivSpot usd_equivalent_spot,
                    tradeTransactions_copy.transFXtoUSD fx_to_usd,
                    tradeTransactions_copy.transFXtoUSDSpot fx_to_usd_spot,
                    NOW() as created_at,
                    NOW() as updated_at,
                    10198 as created_by,
                    10198 as updated_by
                    FROM
                    tradeTransactions_copy, project_deal_association
                    where project_deal_association.deal_id=tradeTransactions_copy.transDealID AND tradeTransactions_copy.transDesc2 like '%Bank Fee%'
                    ");
tbl.repayments <- conKoubel_francine %>% dbGetQuery(userQuery)



tbl.repayments <- tbl.repayments %>% mutate(currency_id=ifelse(currency_id == 'USD',147,ifelse(currency_id == 'RWF',119,ifelse(currency_id == 'EUR',44,0))),
                                            type_id=1,
                                            amount_total=if_else(amount_total<0,0-amount_total,amount_total),
                                            usd_equivalent=if_else(usd_equivalent<0,0-usd_equivalent,usd_equivalent),
                                            usd_equivalent_spot=if_else(usd_equivalent_spot<0,0-usd_equivalent_spot,usd_equivalent_spot),
                                            bank_account_id=if_else(bank=='KCB RWF',10002,if_else(bank=='KCB USD',10004,if_else(bank=='KCB RWF Ops',10003,
                                                                                                                                if_else(bank=='BK RWF-37' | bank=='RWF-37',10001,if_else(bank=='RWF-35',10006,if_else(bank=='USD-Inc 8967',10010,
                                                                                                                                                                                                     if_else(bank=='RWF-10',10005,if_else(bank=='USD-39',10008,if_else(bank=='USD-Inc 5868',10009,
                                                                                                                                                                                                                                                                       if_else(bank=='USD-11',10007,if_else(bank=='USD-INC 8967',10010,0))))))))))),
                                            fx_to_usd=ifelse(is.na(fx_to_usd) | fx_to_usd<=0,fx_to_usd,1/fx_to_usd),
                                            fx_to_usd_spot=ifelse(is.na(fx_to_usd_spot) | fx_to_usd_spot<=0,fx_to_usd_spot,1/fx_to_usd_spot))
tbl.repayments <- tbl.repayments %>% select(-bank)
## use RMySQL's dbWriteTable append
dbWriteTable(conn = conKoubel, name = 'payment_transaction', tbl.repayments, append = TRUE, row.names = FALSE)
dbDisconnect(conKoubel_francine)
dbDisconnect(conKoubel)