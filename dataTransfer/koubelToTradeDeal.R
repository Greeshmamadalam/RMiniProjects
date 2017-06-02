require(dplyr)
require(RMySQL)
source('sqlFunctions.R')
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


get.koubeldata <- function(projectId){
  cat(projectId,"\n")
con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser",
                 password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
userQuery <- paste0("SELECT
                    project.id AS deal_id,
                    project.`name` AS TradeName,
                    business.`name` AS ClientBusinessName,
                    business.contact AS ClientContactName,
                    project.description as productDescription,
                    project.frank_business_id as VARID,
                    'Kountable Trading Limited' as KEntity,
                    'Lynden John' as KSignor,
                    '89 Nexus Way' as KBusinessStreet,
                    'Camana Bay, Grand Cayman KY1-9007, Cayman Islands' as KBusinessCityStateCountry,
                    project.fx_reserve/100 AS fxReserve,
                    project.min_orig_fee AS minOrigFee,
                    project.origination_fee_rate/100 AS origFeeRate,
                    project.servicing_fee/100 AS servicingFee,
                    project.trade_margin/100 AS tradeMargin,
                    business.registration_num AS ClientBusinesRegistrationCompanyCodeTIN,
                    business.city AS ClientPrincipalBusinessCityStateCountry,
                    business.address AS ClientRegisteredAddress,
                    business.contact AS ClientContact,
                    business.phone AS ClientTelephone,
                    project.fx_rate AS initialFXSpotRate,
                    project.due_date AS DueDate,
                    country.name AS FrankCountry,
                    currency.code AS InvoiceCurrency
                      FROM
                      project
                      LEFT JOIN project_state ON project.state_id = project_state.id
                      LEFT JOIN project_status ON project.status_id = project_status.id
                      LEFT JOIN project_type ON project.type_id = project_type.id
                      LEFT JOIN business ON project.frank_business_id = business.id
                      LEFT JOIN product ON project.product_id = product.id
                      LEFT JOIN currency ON project.invoice_currency_id = currency.id
                      LEFT JOIN country ON  business.legal_registered_country_id=country.id
                      where project.id=",projectId,"
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
tbl.koubel <- tbl.koubel %>% mutate(ProcuringEntityTier = ifelse(exists('ProcuringEntityTier', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntityTier),0,as.numeric(tbl.payor$ProcuringEntityTier)),0),
                            ProcuringEntity=ifelse(exists('ProcuringEntity', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntity),"",tbl.payor$ProcuringEntity),""),
                            ProcuringEntityCountry=ifelse(exists('ProcuringEntityCountry', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntityCountry),"",tbl.payor$ProcuringEntityCountry),""),
                            payableUSD= ifelse(exists('amount',where=tbl.payables),tbl.payables$amount,0))
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

## add project Ids to generate tradedeals fields for new projects   (do not remove project ids from the below list unless there is a duplicate)
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
  11049,11072,11098,11104,
  11139,11186,11235,11243
)
tbl.dw_tradeDeals <- bind_rows(lapply(projectIds, get.koubeldata))

drop_in_koubel_franscine("dw_tradeDeals")
koubel_franscine_dplyr <- koubel_franscine_connect_dplyr()
createtable(koubel_franscine_dplyr,tbl.dw_tradeDeals,"dw_tradeDeals")
