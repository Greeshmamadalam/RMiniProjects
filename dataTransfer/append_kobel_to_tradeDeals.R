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
                                      ProcuringEntityCountry=ifelse(exists('ProcuringEntityCountry', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntityCountry),"",tbl.payor$ProcuringEntityCountry),""))
  dbDisconnect(con)
  return (tbl.koubel)
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


projectIds <- list(10943,11028)

tbl.rows_project <- bind_rows(lapply(projectIds, get.koubeldata))
conKoubel_francine <- dbConnect(MySQL(),dbname = "kountable_francine", user = "kdevdbuser", 
                                password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
dbWriteTable(conn = conKoubel_francine, name = 'tradeDeals', tbl.rows_project , append = TRUE, row.names = FALSE)
dbDisconnect(conKoubel_francine)



