require(dplyr)
require(RMySQL)
source('sqlFunctions.R')

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

get.dli_collaterial_data <- function(projectId                   = 11028){
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
                      project.id AS project_id,
                      project.`name` AS projectName,
                      project.frank_business_id as VARIdentifier,
                      country.name AS VARCountry
                      FROM
                      project
                      LEFT JOIN business ON project.frank_business_id = business.id
                      LEFT JOIN country ON  business.legal_registered_country_id=country.id
                      where project.id=",projectId,"
                      ");
  
  tbl.dli_collaterial_data <- conMySql_kountable %>% dbGetQuery(userQuery)
  tbl.dli_collaterial_data <- tbl_df(tbl.dli_collaterial_data)
  tbl.status <- conMySql_kountable_francine %>% dbGetQuery("select dealOwner,koubelProjectID from tradeCFInput")
  tbl.status <- tbl_df(tbl.status) %>% mutate(koubelProjectID = as.integer(koubelProjectID))
  tbl.dli_collaterial_data <- dplyr::left_join(tbl.dli_collaterial_data,tbl.status,by=c("project_id"="koubelProjectID"))
  userQuery <- paste0("SELECT
                      country.name AS ProcuringEntityCountry,
                      business.`name` AS ProcuringEntity
                      FROM
                      project_business_assoc,business,country
                      where business.id = project_business_assoc.business_id AND business.legal_registered_country_id=country.id AND relationship='payor' AND project_business_assoc.project_id=",projectId," limit 1;");
  tbl.payor <- conMySql_kountable %>% dbGetQuery(userQuery)
  
  tbl.dli_collaterial_data <- tbl.dli_collaterial_data %>% mutate(
                                      ProcuringEntity=ifelse(exists('ProcuringEntity', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntity),"",tbl.payor$ProcuringEntity),""),
                                      ProcuringEntityCountry=ifelse(exists('ProcuringEntityCountry', where =tbl.payor),ifelse(is.na(tbl.payor$ProcuringEntityCountry),"",tbl.payor$ProcuringEntityCountry),""))
  
  dbDisconnect(conMySql_kountable)
  dbDisconnect(conMySql_kountable_francine)
  return(tbl.dli_collaterial_data)
}


projects <-  c(10319,10539,10563,10805,10822,10881,10882,10883,11028,11032,11049,11072,10664,10943,10564,11003)

tbl.dli_collaterial_data <- bind_rows(lapply(projects, function(x){
                                 cat('---------------- projectId =',x,'-----------\n')
                        get.dli_collaterial_data(projectId = x)
}))





 
getwd()
write.csv(tbl.dli_collaterial_data, file = "dli_collaterial_data.csv",row.names=FALSE, na="")



