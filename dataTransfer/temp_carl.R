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

userQuery <- "select 
project.id project_id,
project.`name`  project_name,
milestone.`name` milestone_name,
milestone.description milestone_description,
project_milestone_status.`name` as milestone_status,
milestone.start_date milestone_start_date,
milestone.completion_date milestone_completion_date,
milestone.planned_start_date,
milestone.planned_completion_date planned_completion_date,
document.id document_id,
document.name document_name
from milestone 
LEFT JOIN document_entity_assoc on milestone.id=document_entity_assoc.entity_id and entity='milestone'
LEFT JOIN document on document_entity_assoc.document_id=document.id
LEFT JOIN project on milestone.project_id=project.id
LEFT JOIN project_milestone_status on project_milestone_status.id=milestone.status_id
where project_id in (11072,11178,11179)"
# cat('userQuery\n')
# print(userQuery)
tbl.Advances <- conMySql_kountable %>% dbGetQuery(userQuery)

write.csv(tbl.Advances, file = "tbl.carl_temp.csv",row.names=FALSE, na="")