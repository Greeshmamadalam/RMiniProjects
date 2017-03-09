
con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                 password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
responsiveCutoff <- 5
query <- "Select id from project where ISNULL(deleted_at);"
tbl.projectsNotDelete <- con %>% dbGetQuery(query)
query <-"select c.id,c.entity_id as projectId,p.owned_by as uuid,b.user_id as participantuuid 
  from conversation as c,conversation_message as b, project as p 
  where p.id=c.entity_id and c.id=b.conversation_id;"
tbl.conversation <- con %>% dbGetQuery(query) %>% 
  filter(participantuuid!=3810 & (projectId %in% tbl.projectsNotDelete$id)) %>% 
  mutate(response=1,participantuuid=ifelse(participantuuid!=uuid,10001,participantuuid))
tbl.conversation <- tbl.conversation %>%
  group_by(id,projectId,uuid,participantuuid) %>%
  summarise(responsivefrankKoubel=ifelse(sum(response)>=responsiveCutoff,1,0))
tbl.conversation <- tbl.conversation %>%
  group_by(id,projectId,uuid) %>%
  summarise(responsiveCriteria=ifelse(sum(responsivefrankKoubel)>=2,1,0))
tbl.conversation <- tbl.conversation %>%
  group_by(uuid) %>%
  summarise(responsiveCriteria=ifelse(sum(responsiveCriteria)>=1,1,0))
tbl.conversation <- tbl.conversation %>% mutate (uuid=as.numeric(uuid))
