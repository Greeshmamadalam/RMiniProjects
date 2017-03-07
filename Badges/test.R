con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                 password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
query <- "Select id from project where ISNULL(deleted_at) and !(state_id in (3,8)) and state_id<17;"
tbl.projectsNotDelete <- con %>% dbGetQuery(query)
query <-"select c.id,c.entity_id as projectId,c.created_by as uuid,b.user_id as participantuuid 
  from conversation as c,conversation_message as b 
  where c.id=b.conversation_id;"
tbl.conversation <- con %>% dbGetQuery(query) %>% 
  filter(participantuuid!=3810 & (projectId %in% tbl.projectsNotDelete$id)) %>% 
  mutate(response=1,participantuuid=ifelse(participantuuid!=uuid,10001,participantuuid))
tbl.conversation <- tbl.conversation %>%
  group_by(id,projectId,uuid,participantuuid) %>%
  summarise(responsivefrankKoubel=ifelse(sum(response)>=1,1,0))
tbl.conversation <- tbl.conversation %>%
  group_by(id,projectId,uuid) %>%
  summarise(responsiveCriteria=ifelse(sum(responsivefrankKoubel)>=2,1,0))
tbl.conversation <- tbl.conversation %>%
  group_by(uuid) %>%
  summarise(responsiveCriteria=ifelse(sum(responsiveCriteria)>=1,1,0))