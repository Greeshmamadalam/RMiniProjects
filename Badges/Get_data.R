# library(dplyr)
# library(RMySQL)

get.users <- function(){
  require(dplyr)
  require(RMySQL)
  con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                   password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
  userQuery <- "Select id as uuid from user where status_id=1;"
  tbl.users <- con %>% dbGetQuery(userQuery)
  dbDisconnect(con)
  return(tbl.users)
}
get.kdatauuid <- function(uuid){
  require(httr)
  require(dplyr)
    url <- paste0("http://analytics-prod-proxy.cluster.kountable.com:3000/franks/profiles/",uuid,"/by-projects")
    cat("uuid",uuid,"\n") 
    kdatauuid <- GET(url) %>% content("parsed")
    i=0
  if(is.null(kdatauuid$uuid) & i<2){
    url <- paste0("http://analytics-prod-proxy.cluster.kountable.com:3000/franks/profiles/",uuid,"/by-projects")
    cat("uuid",uuid,"\n") 
    kdatauuid <- GET(url) %>% content("parsed")
    i=i+1;
  }
  return(kdatauuid)
}
get.kscoredata <- function(){
  allUsers <- get.V2UsersList()
  names(allUsers) <- allUsers
  kscoredata <- lapply(allUsers, function(x) get.kdatauuid(x))
  return(kscoredata)
}
get.personaldata <- function(frankdata){
  cat(frankdata$uuid,"\n")
  tbl.personaldata <- data_frame(uuid = frankdata$uuid,
                                 profileCriteria = ifelse(is.null(frankdata$profileDataScoreV2$score),0,as.numeric(frankdata$profileDataScoreV2$score)),
                                 facebook = ifelse(is.null(frankdata$socialMediaAuthScoreV2$facebook$score),0,as.numeric(frankdata$socialMediaAuthScoreV2$facebook$score)),
                                 linkedin = ifelse(is.null(frankdata$socialMediaAuthScoreV2$linkedin$score),0,as.numeric(frankdata$socialMediaAuthScoreV2$linkedin$score)),
                                 yahoo = ifelse(is.null(frankdata$socialMediaAuthScoreV2$yahoo$score),0,as.numeric(frankdata$socialMediaAuthScoreV2$yahoo$score)),
                                 googleplus = ifelse(is.null(frankdata$socialMediaAuthScoreV2$googleplus$score),0,as.numeric(frankdata$socialMediaAuthScoreV2$googleplus$score)),
                                 twitter = ifelse(is.null(frankdata$socialMediaAuthScoreV2$twitter$score),0,as.numeric(frankdata$socialMediaAuthScoreV2$twitter$score))
  )
  return(tbl.personaldata)  
}
get.projectscore <- function(x){
  tbl.projectscore <- data_frame(projectid=x$uuid,projectscore=x$score)
  return(tbl.projectscore)
}
get.projectscoresuuid <- function(frankdata){
  tbl.projectscoresuuid <- bind_rows(lapply(frankdata$projectScoreV2$projects,function(x){
    tbl.projectscore <- data_frame(projectid=x$uuid,projectscore=x$projectDataScore$score,projectdocscore=x$projectDocumentsScore$score)
    return(tbl.projectscore)
  }))
  tbl.projectscoresuuid <- tbl.projectscoresuuid %>% mutate(uuid=as.numeric(frankdata$uuid))
  return(tbl.projectscoresuuid)
}


get.badge1data <- function(tbl.users,kscoredata){
  require(dplyr)
  require(RMySQL)
  fprofilecutoff <- 0.25*39
  profileCutoff <- 0.5*77
  mediaCutoff <- 0.5*195
  responsiveCutoff <- 5
  facebookcutoff <- 0.5*26
  linkedincutoff <- 0.5*35
  twittercutoff <- 0.5*33
  yahoocutoff <- 0.5*21
  googlepluscutoff <- 0.5*36
  socialmediaacesscutoff <- 3
  tbl.personaldata <- bind_rows(lapply(kscoredata,function(frankdata) get.personaldata(frankdata)))
  tbl.personaldata <- tbl.personaldata %>% mutate(uuid=as.numeric(uuid)) %>% filter(uuid %in% tbl.users$uuid) %>%
    mutate(facebook=ifelse(facebook>=facebookcutoff,1,0),
           linkedin=ifelse(linkedin>=linkedincutoff,1,0),
           googleplus=ifelse(googleplus>=googlepluscutoff,1,0),
           twitter=ifelse(twitter>=twittercutoff,1,0),
           yahoo=ifelse(yahoo>=yahoocutoff,1,0),
           mediaCriteria=ifelse((facebook+googleplus+twitter+linkedin+yahoo)>=socialmediaacesscutoff,1,0),
           profileCriteria =ifelse(profileCriteria >= profileCutoff, 1, 0))
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
    summarise(responsivefrankKoubel=ifelse(sum(response)>=responsiveCutoff,1,0))
  tbl.conversation <- tbl.conversation %>%
    group_by(id,projectId,uuid) %>%
    summarise(responsiveCriteria=ifelse(sum(responsivefrankKoubel)>=2,1,0))
  tbl.conversation <- tbl.conversation %>%
    group_by(uuid) %>%
    summarise(responsiveCriteria=ifelse(sum(responsiveCriteria)>=1,1,0))
  tbl.personaldata <- tbl.personaldata %>% left_join(tbl.conversation, by = "uuid") %>% 
    mutate(responsiveCriteria=ifelse(is.na(responsiveCriteria),0,responsiveCriteria)) %>% 
    select(uuid,profileCriteria,mediaCriteria,responsiveCriteria)
  tbl.frankeditprofiledata <- get.frankeditprofiledata() %>% mutate(frank_filled_profile=ifelse(totalscore>=fprofilecutoff,1,0)) %>% select(uuid,frank_filled_profile)
  tbl.personaldata <- tbl.personaldata %>% left_join(tbl.frankeditprofiledata,by='uuid')
  tbl.personaldata <- tbl.personaldata %>% 
    mutate(badge1=ifelse(frank_filled_profile==1,ifelse(mediaCriteria==1,ifelse(responsiveCriteria==1,1,0),0),0),
           one_two=ifelse(frank_filled_profile==1,ifelse(mediaCriteria==1,1,0),0),
           one_three=ifelse(frank_filled_profile==1,ifelse(responsiveCriteria==1,1,0),0),
           two_three=ifelse(mediaCriteria==1,ifelse(responsiveCriteria==1,1,0),0))
  dbDisconnect(con)
  tbl.personaldata <- tbl.personaldata %>% select(uuid,frank_filled_profile,mediaCriteria,responsiveCriteria,one_two,two_three, one_three,badge1)
  return (tbl.personaldata)
}
get.badge1summary <- function(tbl.badge_one_data){
  require(dplyr)
  tbl.Badge_summary <-  tbl.badge_one_data %>% summarise(frank_filled_profile=sum(frank_filled_profile),mediaCriteria=sum(mediaCriteria),responsiveCriteria=sum(responsiveCriteria),one_two=sum(one_two),two_three=sum(two_three),one_three=sum(one_three),badge1=sum(badge1))
  return (tbl.Badge_summary)
}
get.badge2data <- function(tbl.users,kscoredata){
  require(dplyr)
  require(RMySQL)
  projectCutoff <- 0.5*65
  maxdocscore <- 0.5*20
  tbl.projectdata <- bind_rows(lapply(kscoredata,function(frankdata) get.projectscoresuuid(frankdata)))
  tbl.projectdata <- tbl.projectdata %>% filter(uuid %in% tbl.users$uuid)
  con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                   password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
  query <- "Select id from project where ISNULL(deleted_at) and !(state_id in (3,8)) and state_id<17;"
  tbl.projectsNotDelete <- con %>% dbGetQuery(query)
  tbl.projectdata <- tbl.projectdata%>% filter(projectid %in% tbl.projectsNotDelete$id) %>%
    mutate(projectDataCriteria=ifelse(projectscore>=projectCutoff,1,0),projectDocumentCriteria=ifelse(projectdocscore==20,1,0),Badge2=ifelse(projectDataCriteria+projectDocumentCriteria==2,1,0)) %>%
    group_by(uuid) %>%summarise(projectDataCriteria=ifelse(sum(projectDataCriteria)>=1,1,0),projectDocumentCriteria=ifelse(sum(projectDocumentCriteria)>=1,1,0),Badge2=ifelse(sum(Badge2)>=1,1,0))
  tbl.badge_two_data <- tbl.users %>% left_join(tbl.projectdata, by="uuid") %>% mutate(projectDataCriteria=ifelse(is.na(projectDataCriteria),0,projectDataCriteria),projectDocumentCriteria=ifelse(is.na(projectDocumentCriteria),0,projectDocumentCriteria),Badge2=ifelse(is.na(Badge2),0,Badge2))
  dbDisconnect(con)
  return(tbl.badge_two_data)
}
get.badge2summary <- function(tbl.badge_two_data){
  require(dplyr)
  tbl.badge_two_summary <- tbl.badge_two_data %>% summarise(projectDataCriteria=sum(projectDataCriteria),projectDocumentCriteria=sum(projectDocumentCriteria),Badge2=sum(Badge2))
  return (tbl.badge_two_summary)
}
get.badge3data <- function(tbl.users){
  require(dplyr)
  require(RMySQL)
  con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                   password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
  query <- "Select p.name as project_name, p.owned_by as uuid, p.state_id from project as p where ISNULL(p.deleted_at);"
  tbl.projectStatus <- con %>% dbGetQuery(query) %>%
    mutate(legalsigned=ifelse(state_id >=17 & state_id < 24,1,0),completed=ifelse(state_id == 24,1,0)) %>%  
    group_by(uuid) %>%
    summarise(legalsigned=sum(legalsigned),completed=sum(completed))
  tbl.dealmaker <- tbl.users %>% left_join(tbl.projectStatus, by = "uuid") %>% 
    mutate(legalsigned=ifelse(is.na(legalsigned),0,legalsigned),completed=ifelse(is.na(completed),0,completed))
  tbl.dealmaker <- tbl.dealmaker %>% 
    mutate(badge3=ifelse(completed>=1 & legalsigned>=1,1,0))
  dbDisconnect(con)
  return(tbl.dealmaker)
}
get.badge3summary <- function(tbl.badge_three_data){
  require(dplyr)
  tbl.dealmaker_summary <- tbl.badge_three_data %>% summarise(legalsigned=sum(legalsigned),completed=sum(completed),badge3=sum(badge3))
  return (tbl.dealmaker_summary)
}
get.frankeditprofiledata <- function(){
  require(dplyr)
  require(RMySQL)
  con <- dbConnect(MySQL(),dbname = "kountable", user = "kdevdbuser", 
                   password = "t00rYFWOiRF", host = "prod-db.clv18hnrncxz.us-west-2.rds.amazonaws.com")
  userQuery <- "Select id as uuid,first_name,last_name,email_verified,phone_verified,country_id as country,skype,address,state,city,postal_code from user where status_id=1;"
  tbl.profile <- con %>% dbGetQuery(userQuery)
  tbl.profile <- tbl.profile %>% mutate(first_name=ifelse(first_name=='',0,2),last_name=ifelse(last_name=='',0,2),skype=ifelse(skype=='',0,2),
                                        country =ifelse(country==0,0,2),email_verified=ifelse(email_verified==0,0,10),
                                        phone_verified=ifelse(phone_verified==0,0,10),
                                        address=ifelse(address=='',0,5),city=ifelse(city=='',0,2),postal_code=ifelse(postal_code=='',0,2),
                                        state=ifelse(state=='',0,2),
                                        totalscore=first_name+last_name+skype+email_verified+phone_verified+country+address+city+postal_code+state)
  dbDisconnect(con)
  return(tbl.profile)
}

tbl.users <- get.users()
kscoredata <- get.kscoredata()
tbl.badge_one_data <- get.badge1data(tbl.users,kscoredata)
tbl.badge_one_summary <- get.badge1summary(tbl.badge_one_data)
tbl.badge_two_data <- get.badge2data(tbl.users,kscoredata)
tbl.badge_two_summary <- get.badge2summary(tbl.badge_two_data)
tbl.badge_three_data <- get.badge3data(tbl.users)
tbl.badge_three_summary <- get.badge3summary(tbl.badge_three_data)

