require(httr)
require(dplyr)

allUsers <- get.V2UsersList()
get.DSRbadge <- function(uuid){
  url <- paste0("http://analytics-prod-proxy.cluster.kountable.com:3000/franks/profiles/",uuid,"/badges")
  cat("uuid",uuid,"\n") 
  badgedata <- GET(url) %>% content("parsed")
  tbl.DSRbadge <- data_frame(uuid = uuid,
                              profileCompleteness =ifelse(badgedata$badge1$personBadgeCriteria[[1]]$status=="FALSE",0,1),
                              mediaCompleteness = ifelse(badgedata$badge1$personBadgeCriteria[[2]]$status=="FALSE",0,1),
                              responsiveness = ifelse(badgedata$badge1$personBadgeCriteria[[3]]$status=="FALSE",0,1),
                              badge1 = ifelse(badgedata$badge1$status=="FALSE",0,1),
                              projectData = ifelse(badgedata$badge2$personBadgeCriteria[[1]]$status=="FALSE",0,1),
                              projectDocumentScore = ifelse(badgedata$badge2$personBadgeCriteria[[2]]$status=="FALSE",0,1),
                              badge2 = ifelse(badgedata$badge2$status=="FALSE",0,1),
                              completedProjects = ifelse(badgedata$badge3$personBadgeCriteria[[1]]$status=="FALSE",0,1),
                              fundedprojects = ifelse(badgedata$badge3$personBadgeCriteria[[2]]$status=="FALSE",0,1),
                              badge3 = ifelse(badgedata$badge3$status=="FALSE",0,1)
  )
  return(tbl.DSRbadge)
}
get.DSRbadgeSummary <- function(tbl.DSRbadge){
  require(dplyr)
  tbl.DSRbadgeSummary <-  tbl.DSRbadge %>% summarise(profileCompleteness=sum(profileCompleteness),
                                                     mediaCompleteness=sum(mediaCompleteness),
                                                     responsiveness=sum(responsiveness),
                                                     badge1=sum(badge1),
                                                     projectData=sum(projectData),
                                                     projectDocumentScore=sum(projectDocumentScore),
                                                     badge2=sum(badge2),
                                                     completedProjects=sum(completedProjects),
                                                     fundedprojects= sum(fundedprojects),
                                                     badge3=sum(badge3))
  return (tbl.DSRbadgeSummary)
}
names(allUsers) <- allUsers
tbl.DSRbadge <- bind_rows(lapply(allUsers,function(uuid) get.DSRbadge(uuid)))
tbl.DSRbadgeSummary <- get.DSRbadgeSummary(tbl.DSRbadge)

tbl.users <- get.users()

tbl.DSRbadge <- tbl.DSRbadge %>% filter(uuid %in% tbl.users$uuid)