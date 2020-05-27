library(httr)
library(jsonlite)
library(tidyverse)
library(phytools)
library(linkcomm)
library(igraph)
library(stringi)

#Create a df with all the tournaments
data_list <- list()
tourn <- paste0("https://rating.chgk.info/api/tournaments.json/?page=", c(1:7)) %>%
  map(jsonlite::fromJSON, flatten = FALSE)

#Data consist of several chunks, we merge them
for (i in 1:7){
  dat <- tourn[[i]]$items
  data_list[[i]] <- dat
}
all_tourn <- do.call(rbind, data_list)

#Drop unnecessary columns from our dataset and clean the environment
all_tourn <- subset(all_tourn, select = -c(archive, date_archived_at, 
                                           type_name, date_end, date_start))
all_tourn$idtournament <- as.numeric(all_tourn$idtournament)
rm(data_list,dat,tourn, vec,i)

#We go all in and do not set Sys.sleep(); our current API seems to be okay with that.
api_links<-paste0('https://rating.chgk.info/api/tournaments/',all_tourn[,1],"/recaps.json")
rating <- lapply(api_links,fromJSON,flatten=FALSE)

#Some of the older tournaments do not have recorded line-ups. 
#To get rid of these NA's, we note that their class differs from 
#that of the existing data - NA's are lists, not dataframes.
rating <- rating[-which(lapply(rating,class) == "list")]

#The following function takes a team line-up (a list), converts it to an edgelist,
#removes duplicates and adds player pairs to the bigger edgelist (edge_mat).
#All that is done iteratively.
#Note that the process is fairly slow, so replication may take quite some time.
edge_mat <- matrix(ncol=2,nrow=0)

for (i in 1:length(rating)){
  for (j in 1:length(pluck(rating,i,"recaps"))){
    team <- pluck(rating,i,"recaps",j,1)
    d <- to.matrix(team, seq = team)
    rownames(d)<-team
    f<-1-d
    g <- graph.adjacency(f)
    edge <- get.edgelist(g)
    new_edge <- edge.duplicates(edge)
    new_edge<-new_edge$edges
    edge_mat<-rbind(edge_mat,new_edge)
  }
}

#More duplicates are appearing when people play together more
#than once. We remove them as well, which yields a new matrix that 
#is almost five times smaller.
edge_mat_1 <- edge.duplicates(edge_mat)
edges_clear <- edge_mat_1$edges

rm(edge_mat_1,d,g,f,i,j,team,new_edge,edge)

#Now we can proceed to working with the graph itself
rating_graph <- graph_from_edgelist(edges_may, directed = FALSE)

#It would be nice to add a geographical attribute to each player, such as a city he played in
#most often recently. The site does allow us to search by "location", yet its API is unable to
#return this parameter, and scraping does not seem convenient in this case. 
#So I used a bit of social engineering and requested the data directly from the rating 
#administrator. He was eager to help, and what i got was a .csv file with all player ids 
#and their associated locations

towns <- read.csv("towns.csv", sep = ";")
summary(city$town)

n_occur <- data.frame(table(towns$idplayer))

ids <- V(rating_graph)$name
loc_data <- data.frame("id" = character(0), 
                       "city" = character(0), 
                       "country"=character(0), 
                       stringsAsFactors = FALSE)

for (i in ids){
  try({
    player<-paste0("https://rating.chgk.info/api/players/",i,"/tournaments.json") %>%
      jsonlite::fromJSON(flatten = FALSE)
    if (length(player) > 0){
      last_team <- player[[1]]$tournaments$idteam[1]
      team_json <- paste0("https://rating.chgk.info/api/teams/",last_team,".json") %>%
        jsonlite::fromJSON(flatten = FALSE)
      city <- team_json$town
      country <- team_json$country_name
      player_data <- data.frame("id" = player[[1]]$idplayer, 
                                "city" = city, 
                                "country" = country, 
                                stringsAsFactors = FALSE)
      loc_data <- rbind(loc_data, player_data)
    }else{
      next
    }
    print(paste("player",i,"done"))
  })
}

# convert ids to character for simpler matching later
loc_data$id <- as.character(loc_data$id)
# We still have around 400 accounts unlisted, which can be seen with:
ids_left <- ids[!(ids %in% loc_dat$id)]
# However, by manually checking a sample of them, we find out that these are
# either deleted accounts, or players with 0 games (who are irrelevant for our analysis 
# due to having no connections)
to_del_e <- which(V(rating_graph)$name %in% ids_left)
full_graph <- delete_vertices(rating_graph, to_del_e)

#add attributes
ungraph <- igraph::as_data_frame(full_graph, 'both')
ungraph$vertices <- ungraph$vertices %>% 
  left_join(loc_data, c('name'='id'))
the_graph <- graph_from_data_frame(ungraph$edges,
                                   directed = FALSE,
                                   vertices = ungraph$vertices)
rm(full_graph,rating_graph, ids, ids_left, n_occur, to_del_e)

#some teams (from Crimea, Abkhazia etc.) have no "country" attribute. We fix that.
V(the_graph)$country[which(V(the_graph)$country == "")] <- "NoCountry"


