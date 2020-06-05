
V(the_graph)$country[which(V(the_graph)$country == "")] <- "NoCountry"
V(the_graph)$country[which(V(the_graph)$country == "~")] <- "NoCountry"

#Network parameters
diameter(the_graph)
centr_degree(the_graph)$centralization
centr_clo(the_graph, mode = "all")$centralization
centr_betw(the_graph, directed = FALSE)$centralization
centr_eigen(the_graph, directed = FALSE)$centralization
transitivity(the_graph)
mean_distance(the_graph)
mean(degree(the_graph))

#Components
count_components(the_graph)
components(the_graph)$csize %>% max()
components(the_graph)$csize[2:8224] %>% mean()
components(the_graph)$csize[2:8224] %>% median()
components(the_graph)$csize[2:8224] %>% sort() %>% 
  barplot(main = "Component size distribution without the largest one", 
       ylab = "Component size")
#       pch = 16, cex = 0.5)

#Degree
degdis <- degree_distribution(the_graph)
deg_freq <- degree(the_graph) %>% table() %>% as.data.frame()
median(deg_freq$Freq)
poi <- c(485,487,490,495,498,
         501,517,527,531,537,558,574,580,606,610,
         652,663,667,671,683,686,809,840)
plot(degdis[1:484], main = "Degree Distribution",
     xlim = c(0,850),
     xlab = "Degree",
     ylab = "Frequency",
     type = "l")
points(x = poi,y= c(rep(0.0001,length(poi))), pch = 16,cex=0.8, col = "blue")
degree(the_graph) %>% sort() %>% plot()

# Three more faulty edges discovered, we remove them
the_graph <- delete.vertices(the_graph,which(degree(the_graph) == 0))

#influentials
which(degree(m,V(m)) > 100) %>% length()
which(degree(m,V(m)) > 200) %>% length()

top_names<-degree(m) %>% sort(decreasing = TRUE) %>% names()
top_df <- V(m)[which(V(m)$name %in% top_names[1:100])]$city %>%
            table() %>%
            as.data.frame(colnames=(c("city","freq")))
knn(m,V(m)[which(V(m)$name %in% top_names[1:200])])$knn %>% mean()
knn(m,V(m)[which(V(m)$name %in% top_names[1:100])])$knn %>% median()

library(plotly)
plot_ly(top_df, labels = ~., values = ~Freq, 
        type = 'pie',textposition = 'outside',
        textinfo = 'label+percent', rotation = 75) %>%
  layout(title = NA, showlegend=FALSE, margin = list(b = 130, l=100),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Compare to random
set.seed(1729)
p_erdos <- erdos.renyi.game(n = 203116, p.or.m = 0.00006, type = "gnp", loops = FALSE)
transitivity(p_erdos, type = "average")
mean(degree(p_erdos))

#Small world 
ss <- sample_smallworld(1, 203116, 6, 0.05, loops = FALSE, multiple = FALSE)
deg_freq <- degree(ss) %>% table() %>% as.data.frame()
transitivity(ss, type = "average")

# Main component
large_comps <- decompose(the_graph, min.vertices = 1000)
main_component <- large_comps[[1]]
rm(large_comps)
m<-main_component #these will be used interchangeably; 
# shorter name is useful in lengthy functions/queries

m <- set.vertex.attribute(m,"weight",index=V(m),1)
m <- set.edge.attribute(m,"weight",index=E(m),1)

V(g2)$country <- gsub('ё','e',gsub('э','е',V(g2)$country))
V(g2)$country <- gsub("ь","",gsub('Ё','E',V(g2)$country))
V(g2)$country <- gsub('ъ','',V(g2)$country)
V(g2)$country <- stri_trans_general(V(g2)$country, "russian-latin/bgn")

V(m)$city <- gsub('ё','e',gsub('э','е',V(m)$city))
V(m)$city <- gsub("ь","",gsub('Ё','E',V(m)$city))
V(m)$city <- gsub('ъ','',V(m)$city)
V(m)$city <- stri_trans_general(V(m)$city, "russian-latin/bgn")

centr_degree(main_component)$centralization
centr_clo(main_component, mode = "all")$centralization
centr_betw(main_component, directed = FALSE)$centralization
centr_eigen(main_component, directed = FALSE)$centralization
transitivity(main_component)
transitivity(main_component, type = "average")
mean_distance(main_component)
mean(degree(main_component))

fastgreedy<=cluster_fast_greedy(main_component)
infomap<-cluster_infomap(main_component)
louvain<-cluster_louvain(main_component)
labelprop<-cluster_label_prop(main_component)
spinglass<-cluster_spinglass(main_component, gamma = 0.9)
eigencluster<-cluster_leading_eigen(main_component)

#compare clusters
compare(louvain, spinglass, method = "nmi")

# visualize spinglass 
mmode <- function(i) {
  uniq <- unique(i)
  uniq[which.max(tabulate(match(i, uniq)))]
}

mspin <- contract.vertices(m,membership(spinglass),
                           vertex.attr.comb=list(city=function(x) mmode(x),
                                                 country=function(x) mmode(x),
                                                 weight="sum",
                                                 "ignore"))
mspin<-simplify(mspin, edge.attr.comb = list(weight="sum"))
degree(mspin)
E(mspin)$weight

# plot it
set.seed(1)
png('mspin.png', width = 3000, height=2400)
plot (mspin, vertex.size = V(mspin)$weight/1500,
        vertex.label=V(mspin)$city, edge.width = V(mspin)$weight/500,
        layout = layout_nicely, vertex.label.cex = 4, asp=9/16, 
        vertex.color = "darkolivegreen2")
dev.off()

tashkent <- induced.subgraph(m,V(m)[which(membership(spinglass) == 1)])

mspin40 <- contract.vertices(m,membership(spin1),
                           vertex.attr.comb=list(city=function(x) mmode(x),
                                                 country=function(x) mmode(x),
                                                 weight="sum",
                                                 "ignore"))
mspin40<-simplify(mspin40, edge.attr.comb = list(weight="sum"))
V(mspin40)$weight
E(mspin40)$weight

set.seed(1)
png('mspin40.png', width = 4000, height=3000)
plot (mspin40, vertex.size = V(mspin40)$weight/2000,
      vertex.label=V(mspin40)$city, 
      edge.width = ifelse(V(mspin40)$weight >1000, V(mspin40)$weight/500, V(mspin40)$weight/100),
      layout = layout_nicely, 
      vertex.label.cex = 3, 
      asp=9/16, 
      vertex.color = "darkolivegreen2",
      edge.color = ifelse(E(mspin40)$weight > 100, "tomato", "lightblue2"))
dev.off()

# spinglass with 100 states
mspin100 <- contract.vertices(m,membership(spin100),
                             vertex.attr.comb=list(city=function(x) mmode(x),
                                                   country=function(x) mmode(x),
                                                   weight="sum",
                                                   "ignore"))
mspin100<-simplify(mspin100, edge.attr.comb = list(weight="sum"))
V(mspin100)$weight
E(mspin100)$weight

set.seed(1)
png('mspin100.png', width = 4000, height=3000)
plot (mspin100, vertex.size = V(mspin100)$weight/2000,
      vertex.label=V(mspin100)$city, 
      edge.width = ifelse(V(mspin100)$weight > 100, V(mspin40)$weight/1000, V(mspin40)$weight/100),
      layout = layout_nicely, 
      vertex.label.cex = 2, 
      asp=9/16, 
      vertex.color = "darkolivegreen2",
      edge.color = ifelse(E(mspin100)$weight > 100, "tomato", "lightblue2"))
dev.off()

#Louvain
mlou <- contract.vertices(m,membership(louvain),
                           vertex.attr.comb=list(city=function(x) mmode(x),
                                                 country=function(x) mmode(x),
                                                 weight="sum",
                                                 "ignore"))
mlou<-simplify(mlou, edge.attr.comb = list(weight="sum"))

set.seed(1000)
png('mlou1.png', width = 10000, height=8000)
plot (mlou, vertex.size = ifelse(V(mlou)$weight>100, E(mlou)$weight/1000, 
                                 E(mlou)$weight/100),
      vertex.label=(NA), 
      edge.width = ifelse(E(mlou)$weight>100, E(mlou)$weight/300, E(mlou)$weight/10),
      layout = layout_nicely, vertex.label.cex = 2, asp=9/16, 
      vertex.color = "darkblue", vertex.frame.color = "floralwhite")
dev.off()

set.seed(300)
png('loulou12.png', width = 4000, height=3000)
plot (mlou, 
      vertex.size = ifelse(V(mlou)$weight>100,V(mlou)$weight/3000,V(mlou)$weight/400),
      vertex.label=ifelse(V(mlou)$weight>2000, V(mlou)$city, NA),
      vertex.color=ifelse(V(mlou)$weight>2000, "deepskyblue", "bisque"),
      edge.width = E(mlou)$weight/500,
      layout = layout_with_graphopt(mlou,charge = 0.4),
      vertex.label.cex = 4, label.degree = pi/2, label.dist = 1,
      asp=9/16)
dev.off()

louv_summary <- data.frame("city" = character(0), 
                       "count" = numeric(0))
louvain_big <- sort(V(mlou)$weight, decreasing = TRUE)[1:15]
for (i in louvain_big){
  city_player <- data.frame("city" = V(mlou)[which(V(mlou)$weight == i)]$city, 
                            "count" = i, stringsAsFactors = FALSE)
  louv_summary<-rbind(louv_summary,city_player)
}

city_freq <- V(main_component)$city %>% table() %>% as.data.frame()
onefifteen<-city_freq[order(city_freq$Freq, decreasing = TRUE),][1:15,]
louvain_comp<-cbind(louv_summary,onefifteen)
colnames(louvain_comp) <- c("City Louvain", "Louvain Count", "City Real", "Real Count")
library(xtable)
louv_table <- xtable(louvain_comp)
print(louv_table, floating=FALSE, type="latex")

#almost all superconnectors are there!

#membership of the largest clusters - pick the largest from l
l<-louvain$membership %>% table() %>% as.data.frame()
lm<-V(m)[degree(m, louvain[[297]]) %>% sort(decreasing = TRUE) %>% names()]$city
ld <- lm %>% table() %>% as.data.frame()

tab <- xtable(head(ld[order(ld$Freq, decreasing = TRUE),]))
print(tab, floating=TRUE, type="latex")

# walktrap

mwalk <- contract.vertices(m,membership(walk),
                          vertex.attr.comb=list(city=function(x) mmode(x),
                                                country=function(x) mmode(x),
                                                weight="sum",
                                                "ignore"))
mwalk<-simplify(mwalk, edge.attr.comb = list(weight="sum"))
walk2 <- cluster_walktrap(mwalk)

#infoMAP
mmap <- contract.vertices(m,membership(infomap),
                           vertex.attr.comb=list(city=function(x) mmode(x),
                                                 country=function(x) mmode(x),
                                                 weight="sum",
                                                 "ignore"))
mmap<-simplify(mmap, edge.attr.comb = list(weight="sum"))

map_second <- cluster_walktrap(mmap)

mmap2 <- contract.vertices(mmap,membership(map_second),
                          vertex.attr.comb=list(city=function(x) mmode(x),
                                                country=function(x) mmode(x),
                                                weight="sum",
                                                "ignore"))
mmap2<-simplify(mmap2, edge.attr.comb = list(weight="sum"))
set.seed(300)
png('loulou1.png', width = 4000, height=3000)
plot (mlou, 
      vertex.size = ifelse(V(mlou)$weight>100,V(mlou)$weight/3000,V(mlou)$weight/400),
      vertex.label=ifelse(V(mlou)$weight>2000, V(mlou)$city, NA),
      vertex.color=ifelse(V(mlou)$weight>2000, "deepskyblue", "bisque"),
      edge.width = E(mlou)$weight/500,
      layout = layout_with_graphopt(mlou,charge = 0.21),
      vertex.label.cex = 2,
      asp=9/16)
dev.off()

#label propagation
lapro2 <- contract.vertices(m,membership(labelprop),
                           vertex.attr.comb=list(city=function(x) mmode(x),
                                                 country=function(x) mmode(x),
                                                 weight="sum",
                                                 "ignore"))
lapro2<-simplify(lapro2, edge.attr.comb = list(weight="sum"))

lapro2_comms <- cluster_label_prop(lapro2)
lapro3 <- contract.vertices(lapro2,membership(lapro2_comms),
                            vertex.attr.comb=list(city=function(x) mmode(x),
                                                  country=function(x) mmode(x),
                                                  weight="sum",
                                                  "ignore"))
lapro3<-simplify(lapro3, edge.attr.comb = list(weight="sum"))


#fastgreedy
fastgr <- contract.vertices(m,membership(fastgreedy),
                            vertex.attr.comb=list(city=function(x) mmode(x),
                                                  country=function(x) mmode(x),
                                                  weight="sum",
                                                  "ignore"))
fastgr<-simplify(fastgr, edge.attr.comb = list(weight="sum"))
set.seed(191)
png('fastgreedy1.png', width = 4000, height=3000)
plot (fastgr, 
      vertex.size = ifelse(V(fastgr)$weight>3000,V(fastgr)$weight/2000,V(fastgr)$weight/500),
      vertex.label=ifelse(V(fastgr)$weight>800, V(fastgr)$city, NA),
      vertex.color=ifelse(V(fastgr)$weight>800, "deepskyblue", "bisque"),
      edge.width = E(fastgr)$weight/400,
      layout = layout_with_graphopt(fastgr,charge = 0.8),
      vertex.label.cex = 5,
      asp=9/16)
dev.off()
