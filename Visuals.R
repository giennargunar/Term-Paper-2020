
# plot the whole graph
set.seed(1000)
png('exper2.png', width = 12000, height=9000)
plot (the_graph,
      vertex.size = 0.007,
      vertex.label=NA,
      vertex.color="navy",
      edge.width = 0.01,
      layout = layout_nicely,
      asp=9/16)
dev.off()

# plot network contracted by cities
# c_m is created in CountrySelector
c_m <- set.vertex.attribute(c_m,"weight",index=V(c_m),1)
c_m <- set.edge.attribute(c_m,"weight",index=E(c_m),1)

c_m_city <- contract.vertices(c_m,
                              mapping=as.integer(as.factor(V(c_m)$city)),
                              vertex.attr.comb=list(city="first",country="first",weight="sum", "ignore"))
c_m_city<-simplify(c_m_city, edge.attr.comb = list(weight="sum", "ignore"))

set.seed(10000)
png('citygraph.png', width = 6000, height=4000)
plot (c_m_city, layout=layout_with_graphopt(c_m_city, charge = 0.7), 
      vertex.size = ifelse( V(c_m_city)$city == "Moskva",10, ifelse(
        V(c_m_city)$weight>300, V(c_m_city)$weight/1800, V(c_m_city)$weight/300)),
      edge.width = E(c_m_city)$weight/250,
      vertex.label=ifelse(V(c_m_city)$weight>1000, V(c_m_city)$city, NA),
      vertex.color = ifelse(V(c_m_city)$city == "Moskva", "forestgreen",
                            ifelse(V(c_m_city)$weight>1000,"tomato","steelblue1")), 
      vertex.label.cex = 4, vertex.label.color = "black", asp=9/16)
dev.off() 
count_triangles(c_m_city, V(c_m_city)) %>% sort()

# plot network contracted by countries

g2 <- contract.vertices(c_m,
                        mapping=as.integer(as.factor(V(c_m)$country)),
                        vertex.attr.comb=list(country="first",weight="sum", "ignore"))
g2<-igraph::simplify(g2, edge.attr.comb = "sum") 
g23 <- delete.vertices(g2,which(V(g2)$country == "Россия"))

layoutcou = layout.star(g2, center=V(g2)[32])
set.seed(10000)
png('countrygraph.png', width = 4000, height=3000)
plot (g2, layout=layoutcou, 
      vertex.size = ifelse(V(g2)$country == "Rossiya", 20, ifelse(
        V(g2)$weight>1000, V(g2)$weight/2000, V(g2)$weight/200)),
      edge.width = E(g2)$weight/100,
      vertex.label=V(g2)$country,
      vertex.color = ifelse(V(g2)$country == "Rossiya", "forestgreen",
                            ifelse(V(g2)$weight>1000,"tomato","steelblue1")), 
      vertex.label.cex = 4, vertex.label.color = "black", asp=9/16)
dev.off() 


# plot american community 

uscan <- induced.subgraph(the_graph,
                         which(V(the_graph)$country == "США"|
                                 V(the_graph)$country == "Канада"))
V(uscan)$color = ifelse(V(uscan)$country == "США", "blue","red")
uscan<-set_vertex_attr(uscan, "weight",index=V(uscan),1)
uscan<-set.edge.attribute(uscan,"weight",index=E(uscan),1)
set.seed(1000)
png('uscandots.png', width = 7000, height=5000)
plot (uscan, vertex.size = 1,, edge.width = 0.09, vertex.label = V(uscan)$city,
      layout = layout_nicely, asp=9/16)
dev.off()

uscan2 <- contract.vertices(uscan,
                           mapping=as.integer(as.factor(V(uscan)$city)),
                           vertex.attr.comb=list(city="first", weight="sum","ignore")) 
uscan2<-simplify(uscan2, edge.attr.comb = list(weight="sum"))

set.seed(10000)
png('uscancity.png', width = 1200, height=800)
plot (uscan2, vertex.size = V(uscan2)$weight/70, rescale = TRUE,
      vertex.label=V(uscan2)$city, edge.width = V(uscan2)$weight/50,
      layout = layout.circle, vertex.label.cex = 2, asp=9/16)
dev.off()

install.packages("devtools")
library(devtools)
install.packages("remotes")
remotes::install_github("wjrl/RBioFabric")

library(RBioFabric)
library(stringi)
V(uscan2)$name <- V(uscan2)$city

height <- vcount(uscan2)
width <- ecount(uscan2)
aspect <- height / width;
plotWidth <- 100.0
plotHeight <- plotWidth * (aspect * 1.2)
pdf("uscanOutput.pdf", width=plotWidth, height=plotHeight)
bioFabric(uscan2)
dev.off()


mmode <- function(i) {
uniq <- unique(i)
uniq[which.max(tabulate(match(i, uniq)))]
}

#Asia
asia <- induced.subgraph(the_graph,which(V(the_graph)$country == "Uzbekistan"|
                                        V(the_graph)$country == "Azerbaydzhan"|
                                        V(the_graph)$country == "Armeniya"|
                                        V(the_graph)$country == "Turkmenistan"|
                                        V(the_graph)$country == "Tadzhikistan"|
                                        V(the_graph)$country == "Kyrgyzstan"|
                                        V(the_graph)$country == "Kazakhstan"|
                                        V(the_graph)$country == "Turtsiya"|
                                        V(the_graph)$country == "Gruziya"|
                                        V(the_graph)$country == "OAE"|
                                        V(the_graph)$country == "Yaponiya"|
                                        V(the_graph)$country == "Kitay"| 
                                        V(the_graph)$country == "Singapur"))
asia <- set.vertex.attribute(asia,"weight",index=V(asia),1)
asia <- set.edge.attribute(asia,"weight",index=E(asia),1)
asia2 <- contract.vertices(asia,
                           mapping=as.integer(as.factor(V(asia)$city)),
                           vertex.attr.comb=list(city="first", 
                                                 weight="sum",
                                                 country="first", 
                                                 "ignore")) 
asia2<-simplify(asia2, edge.attr.comb = list(weight="sum", "ignore"))

set.seed(1000)
png('cenasia111.png', width = 4000, height=3000)
plot (asia, vertex.size = 0.8,
      vertex.label=NA, edge.width = 0.1,
      layout = layout_nicely, asp=9/16, 
      vertex.color = pal[as.numeric(as.factor(V(asia)$country))])
dev.off()

library(RColorBrewer)
pal <- brewer.pal(length(unique(V(asia2)$country)), "Paired")
set.seed(3)
png('cenasia.png', width = 2000, height=1500)
plot (asia2, vertex.size = 2,
      vertex.label=V(asia2)$city, edge.width = E(asia2)$weight/25,
      layout = layout.fruchterman.reingold, asp=9/16, 
      vertex.label.cex = 1,vertex.label.color = "black",
      vertex.color = pal[as.numeric(as.factor(V(asia2)$country))])
dev.off()

#israel
isr<-induced_subgraph(the_graph, V(the_graph)[which(V(the_graph)$country == "Izrail")])
isr <- set.vertex.attribute(isr,"weight",index=V(isr),1)
isr <- set.edge.attribute(isr,"weight",index=E(isr),1)
isr2 <- contract.vertices(isr,
                          mapping=as.integer(as.factor(V(isr)$city)),
                          vertex.attr.comb=list(city="first", 
                                                weight="sum",
                                                country="first", 
                                                "ignore")) 
isr2<-simplify(isr2, edge.attr.comb = list(weight="sum", "ignore"))


#baltic
balt<-induced_subgraph(the_graph, V(the_graph)[which(V(the_graph)$country == "Estoniya"|
                                                      V(the_graph)$country == "Latviya"|
                                                      V(the_graph)$country == "Litva")])
balt <- set.vertex.attribute(balt,"weight",index=V(balt),1)
balt <- set.edge.attribute(balt,"weight",index=E(balt),1)
balt2 <- contract.vertices(balt,
                           mapping=as.integer(as.factor(V(balt)$city)),
                           vertex.attr.comb=list(city="first", 
                                                 weight="sum",
                                                 country="first", 
                                                 "ignore")) 
balt2<-simplify(balt2, edge.attr.comb = list(weight="sum", "ignore"))

pal <- brewer.pal(length(unique(V(balt2)$city)), "Paired")
set.seed(1000)
png('balt.png', width = 1500, height=1200)
plot (balt, vertex.size = 0.8,
      vertex.label=NA, edge.width = 0.2,
      layout = layout_nicely, asp=9/16, 
      vertex.color = ifelse(V(balt)$country == "Estoniya", "mediumblue", 
                            ifelse(V(balt)$country == "Latviya", 
                                   "indianred4", "yellow1")))
dev.off()

set.seed(1)
png('balt2.png', width = 1000, height=750)
plot (balt2, vertex.size = V(balt2)$weight/40,
      vertex.label=V(balt2)$city, edge.width = E(balt2)$weight/30,
      layout = layout_with_graphopt(balt2,charge=0.4), asp=9/16, 
      vertex.label.cex = 2,vertex.label.color = "black",
      vertex.color = pal[as.numeric(as.factor(V(balt2)$country))])
dev.off()


eu <- induced.subgraph(the_graph, which(V(the_graph)$country == "Черногория"|
                                          V(the_graph)$country == "Германия"|
                                          V(the_graph)$country == "Чехия"|
                                          V(the_graph)$country == "Швейцария"|
                                          V(the_graph)$country == "Великобритания"|
                                          V(the_graph)$country == "Эстония"|
                                          V(the_graph)$country == "Латвия"|
                                          V(the_graph)$country == "Литва"|
                                          V(the_graph)$country == "Финляндия"|
                                          V(the_graph)$country == "Польша"|
                                          V(the_graph)$country == "Нидерланды"|
                                          V(the_graph)$country == "Швеция"|
                                          V(the_graph)$country == "Франция"|
                                          V(the_graph)$country == "Кипр"|
                                          V(the_graph)$country == "Австрия"|
                                          V(the_graph)$country == "Испания"|
                                          V(the_graph)$country == "Венгрия"|
                                          V(the_graph)$country == "Норвегия"|
                                          V(the_graph)$country == "Дания"|
                                          V(the_graph)$country == "Ирландия"|
                                          V(the_graph)$country == "Мальта"|
                                          V(the_graph)$country == "Болгария"|
                                          V(the_graph)$country == "Италия"))

eu <- set.vertex.attribute(eu,"weight",index=V(eu),1)
eu <- set.edge.attribute(eu,"weight",index=E(eu),1)
eu2 <- contract.vertices(eu,
                           mapping=as.integer(as.factor(V(eu)$city)),
                           vertex.attr.comb=list(city="first", 
                                                 weight="sum",
                                                 country="first", 
                                                 "ignore")) 
eu2<-simplify(eu2, edge.attr.comb = list(weight="sum", "ignore"))

  V(eu2)[V(eu2)$country == "Германия"]$color <- "indianred4"
  V(eu2)[V(eu2)$country == "Чехия"]$color <- "hotpink"
  V(eu2)[V(eu2)$country == "Швейцария"]$color <- "mediumpurple4"
    V(eu2)[V(eu2)$country == "Великобритания"]$color <- "royalblue1"
    V(eu2)[V(eu2)$country == "Эстония"]$color <- "paleturquoise3"
    V(eu2)[V(eu2)$country == "Латвия"]$color <- "lightsalmon1"
    V(eu2)[V(eu2)$country == "Литва"]$color <- "olivedrab1"
    V(eu2)[V(eu2)$country == "Польша"]$color <- "mediumslateblue"
    V(eu2)[V(eu2)$country == "Финляндия"]$color <- "lightblue1"
    V(eu2)[V(eu2)$country == "Нидерланды"]$color <- "khaki3"
    V(eu2)[V(eu2)$country == "Швеция"]$color <- "forestgreen"
    V(eu2)[V(eu2)$country == "Кипр"]$color <- "gold1"
    V(eu2)[V(eu2)$country == "Австрия"]$color <- "olivedrab"
    V(eu2)[V(eu2)$country == "Испания"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Венгрия"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Норвегия"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Дания"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Ирландия"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Мальта"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Болгария"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Италия"]$color <- "gray62"
    V(eu2)[V(eu2)$country == "Черногория"]$color <- "gray62"
    
set.seed(100)
png('eu3.png', width = 4000, height = 3000)
plot (eu2, vertex.size = V(eu2)$weight/170,
      vertex.label=estimate_betweenness(eu2, cutoff = 7), edge.width = E(eu2)$weight/50,
      layout = layout_with_graphopt(eu2,charge=0.2), asp=9/16, 
      vertex.label.cex = 2,vertex.label.color = "black",
      vertex.color = V(eu2)$color)
dev.off()

#germany
ger <- induced.subgraph(the_graph,
                          which(V(the_graph)$country == "Germaniya"))
ger2 <- contract.vertices(ger,
                           mapping=as.integer(as.factor(V(ger)$city)),
                           vertex.attr.comb=list(city="first", 
                                                 weight="sum", 
                                                 "ignore"))
ger2<-simplify(ger2, edge.attr.comb = list(weight="sum", "ignore"))

V(ger2)[V(ger2)$city == "Berlin"]$color <- "darkorchid4"
V(ger2)[V(ger2)$city == "Gamburg"]$color <- "goldenrod1"
V(ger2)[V(ger2)$city == "Myunkhen"]$color <- "azure4"
V(ger2)[V(ger2)$city == "Shtutgart"]$color <- "plum2"
V(ger2)[V(ger2)$city == "Drezden"]$color <- "forestgreen"
V(ger2)[V(ger2)$city == "Dortmund"]$color <- "yellow"
V(ger2)[V(ger2)$city == "Dyusseldorf"]$color <- "slategray2"
V(ger2)[V(ger2)$city == "Keln"]$color <- "wheat4"
V(ger2)[V(ger2)$city == "Nyurnberg"]$color <- "darkolivegreen2"
V(ger2)[V(ger2)$city == "Frankfurt-na-Mayne"]$color <- "lightsalmon2"

pal <- brewer.pal(length(unique(V(ger2)$city)), "Paired")
set.seed(10)
png('ger2.png', width = 1200, height = 800)
plot (ger2, vertex.size = V(ger2)$weight/50,
      vertex.label=V(ger2)$city, edge.width = E(ger2)$weight/40,
      layout = layout_with_graphopt(ger2,charge = 0.5), asp=9/16, 
      vertex.label.cex = 2,vertex.label.color = "black",
      vertex.color = V(ger2)$color)
dev.off()