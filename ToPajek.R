#To Pajek

library(stringi)

#Russian language is nice and all, but working with latin symbols is way more 
#convenient in Pajek, so we convert all city and country names to 
# latin letters (in UTF-8 encoding).
g <- the_graph
V(g)$city <- gsub('ё','e',gsub('Ё','E',V(g)$city))
V(g)$city <- gsub('ь',"'",gsub('ъ',"'",V(g)$city))
V(g)$country <- gsub('ё','e',gsub('Ё','E',V(g)$country))
V(g)$country <- gsub('ь',"'",gsub('ъ',"'",V(g)$country))

V(g)$city <- stri_trans_general(V(g)$city, "russian-latin/bgn")
V(g)$country <- stri_trans_general(V(g)$country, "russian-latin/bgn")

V(g)$country[which(V(g)$country == "")] <- "NoCountry"

m <- decompose(g, min.vertices = 300)[[1]]

V(g)$id <- V(g)$city
write.graph(g,"CHGK_cities.net", format = "pajek")

V(g)$id <- V(g)$country
write.graph(g,"CHGK_countries.net", format = "pajek")

# write.graph can only write 1 attribute to pajek, so we create two networks.

V(m)$id <- V(m)$city
write.graph(m,"Main_comp_cities.net", format = "pajek")

V(m)$id <- V(m)$country
write.graph(m,"Main_comp_countries.net", format = "pajek")

