setwd("C:/Users/Mimic03/Documents/GA_AR/Corpus/fake")
library(igraph)
rm(list=ls())

nodes<-read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links<-read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))
links<-aggregate(links[,3], links[,-3], sum)
links<-links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

net<-graph_from_data_frame(d=links,vertices=nodes,directed=T) 
class(net)

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media 
plot(net, edge.arrow.size=.4,vertex.label=NA)
net<-simplify(net,remove.multiple=F,remove.loops=T) 


####
nodes2<-read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2<-read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
head(nodes2)
head(links2)
links2<-as.matrix(links2)

net2<-graph_from_incidence_matrix(links2)
table(V(net2)$type)

net2.bp <- bipartite.projection(net2)
as_incidence_matrix(net2)  %*% t(as_incidence_matrix(net2)) 
t(as_incidence_matrix(net2)) %*%   as_incidence_matrix(net2)
plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     
     vertex.size=7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])
