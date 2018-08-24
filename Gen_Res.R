library(readr)
library(stringr)
library(coreNLP)
library(openNLP)

setwd("C:/Users/Alex/Documents/GA_AR/Corpus")
filename<-"clean_tweet_input.txt"
clean_input<-paste(scan(filename,what="character",sep=NULL),collapse=" ")
Encoding(clean_input)<-"UTF-8"
clean_input<-gsub("<COREF CAD= IDE=>","",clean_input,fixed=TRUE)
clean_input<-gsub("</coref>","",clean_input,fixed=TRUE)
#Parsing
# POS Tagging
#library(NLP)
#library(openNLP)
#library(openNLPmodels.es)
#library(tm)
#library(stringr)
#library(gsubfn)
#library(plyr)
#clean_input<-str_trim(clean_input,side="both")
#clean_input<-lapply(clean_input,function(x){x<-as.String(x)})

#corpus.tagged<-lapply(clean_input,function(x){
#   sent_token_annotator<-Maxent_Sent_Token_Annotator()
#   word_token_annotator<-Maxent_Word_Token_Annotator()
#   pos_tag_annotator<-Maxent_POS_Tag_Annotator()
#   y1<-annotate(x,list(sent_token_annotator,word_token_annotator))
#   y2<-annotate(x,pos_tag_annotator,y1)
# y3 <- annotate (x, Maxent_POS_Tag_Annotator ( probs = TRUE ),y1)
# y2w<-subset(y2,type=="word")
# tags<-sapply(y2w$features,'[[',"POS")
# r1<-sprintf("%s/%s",x[y2w],tags)
# r2<-paste(r1,collapse=" ")
# return(r2) })

# POS Tagged Corpus
#corpus.tagged
# NER
#require("NLP")
## Some text.
#s<-clean_input
#s<-as.String(s)
## Need sentence and word token annotations.
#sent_token_annotator<-Maxent_Sent_Token_Annotator()
#word_token_annotator<-Maxent_Word_Token_Annotator()
#a2<-annotate(s,list(sent_token_annotator,word_token_annotator))
## Entity recognition for persons.
#entity_annotator<-Maxent_Entity_Annotator(language="es")
#entity_annotator
#annotate(s,entity_annotator,a2)
## Directly:
#entity_annotator(s,a2)
## And slice ...
#entity_list<-s[entity_annotator(s, a2)]
#entity_list<-as.list(entity_list)
## Variant with sentence probabilities as features.
#annotate(s, Maxent_Entity_Annotator(language="es",probs = TRUE), a2)


#install.packages("udpipe")
library(udpipe)
#udmodel<-udpipe_download_model(language="spanish")
#vignette("udpipe-tryitout",package="udpipe")
#vignette("udpipe-annotation", package="udpipe")
#vignette("udpipe-train",package="udpipe")
#vignette("udpipe-usecase-postagging-lemmatisation",package="udpipe")
#vignette("udpipe-usecase-topicmodelling",package="udpipe")

#udmodel
udmodel_spanish<-udpipe_load_model(file="spanish-ud-2.0-170801.udpipe")
clean_input<-as.vector(clean_input)
x<-udpipe_annotate(udmodel_spanish,clean_input)
x<-as.data.frame(x)
#This function adds the parent info to the annotated data.frame
x<-cbind_dependencies(x,type=c("parent","child"))
#Frecuencia de POS tags
library(lattice)
stats<-txt_freq(x$upos)
stats$key<-factor(stats$key,levels=rev(stats$key))
barchart(key~freq,data=stats,col="cadetblue",main="UPOS \n freq of occurrence",xlab="Frequency")

## Most ocurring proper nouns
stats<-subset(x,upos %in% c("PROPN"))
stats<-txt_freq(stats$token)
stats$key<-factor(stats$key,levels=rev(stats$key))
barchart(key~freq,data=head(stats,20),col="cadetblue",main="Most ocurring proper nouns",xlab="Frequency")

#Co-ocurrencias
cooc<-cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                   term = "lemma", 
                   group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)
library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork<-head(cooc, 30)
wordnetwork<-graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none") +
  labs(title = "Coocurrencias dentro de una frase", subtitle = "Sustantivos y Adjetivos")

#Correlaciones
x$id<-unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm<-subset(x, upos %in% c("NOUN", "ADJ"))
dtm<-document_term_frequencies(dtm, document = "id", term = "lemma")
dtm<-document_term_matrix(dtm)
dtm<-dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y<-as_cooccurrence(termcorrelations)
y<-subset(y, term1 < term2 & abs(cooc) > 0.2)
y<-y[order(abs(y$cooc), decreasing = TRUE), ]
head(y)

#Formato Conllu para mostrar tabla de dependency parsing
library(udpipe)
conllu<-as_conllu(x)
cat(conllu)
#Creamos una file en formato conllu que se va al working directory documents/GA_AR/Corpus
cat(as_conllu(x),file=file("annotations.conllu",encoding="UTF-8"))

#Extraccion de keywords usando dependency parsing
stats<-merge(x, x, 
             by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
             by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
             all.x = TRUE, all.y = FALSE, 
             suffixes = c("", "_parent"), sort = FALSE)
stats <- subset(stats, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
stats$term <- paste(stats$lemma_parent, stats$lemma, sep = " ")
stats <- txt_freq(stats$term)
library(wordcloud)
wordcloud(words = stats$key, freq = stats$freq, min.freq = 3, max.words = 100,
          random.order = FALSE, colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))

#####################################################
##Subset de proper nouns, nouns y pronombres.
stats<-subset(x,upos %in% c("PROPN","NOUN","PRON"))
library(dplyr)
stats_subset<-select(stats,token,upos,feats)
stats_subset$uid <- sprintf("S%003d", 1:nrow(stats_subset))
stats_subset<-stats_subset[,c(4,1,2,3)]
#subset de las primeras 100 filas
stats_subset2<-stats_subset[1:100,]
mat<-matrix(1,100,100)
rownames(mat)<-colnames(mat)<-stats_subset2$uid
library(igraph)
g<-graph.adjacency(mat)
linking<-get.edgelist(g)
linking<-as.data.frame(linking)
nodes<-stats_subset2
linking$token<-nodes$token
linking$upos<-nodes$upos
#linking$fromPos<-nodeses$upos
net<-graph_from_data_frame(d=linking,vertices=nodes,directed=T)
#Edges/Links of the nerwork
E(net)
#Vertices of the nerwork
V(net)
#we remove loops
net<-simplify(net,remove.multiple=F,remove.loops=T)  
plot(net,edge.arrow.size=.1,vertex.label=V(net)$token,vertex.shape="none")
##The proportion of present edges from all possible edges in the network (for a directed network)
#ecount(net)/(vcount(net)*(vcount(net)-1))
#imprime una matrix de direcciones de nodos
ends(net,es=E(net),names=F)

library(stringr)
#separamos feats y creamos nuevas columnas de ellos 1 a 1
feat_node<-str_split_fixed(nodes$feats, "\\|", 6)
feat_node<-sub("^$",NA,feat_node)
feat_node<-as.data.frame(feat_node,stringsAsFactors=FALSE)
feat_node
colnames(linking)<-c("ID1","ID2","token","upos")
linking<-cbind(linking,feat_node)
