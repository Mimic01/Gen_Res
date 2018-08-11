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
clean_input
# POS Tagging
library(NLP)
library(openNLP)
library(openNLPmodels.es)
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
clean_input<-str_trim(clean_input,side="both")
clean_input<-lapply(clean_input,function(x){x<-as.String(x)})

corpus.tagged<-lapply(clean_input,function(x){
   sent_token_annotator<-Maxent_Sent_Token_Annotator()
   word_token_annotator<-Maxent_Word_Token_Annotator()
   pos_tag_annotator<-Maxent_POS_Tag_Annotator()
   y1<-annotate(x,list(sent_token_annotator,word_token_annotator))
   y2<-annotate(x,pos_tag_annotator,y1)
   # y3 <- annotate (x, Maxent_POS_Tag_Annotator ( probs = TRUE ),y1)
 y2w<-subset(y2,type=="word")
 tags<-sapply(y2w$features,'[[',"POS")
 r1<-sprintf("%s/%s",x[y2w],tags)
 r2<-paste(r1,collapse=" ")
 return(r2) })
 
# POS Tagged Corpus
corpus.tagged
# NER
require("NLP")
## Some text.
s<-corpus.tagged
s<-as.String(s)
## Need sentence and word token annotations.
sent_token_annotator<-Maxent_Sent_Token_Annotator()
word_token_annotator<-Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
## Entity recognition for persons.
entity_annotator<-Maxent_Entity_Annotator(language="es")
entity_annotator
annotate(s, entity_annotator, a2)
## Directly:
entity_annotator(s, a2)
## And slice ...
s[entity_annotator(s, a2)]
## Variant with sentence probabilities as features.
annotate(s, Maxent_Entity_Annotator(language="es",probs = TRUE), a2)



