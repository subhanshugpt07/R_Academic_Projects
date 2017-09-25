install.packages('readxl')
library (readxl)
core<-read_excel("core.xlsx")
core<-na.omit(core)


View(core)



library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)




############################################################################################
library(readxl)
core<-read_excel("core.xlsx")
core<-na.omit(core)
str(core)

coreold<- corpus(core$`Mission Statement `,
                 docnames = core$`Sr No`,
                 docvar= data.frame(Company=core$`Company `))


head(coreold,3) 
options(max.print = 999999999)
summary(coreold)
coreold

############################################################################################


require(tm)
corenew<- toLower(coreold,keepAcronyms = FALSE)
corenew 

corenew<- tokenize(corenew,
                   removeNumbers =T ,
                   removePunct = T,
                   removeSeparators=T,
                   removeTwitter = T,
                   removeHyphens = T,
                   removeSymbols = T,
                   verbose = T,
                   ngrams=1)
head(corenew,2)
############################################################################################
# STEM reduces words to its root eg clients = client, valen's = valen
#STOPWORDS is during the implementation of document feature martix(dfm), the data frame will 
                                        # stop on those words which are english 
############################################################################################
#Another method for steming words and also a way to remove unwanted words by making dictionary  
#to create a custom dictionary  list of stop words

corelist<- c("mission","use","can","make","use","way","lead","everi","vision","success")#16,6,6
corestem<-dfm(corenew,
              ignoredFeatures = c(corelist,stopwords("english")),
              stem = T,
              verbose=T
              )
corestem 
############################################################################################

coretopnew<- topfeatures(corestem,30)
coretopnew

############################################################################################


##################
### WORD CLOUD ###   
##################
require(wordcloud)
wordcloud(names(coretopnew),
          coretopnew,
          max.words = 300,
          scale = c(3,1),
          colors = brewer.pal(8,"Set2"))
############################################################################################



head(corestem,3)
coretm<- convert (corestem, to = "tm")

#specifying a correlation limit of 0.5  
require(tm)
?findAssocs
findAssocs(coretm,c("data","compani","busi","help","world",
                    "peopl","custom","valu","work","enterpris",
                    "technolog","inform","analyt","improv","organ"),
           corlimit=0.78)

############################################################################################

########################
### TOPIC MODELLING ####   
########################

library(stm)
?textProcessor
coretemp <- textProcessor(documents = core$`Mission Statement `,
                          metadata = core)

names(coretemp)

meta<-coretemp$meta
vocab<-coretemp$vocab
docs<-coretemp$documents

coreout<-prepDocuments(docs,vocab,meta)
coreout

docs<-coreout$documents
vocab<-coreout$vocab
meta<-coreout$meta
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?stm
coreprevfit <- stm(docs,vocab,
                 30,
                  verbose=TRUE,
                  data=meta,
                  max.em.its = 15) 

coretopics<-labelTopics(coreprevfit,topics = c(1:20))
coretopics
############################################################################################
?findThoughts
findThoughts(coreprevfit,texts = core$`Mission Statement `,
             topics = c(1,2),n=2)


plot.STM(coreprevfit,type="summary")
plot.STM(coreprevfit,type="labels",topics = c(1:5))############################################################################################
plot.STM(coreprevfit,type="perspectives",topics = c(1,2))

############################################################################################
library(igraph)
?topicCorr
corecor<-topicCorr(coreprevfit)
plot.topicCorr(corecor)

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
customers

analytics --- data 



top 5 words words
wordcloud 
correlation 

