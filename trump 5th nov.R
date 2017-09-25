install.packages('readxl')
library (readxl)
trump<-read_excel("trump.xlsx")
trump<-na.omit(trump)

View(trump)



library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)




############################################################################################

trump<-read_excel("trump.xlsx")
trump<-na.omit(trump)

trumpold<- corpus(trump$`Transcript `,
                    docnames = trump$Number)
  


head(trumpold,1) 

summary(trumpold)




View(trumpold)
class(trumpold)
trumpold
names(trumpold)

options(max.print = 999999999)
summary(trumpold)
trumpold
options(dplyr.print_max = 99999999)

############################################################################################


require(tm)
trumpnew<- toLower(trumpold,keepAcronyms = FALSE)
trumpnew 

trumpnew<- tokenize(trumpnew,
                      removeNumbers = T ,
                      removePunct = T,
                      removeSeparators=T,
                      removeTwitter = T,
                      removeHyphens = T,
                      removeSymbols = T,
                      verbose = T,
                      ngrams=1)
trumpnew
head(trumpnew,3)
###########################################################################################
# STEM reduces words to its root eg clients = client, valen's = valen
#STOPWORDS is during the implementation of document feature martix(dfm), the data frame will 
# stop on those words which are english 
############################################################################################
#Another method for steming words and also a way to remove unwanted words by making dictionary  
#to create a custom dictionary  list of stop words

trumplist<- c("core","value","use","can","will","one","time","go","make","now","want","put","back")#16,6,6
trumpstem<-dfm(trumpnew,
                 ignoredFeatures = c(trumplist,stopwords("english")),
                 stem = T,
                 verbose=T
)
trumpstem 
############################################################################################

trumptopnew<- topfeatures(trumpstem,50)
trumptopnew

############################################################################################
#by using KWIC you can see the word you want to see and the pre and suffix of that resp. word
kwic(trumpnew,"american",3)
kwic(trumpnew,"data",3)


############################################################################################
#Sentiment Aanlysis 
trumpdict<- dictionary(list(negative=c("detriment*", "bad*", "awful*", "terrib*", "horribl*,"),
                              positve=c("good", "great", "super*", "excellent", "yay","vision",
                                        "achieve","success")))
trumpdict
trumpsentiment<-dfm(trumpnew,dictionary = coredict)
View(trumpsentiment)

############################################################################################

##################
### WORD CLOUD ###   
##################
require(wordcloud)
wordcloud(names(trumptopnew),
          trumptopnew,
          max.words = 50,
          scale = c(2,1),
          colors = brewer.pal(8,"Set2"))

############################################################################################


head(trumpstem,3)
trumptm<- convert (trumpstem, to = "tm")

#specifying a correlation limit of 0.5  
require(tm)
?findAssocs
findAssocs(trumptm,c("data","business","help","client","mission"),
           corlimit=0.3)

############################################################################################

########################
### TOPIC MODELLING ####   
########################

library(stm)
?textProcessor
trumptemp <- textProcessor(documents = trump$`Transcript `,
                             metadata = trump)

names(trumptemp)

meta<-trumptemp$meta
vocab<-trumptemp$vocab
docs<-trumptemp$documents

trumpout<-prepDocuments(docs,vocab,meta)
trumpout

docs<-trumpout$documents
vocab<-trumpout$vocab
meta<-trumpout$meta
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?stm
trumpprevfit <- stm(docs,vocab,
                      3,
                      verbose=TRUE,
                      data=meta,
                      max.em.its = 15) 

trumptopics<-labelTopics(trumpprevfit,topics = c(1:3))
trumptopics
############################################################################################
?findThoughts
findThoughts(trumpprevfit,texts = trump$`Transcript `,
             topics = c(1,2),n=2)


plot.STM(trumpprevfit,type="summary")
plot.STM(trumpprevfit,type="labels",topics = c(1:3))############################################################################################
plot.STM(trumpprevfit,type="perspectives",topics = c(1,3))

############################################################################################
library(igraph)
?topicCorr
trumpcor<-topicCorr(trumpprevfit)
plot.topicCorr(trumpcor)

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
