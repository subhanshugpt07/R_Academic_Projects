install.packages("twitteR")
install.packages("ggplot2")
install.packages("RCurl")
install.packages("tm",dependencies = TRUE)
install.packages("slam")
install.packages("wordcloud")
install.packages("NLP")
version
install.packages("pacman")
pacman::p_load(tm)
library(NLP)
library(tm)
library(RColorBrewer)
library(twitteR)
library(RCurl)
library(wordcloud)
###########################################################################################
version
consumer_key<-"NygdcnOj0HWE2cbFRENt*****"
consumer_secret<-"Lz75XDXjzFHKDS2B3NS7AJa55YTuXWiguuXifQ0PR5loH****"
acces_token<-"372927498-H7v2P9jxq1KHJ4IHSQbdoEy0kQDeCecsnbcW****"
access_secret<-  "KYIiuHIqvtL8j0ah1cBwXAqG9Dk15sikkuNTBJDn*****"

setup_twitter_oauth(consumer_key ,consumer_secret,acces_token,access_secret)
1
###########################################################################################


LFC_tweets<- searchTwitter("Global Innovation",n=10,lang="en")
LFC_tweets[1:3]


###########################################################################################

#search examples from function help file 
himearth<-searchTwitter("trump+hiliary",lang="en",n=32,resultType="recent")
class(himearth)
str(himearth)
himearth[1:10]

#Convert list to vector
himearth_text<-sapply(himearth,function(x)x$getText())
str(himearth_text)

#Create Corpus from vector of tweets
him_corpus<-Corpus(VectorSource(himearth_text))
him_corpus
inspect(him_corpus[1])
show(him_corpus[1])
#lower cases,remove number, cut out stopwards, remove punctuation, strip whitespance 
him_clean<-tm_map(him_corpus,removePunctuation)
him_clean<-tm_map(him_clean,content_transformer(tolower))
him_clean<-tm_map(him_clean,removeWords,stopwords("english"))
him_clean<-tm_map(him_clean,removeNumbers)
him_clean<-tm_map(him_clean,stripWhitespace)
inspect(him_clean[1])

#you want to remove the search words these will obviously be very frequent 
him_clean<-tm_map(him_clean,removeWords,c("everest","earthquake"))

#wordcloud play with parameters 
wordcloud(him_clean)
wordcloud(him_clean,random.order=F,max.words=40,scale=c(3,0.5),colors=rainbow(50))

