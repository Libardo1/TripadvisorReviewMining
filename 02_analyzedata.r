#Downloading data from Tripadvisor Pizza Hut ratings and performing sentiment analysis

#Script 1: Reading the dataset downloaded in script1 and analysis of data

#libraries
library(tm)
library(SnowballC)
library(ggplot2)

library(RCurl)

library(ggmap)
library(RgoogleMaps)



#Read the Data Frame created in 01_get_data.r
df<-read.table("allreviews_dataframe.txt")

#Read the text Corpus of all the reviews
#Create a Corpus of Text Documents
cname <- file.path("./reviews/")
dir(cname)
length(dir(cname))
docs<-Corpus(DirSource(cname))
docs.copy<-docs

#Clean the data
for (j in seq(docs)){
        docs[[j]]<-gsub("/"," ",docs[[j]])
        docs[[j]]<-gsub("@","",docs[[j]])
        docs[[j]]<-gsub("\\\\","",docs[[j]])
        docs[[j]]<-gsub("\\\t","",docs[[j]])
        docs[[j]]<-gsub("\\[","",docs[[j]])
        docs[[j]]<-gsub("\\]","",docs[[j]])
        
}


#Transformations
docs<-tm_map(docs,tolower)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removePunctuation)


#Remove Stopwords
docs<-tm_map(docs, removeWords, stopwords("english"))
#Remove Self StopWords - Refine it till all high-freq stopwords are removed
docs<-tm_map(docs, removeWords, c("pizza","also","one"))

#Remove Whitespaces
docs<-tm_map(docs,stripWhitespace)

#make Sure it is a PLainTextDocument
docs<-tm_map(docs,PlainTextDocument)

#inspect
inspect(docs[1])

#Create Document Term matrix
dtm<-DocumentTermMatrix(docs)
dtm
inspect(dtm[1:1,1:10])


#Words Frequency

freq<-colSums(as.matrix(dtm))
ord<-order(freq)
freq[tail(ord,100)]




#Remove sparse terms
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1)
dim(dtms)



#Plot Word Frequencies
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)
freq[1]<-400
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

#Plot
library(ggplot2)
p <- ggplot(subset(wf, freq<100 & freq>1), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


#Wordcloud