#Downloading data from Tripadvisor Zuri ratings and performing sentiment analysis

#Script 2: Reading the dataset downloaded in script1 and analysis of data, Creating
# Word Frequency and Wordcloud. Also, plotting the words correlations

#libraries
library(tm)
library(SnowballC)
library(ggplot2)

library(RCurl)

library(ggmap)
library(RgoogleMaps)




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
docs<-tm_map(docs, removeWords, c("zuri","hotel","bangalore"))
docs<-tm_map(docs, removeWords, c("two","can"))
#Remove Whitespaces
docs<-tm_map(docs,stripWhitespace)

#make Sure it is a PLainTextDocument
docs<-tm_map(docs,PlainTextDocument)

#Create Document Term matrix
dtm<-DocumentTermMatrix(docs)

#Words Frequency
freq<-colSums(as.matrix(dtm))
ord<-order(freq)

#Plot Word Frequencies
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

#Plot
library(ggplot2)
p <- ggplot(subset(wf, freq>75 & freq>1), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


#Wordcloud
library(wordcloud)
set.seed(112)
wordcloud(names(freq), freq, min.freq=30,colors=brewer.pal(6, "Dark2"),
          scale=c(5, .1),rot.per=0.1)
