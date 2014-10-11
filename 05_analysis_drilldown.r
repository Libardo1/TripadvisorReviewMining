#Downloading data from Tripadvisor Pizza Hut ratings and performing sentiment analysis

#Script 4: Reading the dataset downloaded in script1 and sentiment analysis of data



#Steps and Key Analysis to be performed:
#1. Word freq and Wordcloud for each emotion-polarity combination
#2. Calculate Emotion and polarity of Review Headers
#3. Timeline: When are customers writing reviews by month; How old are the good and bad reviews
#4. How many of these customers are local?
#5. Sentiment by geography - How may local residents feel positive or negative
#6: Word Correlation in worst reviews and Best Reviews
#7. Good and Bad Reviews by Polularity/Seniority of reviewers
#8. Action Items : names and details of reviwers - top 5 and bottom 5
#9. Accuracy of model as compared to human interpretation
#10. Accuracy as compared to the reviews ratings on site

#libraries
library(tm)
library(SnowballC)
library(ggplot2)

library(RCurl)

library(ggmap)
library(RgoogleMaps)
library(sentiment)
library(wordcloud)

#Read the Sentiment Data Frame created in 01_get_data.r
dfs<-read.table("sentimentanalysis.txt")

temp<-subset(dfs,polarity=="negative")
write.csv(temp,file="negativereviews.csv")

#####
# 1 #
#####

dfs_negative<-subset(dfs,polarity=="negative" & (emotion=="disgust" | emotion=="anger" | emotion=="fear"))
dfs_positive<-subset(dfs,polarity=="positive"& emotion=="joy")

WordAnalysis(dfs_negative)
WordAnalysis(dfs_positive)

#Write a function to extract the subset of dataset, create wordfreq & wordcloud, plot emotion & polarity


#Update [24sep]: Need to fix the output for the wordfreq chart
WordAnalysis<-function(df){
       
        #Create a corpus of reviews for this subset
        docs <- Corpus(VectorSource(dfs_negative$cust_review))
        
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
        
        
        docs<-tm_map(docs, removeWords, stopwords("english"))
        docs<-tm_map(docs, removeWords, c("zuri","hotel","bangalore"))
        docs<-tm_map(docs, removeWords, c("two","can"))

        docs<-tm_map(docs,stripWhitespace)
        docs<-tm_map(docs,PlainTextDocument)
        
        #Create Document Term matrix
        dtm<-DocumentTermMatrix(docs)
        
        freq<-colSums(as.matrix(dtm))
        ord<-order(freq)
        
        #Plot Word Frequencies
        freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
        wf <- data.frame(word=names(freq), freq=freq)
        
        #Plot
        png("./Output/wordfreq.png")
        p <- ggplot(subset(wf, freq>0), aes(word, freq))
        p <- p + geom_bar(stat="identity")
        p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
        p
        dev.off()
        
        #Wordcloud
        png("./Output/wordcloud.png")
        wordcloud(names(freq), freq, min.freq=1,colors=brewer.pal(6, "Dark2"),
                  scale=c(5, .1),rot.per=0.1)
        dev.off()
}


#####
# 7 #
#####

#Good and Bad Reviews by seniority of Reviewers
#dfs_negative
#dfs_positive
png("./Output/neg_reviwer.png")
pie(table(dfs_negative$cust_rev_title))
dev.off()

png("./Output/pos_reviwer.png")
pie(table(dfs_positive$cust_rev_title))
dev.off()

#####
# 4 #
#####

#local Reviewers

table(dfs_negative$country)
barplot(table(dfs_positive$country))

barplot(table(dfs$polarity)
)
