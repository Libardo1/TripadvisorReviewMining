#Downloading data from Tripadvisor Pizza Hut ratings and performing sentiment analysis

#Script 4: Reading the dataset downloaded in script1 and sentiment analysis of data

########################################################################
# Sentiment Analysis
########################################################################

#libraries
library(tm)
library(SnowballC)
library(ggplot2)

library(RCurl)

library(ggmap)
library(RgoogleMaps)

#Sentiment Analysis
require(sentiment)


#Read the Data Frame created in 01_get_data.r
df<-read.table("allreviews_dataframe.txt")

###Reviews Classification
# classify emotion
class_emo = classify_emotion(df$cust_review, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

png("./Output/emotion.png")
barplot(table(emotion))
dev.off()

# classify polarity
class_pol = classify_polarity(df$cust_review, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
png("./Output/polarity.png")
barplot(class_pol)
dev.off()

df<-cbind(df,data.frame(emotion,polarity))


#Emotion & Polarity from Review Topics
class_emo_title = classify_emotion(df$cust_rev_title, algorithm="bayes", prior=1.0)
emotion_title = class_emo_title[,7]
emotion_title[is.na(emotion_title)] = "unknown"

class_pol_title = classify_polarity(df$cust_rev_title, algorithm="bayes")
polarity_title = class_pol_title[,4]

df<-cbind(df,data.frame(emotion_title,polarity_title))

#Create Customer Id for all customers
df$cust_id<-rownames(df)

#Write the final dataframe to a csv for analysis
write.table(df, file="sentimentanalysis.txt")


