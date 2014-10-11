
#Downloading data from Tripadvisor Zuri Hotel ratings and performing sentiment analysis

#Script 1: Downloading the data and saving all the reviews in different text files 
# for creating the corpus later

#libraries
library(tm)
library(SnowballC)
library(ggplot2)

library(RCurl)
library(XML)

library(ggmap)
library(RgoogleMaps)


#Page Pattern
#1:http://www.tripadvisor.in/Hotel_Review-g297628-d1230533-Reviews-or10-The_Zuri_Whitefield_Bangalore-Bangalore_Karnataka.html#REVIEWS
#2:http://www.tripadvisor.co.uk/Restaurant_Review-g186338-d1010274-Reviews-or10-Pizza_Hut-London_England.html#REVIEWS


alllinks<-c()
#i: 0 to 45
for(i in 0:45){
        if (i==0){
                pageurl<-"http://www.tripadvisor.in/Hotel_Review-g297628-d1230533-Reviews-The_Zuri_Whitefield_Bangalore-Bangalore_Karnataka.html"
        } else {
                pageurl<-paste0("http://www.tripadvisor.in/Hotel_Review-g297628-d1230533-Reviews-or",i,"0-The_Zuri_Whitefield_Bangalore-Bangalore_Karnataka.html")
        }
        
        pagecontent<-readLines(pageurl)
        
        #Write to csv for analyzing pagecontent and to figure out pattern
        #write.csv(pagecontent,file="pagecontent.csv")
        
        #Get lines containing link for individual reviews
        
        doc = htmlParse(pagecontent, asText=TRUE)
        plain.text <- xpathSApply(doc, "//p", xmlValue)
        
        links <- xpathSApply(doc, "//a/@href")
        
        #Write to csv for analyzing links and to figure out pattern
        #write.csv(links,file="links.csv")
        
        comments<-which(grepl("#CHECK_RATES_CONT",links))
        links<-unique(links[comments])
        links<-paste0("http://www.tripadvisor.in",links)
        alllinks<-c(alllinks,links)        
}

alllinks<-unique(alllinks)
#Remove the link with just tripadvisor.in
alllinks<-alllinks[-438]

#Function for extracting user information from the HTML
getcustinfo<-function(attribute){
        xpth<-paste0("//div[@class='",attribute,"']")
        var<-xpathSApply(doc,xpth,xmlValue)
        var<-gsub("\\\n","",var)
        return(var[1])  
}

#All links Stored in variable alllinks
#Read Content

#Define a data Frame
df <- data.frame(cust_name = character(0),
                 cust_loc = numeric(0),
                 cust_rev_title = character(0),
                 cust_contri_review=character(0),
                 review_date=character(0),
                 review_title=character(0),
                 cust_lon=numeric(0),
                 cust_lat=numeric(0),
                 cust_review=character(0))

for(i in 1:length(alllinks)){
        url<-alllinks[i]
        reviews<-readLines(url)
        
        #Write to CSV to analyze for the first time, no need to repeat for all
        #write.csv(reviews,file="reviewContent.csv")
        
        doc = htmlParse(reviews, useInternal=TRUE)
        plain.text <- xpathSApply(doc, "//p", xmlValue)
        
        #Write plaintext to csv to analyze the pattrn, no need to repeat for all
        #write.table(plain.text,file="plaintext.txt")
        
        #Customer Review
        plain.text<-plain.text[1]
        plain.text<-gsub("\\\n","",plain.text)
        
        #Customer Details
        cust_name<-getcustinfo("username mo")
        cust_loc<-getcustinfo("location")
        #cust_mbadge<-getcustinfo("memberBadging")
        cust_rev_title<-getcustinfo("reviewerTitle")
        cust_contri_review<-getcustinfo("contributionReviewBadge")
        
        #review details
        review_date<-getcustinfo("ratingDate")
        review_title<-getcustinfo("quote")
        
        #Get review Date
        xpth<-paste0("//span[@class='ratingDate']")
        var<-xpathSApply(doc,xpth,xmlValue)
        var<-gsub("\\\n","",var)
        var<-gsub("Reviewed ","",var)
        var<-gsub("NEW","",var)
        review_date<-var[1]
        
        #Get Latitude & Longitude of the Location
        loc <- geocode(as.character(cust_loc),output="more")
        cust_lon<-loc$lon
        cust_lat<-loc$lat
        
        
        #Writing everything into a DataFrame
        df<-rbind(df,data.frame(cust_name=cust_name, cust_loc=cust_loc, 
                                cust_rev_title=cust_rev_title,
                                cust_contri_review=cust_contri_review,
                                review_title=review_title,
                                review_date=review_date,
                                cust_lon=cust_lon,cust_lat=cust_lat,
                                cust_review=plain.text))
        
        
        #Write all the reviews in separate text files to be stored as a Corpus
        ReviewText<-paste(plain.text, collapse = "")
        filename<-paste0("./reviews/review",i,".txt")
        writeLines(as.character(ReviewText),con=filename)
        
        
}

#Get City and country
cust_address<-geocode(as.character(df$cust_loc),output="more")

df$cust_city<-cust_address$locality
df$cust_country<-cust_address$country

#Write the data frame to a file
write.table(df,file="allreviews_dataframe.txt")






