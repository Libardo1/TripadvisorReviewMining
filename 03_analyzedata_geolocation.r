#Downloading data from Tripadvisor Pizza Hut ratings and performing sentiment analysis

#Script 3: Reading the dataset downloaded in script1 and geolocation analysis of data

#Libraries

library(tm)
library(SnowballC)
library(ggplot2)

library(RCurl)

library(ggmap)
library(RgoogleMaps)


#Read the Data Frame created in 01_get_data.r
df<-read.table("allreviews_dataframe.txt")

df2<-data.frame(x=df$cust_lon,y=df$cust_lat)
df2<-df2[complete.cases(df2),]

#Latitude & Longitude of London
loc <- geocode("New york",output="more")
lon<-loc$lon
lat<-loc$lat


#Plot coordinates on a Map
map <- get_map(location = c(lon = lon, lat = lat), zoom = 3,
                      maptype = "roadmap", scale = 2)

ggmap(map) +
        geom_point(data = df2, aes(x = x, y = y, fill = "red", alpha = 0), size = 4, shape = 21) +
        guides(fill=FALSE, alpha=FALSE, size=FALSE)

#CityWise & countrywise Reviewers
country_reviews<-table(df$cust_country)
city_reviews<-table(df$cust_city)

write.csv(country_reviews, file="reviewsbycountry.csv")
write.csv(city_reviews, file="reviewsbycity.csv")
