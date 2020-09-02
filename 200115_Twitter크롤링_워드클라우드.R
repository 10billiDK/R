getwd()
setwd("/Users/kimdongkyu/Desktop/twitter")
install.packages(c("twitteR","ROAuth", "base64enc"))
library(twitteR)
library(ROAuth)
library(base64enc)
consumerKey <-"LRtW8vAt7PdgyAfuOHVQvVRac"
consumerSecret <-"MzdjHqGoC4k83Ab5VDG4v1y9FGR4nKegQWDK6C2Nq1M3aUQ7x8"
accessToken <- "1217341031236030464-gT7Jsk7d6zoFxZf5jdmnNLHvnXbGxS"
accessTokenSecret<-"3t0SL3Cvxm6k2BkZLlACVDLr520iPOqe20HpCCafNApev"
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

keyword<-enc2utf8("인공지능")
AI<- searchTwitter(keyword, n=1000, lang="ko")
length(AI)
head(AI)

AI2<- searchTwitter(keyword, since='2020-01-01', until='2020-01-15', lang="ko")
length(AI2)
head(AI2)

library(twitteR)
library(KoNLP)
library(wordcloud)
library(plyr)
library(tm)

AI_twitter.df<-twListToDF(AI)
AI_twitter.text<-AI_twitter.df$text

AI_twitter.text<-gsub("\n","",AI_twitter.text)
AI_twitter.text<-gsub("\r","",AI_twitter.text)
AI_twitter.text<-gsub("RT","",AI_twitter.text)
AI_twitter.text<-gsub("늑김","",AI_twitter.text)
AI_twitter.text<-gsub("빡겜","",AI_twitter.text)
AI_twitter.text<-gsub("무혐의","",AI_twitter.text)
AI_twitter.text<-gsub("다행","",AI_twitter.text)
AI_twitter.text<-gsub("다이","",AI_twitter.text)
AI_twitter.text<-gsub("다오밍","",AI_twitter.text)
AI_twitter.text<-gsub("다가","",AI_twitter.text)
AI_twitter.text<-gsub("http","",AI_twitter.text)

AI_twitter_nouns<-Map(extractNoun, AI_twitter.text)
AI_twitter_word<-unlist(AI_twitter_nouns, use.name=F)
AI_twitter_word<-AI_twitter_word[-which(AI_twitter_word%in% stopwords("english"))]
AI_twitter_word<-gsub("[[:punct:]]","",AI_twitter_word)
AI_twitter_word<-Filter(function(x){nchar(x)>=4},AI_twitter_word)

AI_twitter_count<-table(AI_twitter_word)
pal<- brewer.pal(12,"Paired")

wordcloud(names(AI_twitter_count), freq=AI_twitter_count,scale=c(4,0.5), min.freq=1, random.order=F, rot.per=0.1, colors=pal, family="AppleGothic")
print(AI_twitter_count)


#손흥민
install.packages("twitteR")

install.packages("ROAuth")

install.packages("base64enc")

install.packages("devtools")

install.packages("htmlwidgets")

install.packages("htmltools")

install.packages("jsonlite")

install.packages("yaml")

install.packages("base64enc")

install.packages("tm")

install.packages("wordcloud2")

install.packages("KoNLP")

install.packages("wordcloud")

install.packages("plyr")

install.packages("bit")



library(twitteR)

library(ROAuth)

library(base64enc)

library(devtools)

library(htmlwidgets)

library(htmltools)

library(jsonlite)

library(yaml)

library(tm)

library(wordcloud2)

library(KoNLP)

library(wordcloud)

library(plyr)



consumerKey <- "B50g0f9ZjiezaKFh7pwHCvBeL"

consumerSecret <- "PgKFfKsHTVrWtJ06Hd4vNnsM99Tsiq2t9H26YZ4Bi6e3MxXDb4"

accessToken <- "1217342285475508225-ApfhiZ61er1R3bo7tImMkRdtXiHStS"

accessTokenSecret <- "FEVpThsVqLmKYLlQONptHOJWM6mJDsGSI82zV2uSV4oAC"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)



keyword <- enc2utf8("heungmin")

SON <- searchTwitter(keyword, n =5000, lang="en")

length(SON)

head(SON)



SON_twitter.df <- twListToDF(SON)

SON_twitter.text <- SON_twitter.df$text



write.table(SON_twitter.text, "tw.txt")



options(max.print=10000)

path <- paste0(getwd(),"/tw.txt")

tw_txt <- readLines(path)

tw_txt



last_number_of_lines <- length(readLines(path))

line_numbers <- seq(1, last_number_of_lines, 1)

doc_ids <- line_numbers

df <- data.frame(doc_id = doc_ids, text = tw_txt, stringsAsFactors = FALSE)

tw_data <- Corpus(DataframeSource(df))



inspect(tw_data)

text_delete = c("SON")

tw_data <- tm_map(tw_data, removeWords,text_delete)



Sys.setlocale(category = "LC_ALL", locale = "us")

tw_data <- tm_map(tw_data, stripWhitespace)

tw_data <- tm_map(tw_data, tolower)

tw_data <- tm_map(tw_data, removeNumbers)

tw_data <- tm_map(tw_data, removeWords, stopwords("english"))

tw_data <- tm_map(tw_data, removePunctuation)

inspect(tw_data)

stopwords("english")



tdm_tw <- TermDocumentMatrix(tw_data)

TDM1 <- as.matrix((tdm_tw))

v=sort(rowSums(TDM1), decreasing =T)

profile = data.frame(word=names(v), freq=v)

head(profile, 10)





path2 <- paste0(getwd(),"/tw4.csv")



write.csv(profile, path2)

data <- read.csv(path2)

data <- data[,-1]

data <- data[-1,]

data_pick <- subset(data, freq >= 10)

head(data_pick, 20)





in_out_colors = "function(word,weight){return(weight>100) ? '#F3EF12',:#1EC612}"

x11()

wordcloud2(data_pick, shape="star", size=0.3, backgroundColor = "white")
