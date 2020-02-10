rm(list =ls())
#libraries for tweet collection and sentiment analysis
if(!require("wordcloud")) install.packages("wordcloud"); library("wordcloud")
if(!require("scales")) install.packages("scales"); library("scales")
if(!require("tidytext")) install.packages("tidytext"); library("tidytext")

if(!require("textstem")) install.packages("textstem"); library("textstem")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("textdata")) install.packages("textdata");library("textdata")
#graphs libraries
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
if (!require("igraph")) install.packages("igraph", quiet=TRUE) ; require("igraph")
if (!require("ggraph")) install.packages("ggraph", quiet=TRUE) ; require("ggraph")
#Libraries for topic modelling 
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
#translation package
if (!require("textcat")) install.packages("textcat", quiet=TRUE) ; require("textcat")
#Word cloud of reply tweets
if (!require("wordcloud2")) install.packages("wordcloud2", quiet=TRUE) ; require("wordcloud2")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")

#Access information from twitter developer 
appname= "abdon social"
consumer_key="g5sx66aN6WPLRT9tlCKliR91i "
consumer_secret="OHIGPAnJQKsr1krOahfinmQSJCbVRFANsoJswoD69zbJFBUMTA"
access_token="1218209408137777152-n3zMrX00P69WW2DETzUU2F1QcBBNfD"
access_secret="uTIrBHhH62o4bUz41FM00oBalkbZ5Gr4xPV7bCqCyOidi"

#Token creation
twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv=FALSE)

#tweets of southwest 
# southwesttweets <- search_fullarchive(q = "@SouthwestAir",
#                                       n = 5000, env_name = 'SMA1Archive',
#                                       token = twitter_token,
#                                       fromDate="201906010000",toDate="202001100000")

#save(southwesttweets,file="C:/Users/aahuile/Documents/GitHub/social_media_project/southwesttweets.Rdata")
load("C:/Users/spavot/Documents/GitHub/social_media_project/southwesttweets.Rdata")

#filter and remove the post from soutwest (just leave the ones from the people)
southwesttweets <- subset(southwesttweets, screen_name!="	SouthwestAir")
#apparently there where no screen_names from soutwest but we still do it to make sure 

#Now removal of the mispellings 
southwesttweets$text <-  gsub("https\\S*", "", southwesttweets$text)
southwesttweets$text <-  gsub("@\\S*", "", southwesttweets$text) 
southwesttweets$text  <-  gsub("amp", "", southwesttweets$text) 
southwesttweets$text  <-  gsub("[\r\n]", "", southwesttweets$text)
southwesttweets$text  <-  gsub("[[:punct:]]", "", southwesttweets$text)


# tokenization and removal of stop words of southwesttweets
southwesttweets_token <- southwesttweets %>%
  select(text) %>%
  unnest_tokens(word, text)

southwesttweets_token <- southwesttweets_token %>%
  anti_join(stop_words)

#Sentiment analysis of post to southwest
get_sentiments("bing") %>% 
  count(sentiment)

#CHECK WITH ABDON
southwesttweets_token  <- inner_join(southwesttweets_token,get_sentiments("bing"))

tweets_to_southwest_summary <-southwesttweets_token %>%  
  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)



tweets_to_southwest_summary%>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

tweets_southwest <- rtweet::search_tweets(q = "@southwest",
                                          n = 500,include_rts = FALSE)
#SOuthwest tweets
?get_timeline
Southy <- get_timeline("@SouthwestAir", n= 5000)
#save(Southy,file="C:/Users/aahuile/Documents/GitHub/social_media_project/Southy.Rdata")
southy_noretweets <- Southy[Southy$is_retweet==FALSE, ] 
southy_noretweets <- subset(southy_noretweets, is.na(southy_noretweets$reply_to_status_id))

southy_noretweets <- southy_noretweets%>% arrange(-favorite_count)
southy_noretweets[1,5]

southy_noretweets <- southy_noretweets %>% arrange(-retweet_count)
southy_noretweets[1,5]

?get_timeline
# Keeping only the retweets
Southy_retweets<- Southy[Southy$is_retweet==TRUE,]
# Keeping only the replies
southy_replies <- subset(Southy, !is.na(Southy$reply_to_status_id))


#removal of noice and punctuation in the southy_replies data frame
southy_replies$text <-  gsub("https\\S*", "", southy_replies$text)
southy_replies$text <-  gsub("@\\S*", "", southy_replies$text) 
southy_replies$text  <-  gsub("amp", "", southy_replies$text) 
southy_replies$text  <-  gsub("[\r\n]", "", southy_replies$text)
southy_replies$text  <-  gsub("[[:punct:]]", "", southy_replies$text)

# tokenization and removal of stop words
tweets <- southy_replies %>%
  select(text) %>%
  unnest_tokens(word, text)

tweets <- tweets %>%
  anti_join(stop_words)

#most frequent words
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill="blue") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the replies made by southwest",
       subtitle = "Stop words removed from the list")


#sentiment analysis of replies from southwest 

get_sentiments("bing") %>% 
  count(sentiment)

SouthSentiment <- inner_join(tweets,get_sentiments("bing"))

SouthSentimentSummary <-SouthSentiment %>%  
  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)


wordcloud_second <- SouthSentimentSummary
wordcloud_second$sentiment <- NULL

wordcloud2(data=wordcloud_second,size = 0.3, shape = 'star')

SouthSentimentSummary %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


dev.off()


#followers of southwest airlines random sample of 10000
#We comment this part because we don't always get list from the followers every time we run it:

load("C:/Users/spavot/Documents/GitHub/social_media_project/memberships_southy.Rdata")

# followers_southwest <- get_followers("SouthwestAir")
# info_followers <- lapply(followers_southwest[1:10,],lookup_users)$user_id
# southyfol<-data.frame(info_followers)
#memberships_southy<-do_call_rbind(lapply(southyfol[1:50,1],lists_memberships,n=50))
#save(memberships_southy,file="C:/Users/aahuile/Documents/GitHub/social_media_project/memberships_southy.Rdata")

#Topic modelling for the memberships 
memberships_southy$name <-  gsub("@\\S*", "", memberships_southy$name)

#detect the language per column "name"
memberships_southy$language <- textcat(memberships_southy$name)

cloud_languages <- termFreq(memberships_southy$language)
wordcloud(names(cloud_languages),cloud_languages,
          max.words=40,
          scale=c(1,1),colors=brewer.pal(8, "Dark2"),random.order=FALSE)


#get the memberships


#tweets from competition (random sampling)
tweets_delta <- rtweet::search_tweets(q = c("@Delta Southwest"),
                                      n = 500,include_rts = FALSE)

tweets_american <- rtweet::search_tweets(q = c("@AmericanAir Southwest"),
                                         n = 500,include_rts = FALSE)

tweets_united <- rtweet::search_tweets(q = c("@united Southwest"),
                                       n = 500,include_rts = FALSE)

tweets_skywest <- rtweet::search_tweets(q = c("@skywest Southwest"),
                                        n = 500,include_rts = FALSE)

tweets_jetblue <- rtweet::search_tweets(q = c("@JetBlue Southwest"),
                                        n = 500,include_rts = FALSE)

tweets_comparisons <- rbind(tweets_delta,tweets_american,tweets_united, tweets_skywest, tweets_jetblue)

tweets_comparisons  <- mutate(tweets_comparisons , text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

comparison_tokenized <- tweets_comparisons %>% unnest_tokens(output = "word",
                                                             input = text,
                                                             token = "words",
                                                             drop=FALSE,to_lower=TRUE,collapse=FALSE)


comparison_tokenized <- comparison_tokenized %>%
  anti_join(stop_words) %>%      
  count(status_id,word , sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)

lda_comparison <- LDA(comparison_tokenized, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )

comparison_topics <- tidy(lda_comparison, matrix = "beta")

#top terms per topic
top_comparison_terms <- comparison_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_comparison_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Make topic analysis with bigram (which doesnt work)


comp_tok_dtm <- tweets_comparisons %>% unnest_tokens(output = "bigram",
                                                     input = text,
                                                     token = "ngrams",n=2,
                                                     drop=FALSE,to_lower=TRUE,collapse=FALSE)

#separate the bigram by word 1 and 2 
comtok_separated <- comp_tok_dtm  %>%
  separate(bigram, c("word1", "word2"), sep = " ")


#filter and remove stop words 
comtok_filtered <- comtok_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# count frequency of words
comtok_count <- comtok_filtered %>%
  count(word1, word2, sort = TRUE)

#words linked to competition
words_1 <- c("delta", "united", "jetblue", "americanair","fly")


#bigram network
bigram_network <- comtok_count %>%
  filter(word1 %in% words_1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_network, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightgreen", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#Hashtag analysis

twitter_token = create_token(
  app = 'SentimentPAVOT',
  consumer_key = 'U7aN2NZS5OpZxoZXyjH01XCnJ',
  consumer_secret = 'CTClVhGcfqiYZWYU3i0lCPcMRoo63BajYMJoUlreLCu6BOPL5V',
  access_token = '899588958-kLeVx2uqmMWgIxwD0yuvXIgt2hxIAKgEcI2UhXGr',
  access_secret = 'KGFE6fK13wyftmkegEtAfjNpNXoVkdQp8v0YTXJ0iVjU4',
  set_renv=FALSE)


#Step 2: Data Collection
#Here, we're going to get the data from 3 differents hashtag and stack them in a dataset. 
#We will add flag column to identify from which hashtag come which tweets so we will be able to perform 
#individual hashtag analysis and grouped analysis:


load("C:/Users/spavot/Documents/GitHub/social_media_project/30Day#.Rdata")

#SouthWestAirlines Hashtag:

hashSouthWestAirlines = hashSouth


hashSouthWestAirlines['Hashtag'] = '#southwestairlines'



#SouthWestair hashtag


# hashSouthWestAir = rtweet::search_30day(q = "#southwestair",
#                                          n = 2000, env_name = "30DayTweet", token = twitter_token)
# save(hashSouthWestAir,file = "C:/Users/spavot/Documents/GitHub/social_media_project/southwestAir30Day#.Rdata")
load(file = "C:/Users/spavot/Documents/GitHub/social_media_project/southwestAir30Day#.Rdata")


hashSouthWestAir['Hashtag'] = '#southwestair'

#SouthWest hashtag
# HashSouthWest = rtweet::search_30day(q = "#southwest", 
#                                      n = 2000, env_name = "30DayTweet", token = twitter_token)
#save(HashSouthWest,file = "C:/Users/spavot/Documents/GitHub/social_media_project/southwest30Day#.Rdata")
load(file = "C:/Users/spavot/Documents/GitHub/social_media_project/southwest30Day#.Rdata")


HashSouthWest = subset(HashSouthWest,!(HashSouthWest$text %in% c('flight','plane','departure','@southwestair','arlines')))
HashSouthWest['Hashtag'] = '#southwest'


#save(hashSouth,file = "C:/Users/spavot/Documents/GitHub/social_media_project/30Day#.Rdata")


#Step3: Stacking data
#We stack all the different hashtag dataset in one stacked datasetHhashtag

Hashtag = rbind(hashSouthWestAir,hashSouthWestAirlines,HashSouthWest)
HashTagForGlob = Hashtag
#Step 4: PreProcessing data
#First we get the location: 

Hashtag <- lat_lng(Hashtag)

sum(is.na(Hashtag$lat))

#Then we will remove the hashtag:
#Remove hashtag
Hashtag$text <- gsub("#\\w+", "", Hashtag$text)

#Before removing special characters, we want to remove link and put them instead in a special column
#in order to be able to analyze them later if needed

#We extract link in a new column:
Hashtag$Link <- str_extract(Hashtag$text, "http\\S+")

#And now remove them from the text column:
Hashtag$text = gsub("http\\S+", "", Hashtag$text)

#We will put everyword to lower in order to avoid some errors:

Hashtag$text = tolower(Hashtag$text)

#We remove also the brand word as we don't need it for the analyzis:
Hashtag$text = gsub("southwest", "", Hashtag$text)
Hashtag$text = gsub("southwestair", "", Hashtag$text)
Hashtag$text = gsub("southwestairlines", "", Hashtag$text)

sum(Hashtag$text =='southwestair')

#Now we remove all special characters:

Hashtag <- mutate(Hashtag, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

HashtagUntoken = Hashtag
HashtagUntoken['hour'] = hour(HashtagUntoken$created_at)
HashtagUntoken['day'] = day(HashtagUntoken$created_at)
HashtagUntoken['week'] = week(HashtagUntoken$created_at)
HashtagUntoken['month'] = month(HashtagUntoken$created_at)

#save(HashtagUntoken, file = "C:/Users/spavot/Desktop/Big Data/SMA/HashtagUntoken.Rdata")

#We can now tokenized by word:
#Tokenized by word
Hashtag <- Hashtag %>% unnest_tokens(output = "word", # how should the new column be named?
                                     input = text, # where can we find the text? 
                                     token = "words", # which tokenization scheme should we follow?
                                     drop=FALSE,to_lower=TRUE)

#We now remove the stopwords as we don't need them for the analysis
#Remove stopwords
Hashtag <- Hashtag %>% anti_join(get_stopwords())

#Before analyzing, we're going to use lemmatization as it's working better
#than stemming in our case:

lemma_tagged <- lemmatize_words(Hashtag$word)

#Now we have our word "cleaned", we replace the word column with the clean word:
Hashtag$word = lemma_tagged

#Before going in analysis and visualization, we will first add columns in 
#the merged hashtag relative to hour / day / week / month in order to perform
#some time analysis later:

Hashtag['hour'] = hour(Hashtag$created_at)
Hashtag['day'] = day(Hashtag$created_at)
Hashtag['week'] = week(Hashtag$created_at)
Hashtag['month'] = month(Hashtag$created_at)

#Sentiment analysis:
#For our first analyse, we will use the bing dictionnary:
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

#We do another dataset equal to all hashtag combined:

FullHashSentiment = Hashtag

FullHashSentiment <- left_join(FullHashSentiment,get_sentiments("bing"))
save(FullHashSentiment, file = "C:/Users/spavot/Desktop/Big Data/SMA/fullhashsentiment.Rdata")

#Sentiment Analysis = afinn Dictionnary:
get_sentiments(('afinn'))


FullHashSentimentAff = Hashtag
FullHashSentimentAff <- inner_join(FullHashSentimentAff,get_sentiments("afinn"))

save(FullHashSentimentAff, file = "C:/Users/spavot/Desktop/Big Data/SMA/fullhashsentimentaff.Rdata")

#Topic Analysis:

#Global
TopicAnalysisHash <- Hashtag %>%
  count(status_id,word , sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)

HashtagLDA <- LDA(TopicAnalysisHash, k = 4,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
save(HashtagLDA, file = "C:/Users/spavot/Desktop/Big Data/SMA/HashtagLDA.Rdata")

#For each hashtag
#Soutwest

TopicAnalysisHashSouthWest <- Hashtag[Hashtag$Hashtag == "#southwest",] %>%
  count(status_id,word , sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)

HashtagLDASouthWest <- LDA(TopicAnalysisHashSouthWest, k = 4,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
save(HashtagLDASouthWest, file = "C:/Users/spavot/Desktop/Big Data/SMA/HashtagLDASouthWest.Rdata")

#Southwestair
TopicAnalysisHashSouthWestAir <- Hashtag[Hashtag$Hashtag == "#southwestair",] %>%
  count(status_id,word , sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)

HashtagLDASouthWestAir <- LDA(TopicAnalysisHashSouthWestAir, k = 4,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
save(HashtagLDASouthWestAir, file = "C:/Users/spavot/Desktop/Big Data/SMA/HashtagLDASouthWestAir.Rdata")


#Southwestairlines

TopicAnalysisHashSouthwestairlines <- Hashtag[Hashtag$Hashtag == "#southwestairlines",] %>%
  count(status_id,word , sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)

HashtagLDASouthwestairlines <- LDA(TopicAnalysisHashSouthwestairlines, k = 4,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
save(HashtagLDASouthwestairlines, file = "C:/Users/spavot/Desktop/Big Data/SMA/HashtagLDASouthwestairlines.Rdata")

#Unmentioned

#save(hw,file="C:/Users/lenovo/Desktop/Social media analytics/Data/southwest_final_data.Rdata")
load(file = "C:/Users/spavot/Documents/GitHub/social_media_project/southwest_final_data.Rdata")

for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

hw_southwest <- mutate(hw, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

hw_tokenized <- hw_southwest %>% unnest_tokens(output = "word",
                                               input = text,
                                               token = "words",
                                               drop=FALSE,to_lower=TRUE)
get_sentiments("bing") %>%
  count(sentiment)

################################  
#sentiment analysis
###############################
hw_sentiment <- inner_join(hw_tokenized,get_sentiments("bing"))

summarySentiment <- hw_sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

par(oma=c(0,4,0,0),mfrow=c(1,2))
barplot(summarySentiment[summarySentiment$sentiment == "negative",3],names.arg = summarySentiment[summarySentiment$sentiment == "negative",1],horiz=TRUE,las=1,xlim=c(0,15),col="red",axes=TRUE)                
barplot(summarySentiment[summarySentiment$sentiment == "positive",3],names.arg = summarySentiment[summarySentiment$sentiment == "positive",1],horiz=TRUE,las=1,xlim=c(0,15),col="green")                


############ Top sentiment words drawed in ggplot2 :

if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

########v Get a summary per post
statusSentiment <- hw_sentiment %>%
  count(status_id, sentiment) %>%                
  spread(sentiment, n, fill = 0) %>%      
  mutate(sentiment = positive - negative)

########## Get a summary per post
statusSentiment <- inner_join(hw_tokenized,get_sentiments("afinn")) %>%
  group_by(status_id) %>%                      
  summarize(sentiment = sum(value))
mean(statusSentiment$sentiment)


#topic modeling
for (i in c('SnowballC','slam','tm','Matrix','dplyr','tidytext')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

##############################
#topic modeling
##############################
hw_tokenized <- hw_tokenized %>%
  anti_join(stop_words) %>%       # note that we use all stopword dictionaries here
  count(status_id,word, sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)

if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")

tweets_lda <- LDA(hw_tokenized, k = 4,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
tweets_lda
tweet_topics <- tidy(tweets_lda, matrix = "beta")

#Top terms per topic
top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

##########################
#document division
##########################
tweet_documents <- tidy(tweets_lda, matrix = "gamma")
tweet_doc_topic <- tweet_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1)
tweet_doc_topic %>%
  group_by(topic) %>%
  summarise(nbr_documents = n())

#######################
#Analysis through time
#######################

######data source filter
hw_source <-hw %>%
  select(status_id, source,text,created_at, is_quote) %>%
  filter(source %in%c("Twitter for Android", "Twitter for iPhone", "Twitter Web App"))

######data source hour plot
hw_source%>%
  count(source,hour=hour(with_tz(created_at,"EST")))%>%
  mutate(percent=n/sum(n))%>%
  ggplot(hw_source,mapping=aes(x=hour,y=percent,color=source))+
  geom_line()+
  scale_y_continuous(labels=percent_format())+
  labs(x="Hour of day", y="% of posts", color="")+
  ggtitle('Time of the day poeple tweet')

######data source day plot
hw_source%>%
  count(source,day=day(created_at))%>%
  mutate(percent=n/sum(n))%>%
  ggplot(hw_source,mapping=aes(x=day,y=percent,color=source))+
  geom_line()+
  scale_y_continuous(labels=percent_format())+
  labs(x="Days in months", y="% of posts", color="")+
  ggtitle('Days in months poeple tweet')

######data source weekday plot
hw_source%>%
  count(source,wday=wday(created_at))%>%
  mutate(percent=n/sum(n))%>%
  ggplot(hw_source,mapping=aes(x=wday,y=percent,color=source))+
  geom_line()+
  scale_y_continuous(labels=percent_format())+
  labs(x="Days in a week", y="% of posts", color="")+
  ggtitle('Days in a week poeple tweet')

#######data source months of the year plot
hw_source%>%
  count(source,month=month(created_at))%>%
  mutate(percent=n/sum(n))%>%
  ggplot(hw_source,mapping=aes(x=month,y=percent,color=source))+
  geom_line()+
  scale_y_continuous(labels=percent_format())+
  labs(x="Months of the year", y="% of posts", color="")+
  ggtitle('Months of the year poeple tweet')

#######post quote or not plot 
hw_source%>%
  count(source,is_quote)%>% 
  ggplot(hw_source,mapping=aes(x=source, y=n, fill=is_quote)) +
  geom_bar(stat ="identity", position ="dodge") +
  guides(fill=FALSE) +
  labs(x ="Data source", y ="Number of tweets", fill ="") +
  ggtitle('Whether tweets are quoted')


#Global Analysis:

#First, merge the tweets we collected from the hashtag, mentions and unmentions:
HashTagForGlob$Hashtag = NULL
GlobalTweets = rbind(HashTagForGlob,hw,Southy,southwesttweets )


GlobalTweets <- lat_lng(GlobalTweets)

#Then we will remove the hashtag:
#Remove hashtag
GlobalTweets$text <- gsub("#\\w+", "", GlobalTweets$text)

#Before removing special characters, we want to remove link and put them instead in a special column
#in order to be able to analyze them later if needed


#And now remove them from the text column:
GlobalTweets$text = gsub("http\\S+", "", GlobalTweets$text)

#We will put everyword to lower in order to avoid some errors:

GlobalTweets$text = tolower(GlobalTweets$text)

#We remove also the brand word as we don't need it for the analyzis:
GlobalTweets$text = gsub("southwest", "", GlobalTweets$text)
GlobalTweets$text = gsub("southwestair", "", GlobalTweets$text)
GlobalTweets$text = gsub("southwestairlines", "", GlobalTweets$text)


#Now we remove all special characters:

GlobalTweets <- mutate(GlobalTweets, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


#save(HashtagUntoken, file = "C:/Users/spavot/Desktop/Big Data/SMA/HashtagUntoken.Rdata")

#We can now tokenized by word:
#Tokenized by word
GlobalTweetsToken <- GlobalTweets %>% unnest_tokens(output = "word", # how should the new column be named?
                                     input = text, # where can we find the text? 
                                     token = "words", # which tokenization scheme should we follow?
                                     drop=FALSE,to_lower=TRUE)

#We now remove the stopwords as we don't need them for the analysis
#Remove stopwords
GlobalTweetsToken <- GlobalTweetsToken %>% anti_join(get_stopwords())

#Before analyzing, we're going to use lemmatization as it's working better
#than stemming in our case:

lemma_tagged <- lemmatize_words(GlobalTweetsToken$word)

#Now we have our word "cleaned", we replace the word column with the clean word:
GlobalTweetsToken$word = lemma_tagged



GlobalTweetsToken['hour'] = hour(GlobalTweetsToken$created_at)
GlobalTweetsToken['day'] = day(GlobalTweetsToken$created_at)
GlobalTweetsToken['week'] = week(GlobalTweetsToken$created_at)
GlobalTweetsToken['month'] = month(GlobalTweetsToken$created_at)

save(GlobalTweetsToken, file =  "C:/Users/spavot/Documents/GitHub/GlobalTweetsToken.Rdata")
save(GlobalTweets, file =  "C:/Users/spavot/Documents/GitHub/GlobalTweets.Rdata")



get_sentiments("bing") %>% 
  count(sentiment)

GlobalSentiment = GlobalTweetsToken

GlobalSentiment <- left_join(GlobalSentiment,get_sentiments("bing"))
save(GlobalSentiment, file = "C:/Users/spavot/Desktop/Big Data/SMA/GlobalSentiment.Rdata")

#Sentiment Analysis = afinn Dictionnary:
get_sentiments(('afinn'))


GlobalSentimentAff = GlobalTweetsToken
GlobalSentimentAff <- inner_join(GlobalSentimentAff,get_sentiments("afinn"))

save(GlobalSentimentAff, file = "C:/Users/spavot/Desktop/Big Data/SMA/GlobalSentimentAff.Rdata")

