## Script Twitter sentiment COD Warzone - Data Scientist project
##https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967
## https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

 




library (rtweet)
library (dplyr)
library (tidyr)
library (tidytext)
library (purrr)
library(ggplot2)

token <- create_token(app = "codwz", consumer_key = api_key, consumer_secret = api_secret_key, access_token  = access_token, access_secret = access_token_secret )

test <- search_tweets("Warzone", n=100, include_rts = FALSE)
test <- search_tweets("Warzone", n=100, include_rts = FALSE)
tweets.test = test %>% select(screen_name, text)

head(tweets.test$text)
#quita los http 
tweets.test$stripped_text1 <- gsub("http\\S+","", tweets.test$text)
#convert to lowercase, remove punctuation and add id to each tweet
tweets.test_stem <- tweets.test%>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.test_stem)
#remove stop words
cleaned_tweets.test <- tweets.test_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.test)

head(tweets.test$text)

##Top 10 Words in Warzone
cleaned_tweets.test %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x= word, y = n)) +
  geom_col () +
  xlab(NULL)+
  coord_flip()+
  theme_classic()+
  labs(x = "Count", y = "Unique words", title = "Unique Word counts found in #Warzone tweets")

#bing sentiment analysis
bing_test = cleaned_tweets.test %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#sentiment score for every tweet
 sentiment_bing = function (twt) {
   #step 1 basic text cleaning
   twt_tbl = tibble(text = twt) %>%
     mutate(
       #quita los http
       stripped_text = gsub("http\\S+","",text)
     ) %>%
     unnest_tokens(word,stripped_text) %>%
     anti_join(stop_words) %>% #remove stop words
     inner_join(get_sentiments("bing")) %>% # merge with bing sentiment
     count(word, sentiment, sort = TRUE) %>%
     ungroup()%>%
     ##create column score, -1 for every negative and +1 for every positive word
     mutate(
       score = case_when(
         sentiment == 'negative'~ n*(-1),
         sentiment == 'positive'~ n*1)
     )
   ##calculate tota score
   sent.score = case_when(
     nrow(twt_tbl)==0 ~0, # if there are no words, score is 0
     nrow(twt_tbl)>0 ~sum(twt_tbl$score) # otherwise sum positives and negatives
   )
   ##keep track of which tweets contain no words
   zero.type = case_when(
     nrow(twt_tbl)==0~ "Type1", 
     nrow(twt_tbl)>0~ "Type2" 
   )
   list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
 }
 
#Apply the function with lapply
test_sent = lapply(test$text, function(x){sentiment_bing(x)})
test_sent

test_sentiment = bind_rows(
  tibble(
    WZ = "Warzone",
    score = unlist(map(test_sent, 'score')),
    type = unlist(map(test_sent, 'type'))
  )
)

test_sentiment

ggplot(test_sentiment, aes(x=score, fill = WZ)) + geom_histogram(bins = 15, alpha = .6) + facet_grid(~WZ) + theme_bw()





   