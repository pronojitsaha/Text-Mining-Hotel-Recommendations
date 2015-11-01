dataset = read.table('data',stringsAsFactors=FALSE)

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(dataset$tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

#Model Building
pos.words = read.csv('positive_words.txt')
neg.words = read.csv('negative_words.txt')

library(stringr)
for (i in 1:nrow(dataset)){
  # make a list of words in the tweet
  word.list = str_split(corpus[[i]], "\\s+")
  words = unlist(word.list)
  # compare words to the dictionaries of positive & negative terms
  pos.matches = match(words, pos.words$a)
  neg.matches = match(words, neg.words$X2)
  # we just want a TRUE/FALSE
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  # compute score
  dataset$score[i] = sum(pos.matches) - sum(neg.matches)
  
  # assign sentiment polarity based on scores
  dataset$sentiment[i] = ifelse(dataset$score[i]>=1,'positive', ifelse(dataset$score[i]<=-1, 'negative', 'neutral'))    
}

#top 5
tail(sort(tapply(dataset$score, dataset$hotel, sum)), 5)

#bottom 5
head(sort(tapply(dataset$score, dataset$hotel, sum)), 5)
