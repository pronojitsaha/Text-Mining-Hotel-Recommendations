#For best and worst hotel
dataset = read.table("HotelReviews/hotel_218524.dat", sep = "\n", quote = "", stringsAsFactors=FALSE)
str(dataset)
odd_indexes = seq(1,nrow(dataset),2)
dataset = data.frame(tweet = dataset[odd_indexes,1])
dataset$tweet = gsub('<Content>','',dataset$tweet)
dataset$tweet = gsub('showReview.*','',dataset$tweet)
dataset$tweet = gsub('hotel','',dataset$tweet)


#For 5 best and worst hotels
best_file_list <- c('hotel_247957.dat', 'hotel_150841.dat', 'hotel_149399.dat', 'hotel_478252.dat', 'hotel_218524.dat')
worst_file_list <- c('hotel_85003.dat', 'hotel_100584.dat', 'hotel_252969.dat', 'hotel_305809.dat', 'hotel_306174.dat')
for (file in best_file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(paste("HotelReviews/",file, sep=""), sep = "\n", quote = "", stringsAsFactors=FALSE)
    dataset$hotel = gsub('\\..*','',file)
    odd_indexes = seq(1,nrow(dataset),2)
    dataset = data.frame(tweet = dataset[odd_indexes,1], hotel= dataset[odd_indexes,2])
    dataset$tweet = gsub('<Content>','',dataset$tweet)
    dataset$tweet = gsub('showReview.*','',dataset$tweet)
    dataset$tweet = gsub('hotel','',dataset$tweet)
    rm(odd_indexes)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <- read.table(paste("HotelReviews/",file, sep=""), sep = "\n", quote = "", stringsAsFactors=FALSE)
    temp_dataset$hotel = gsub('\\..*','',file)
    odd_indexes = seq(1,nrow(temp_dataset),2)
    temp_dataset = data.frame(tweet = temp_dataset[odd_indexes,1], hotel= temp_dataset[odd_indexes,2])
    temp_dataset$tweet = gsub('<Content>','',temp_dataset$tweet)
    temp_dataset$tweet = gsub('showReview.*','',temp_dataset$tweet)
    temp_dataset$tweet = gsub('hotel','',temp_dataset$tweet)
    dataset<-rbind(dataset, temp_dataset)
    rm(odd_indexes)
    rm(temp_dataset)
  }
  
}



library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(dataset$tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
frequencies = TermDocumentMatrix(corpus)
frequencies
findFreqTerms(frequencies, lowfreq=70)
term.freq <- rowSums(as.matrix(frequencies))
term_sub.freq <- subset(term.freq, term.freq >= 70) 
df <- data.frame(term = names(term_sub.freq),freq = term_sub.freq, stringsAsFactors=FALSE) 

library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + ggtitle('Best 5 Hotels Top Word Stems') + xlab("Word Stems") + ylab("Count") + coord_flip()


library(wordcloud) 
m <- as.matrix(frequencies) 
# calculate the frequency of words and sort it by frequency 
word.freq <- sort(rowSums(m), decreasing = T) 
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F) 
