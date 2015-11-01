file_list <- list.files("HotelReviews/")
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(paste("HotelReviews/",file, sep=""), sep = "\n", quote = "", stringsAsFactors=FALSE)
    dataset$hotel = gsub('\\..*','',file)
    odd_indexes = seq(1,nrow(dataset),2)
    dataset = data.frame(tweet = dataset[odd_indexes,1], hotel= dataset[odd_indexes,2])
    dataset$tweet = gsub('<Content>','',dataset$tweet)
    dataset$tweet = gsub('showReview.*','',dataset$tweet)
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
    dataset<-rbind(dataset, temp_dataset)
    rm(odd_indexes)
    rm(temp_dataset)
  }
  
}
write.table(dataset,'data')
