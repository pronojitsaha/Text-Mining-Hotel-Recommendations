dataset = read.table('rank_set',stringsAsFactors=FALSE)

boxplot(dataset$score) 
#The following shows outliers start around the score of 1600
out = boxplot(dataset$score, plot = FALSE)$out
range(dataset[dataset$score %in% out,'score'])

#creating a data frame containing the outliers
outliers = dataset[dataset$score > 1600,]
boxplot(outliers$score) 
#setting the extreme outlier values as equal
nrow(dataset[dataset$score > 6000,])
outliers[outliers$score > 6000,'score'] = 6000
#giving ratings to these outliers between 4 and 5
range(outliers$score)
min = min(outliers$score)
outliers$score = outliers$score - min
range(outliers$score)
max = max(outliers$score)
outliers$ranks = outliers$score/max
outliers$ranks = outliers$ranks + 4
boxplot(outliers$ranks)

#creating a data frame for the rest of in-range data points
range = dataset[!(dataset$score > 1600),]
range(range$score)
boxplot(range$score)
#assigning rating to them in the range of 0 to 4
min = min(range$score)
range$score = range$score - min
range(range$score)
max = max(range$score)
range$ranks = range$score/max
range$ranks = range$ranks*4
boxplot(range$ranks)

#combining the two data frames
dataset = rbind(outliers, range)
boxplot(dataset$ranks)


submission = read.table('HotelRatingSubmissionTemplate.csv', sep = ',')
submission$ID = seq(1,1500,1)
merged = merge(submission[c('V1','ID')], dataset[c('hotel','ranks')], by.x='V1', by.y = 'hotel', all.x = TRUE)
str(merged)
colSums(is.na(merged)) #check for data consistency
merged = merged[order(merged$ID),] #order data in the submission file sequence
write.table(merged[c('V1','ranks')], 'submission.csv', sep = ",", row.names = FALSE, col.names = FALSE)
