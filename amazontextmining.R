library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)



review=read.csv(file.choose(), stringsAsFactors = FALSE)
head(review)
names(review)
## Make a vector source and a corpus
corpus_review=Corpus(VectorSource(review$reviews.text))
#data preprocessing
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
#Remove stopwords
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
# Remove context specific stop words
corpus_review=tm_map(corpus_review, removeWords,c("got","get","want","go","amazon", "fire","tv", "hd", "google", "play", "prime", "store", "i","just","tablet"))
## Stem document
corpus_review=tm_map(corpus_review, stemDocument)
##Viewing the corpus content
corpus_review[[8]][1]
# Find the 20 most frequent terms: term_count
term_count <- freq_terms(corpus_review, 20)
# Plot 20 most frequent terms
plot(term_count)
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)
# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]
# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:20], col = "orange", las = 2)
review_word_freq <- data.frame(term = names(review_term_freq),num = review_term_freq)
# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,max.words = 50, colors = "red")
# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))



















#CORPUS 1
review_yes=subset(review$reviews.text,review$reviews.doRecommend==1)
corpus_review_yes=Corpus(VectorSource(review_yes))
#data preprocessing
corpus_review_yes=tm_map(corpus_review_yes, tolower)
corpus_review_yes=tm_map(corpus_review_yes, removePunctuation)
#Remove stopwords
corpus_review_yes=tm_map(corpus_review_yes, removeWords, stopwords("english"))
# Remove context specific stop words
corpus_review_yes=tm_map(corpus_review_yes, removeWords,c("amazon", "fire","tv", "hd", "google", "play", "prime", "store", "i","just","tablet","go","get","can","one","need","buy","like"))
## Stem document
corpus_review_yes=tm_map(corpus_review_yes, stemDocument)
##Viewing the corpus content
corpus_review_yes[[8]][1]
# Find the 20 most frequent terms: term_count
term_count_n <- freq_terms(corpus_review_yes, 20)
# Plot 20 most frequent terms
plot(term_count_n)

p=data.frame(term_count_n)
head(p)
theme_set(theme_bw())
ggplot(p, aes(x=WORD, y=FREQ)) + 
  geom_bar(stat="identity", width=.5, fill="black") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Top Frequent Words When Product Is Recommended") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#CORPUS 2
review_no=subset(review$reviews.text,review$reviews.doRecommend==0)


corpus_review_no=Corpus(VectorSource(review_no))
#data preprocessing
corpus_review_no=tm_map(corpus_review_no, tolower)
corpus_review_no=tm_map(corpus_review_no, removePunctuation)
#Remove stopwords
corpus_review_no=tm_map(corpus_review_no, removeWords, stopwords("english"))
# Remove context specific stop words
corpus_review_no=tm_map(corpus_review_no, removeWords,c("got","get","want","go","amazon", "fire","tv", "hd", "google", "play", "prime", "store", "i","just","tablet"))
## Stem document
corpus_review_no=tm_map(corpus_review_no, stemDocument)
##Viewing the corpus content
corpus_review_no[[8]][1]
# Find the 20 most frequent terms: term_count
term_count_p <- freq_terms(corpus_review_no, 20)
# Plot 20 most frequent terms
library(RColorBrewer)
display.brewer.all()
plot(term_count_p,col="red")

p=data.frame(term_count_p)
head(p)
theme_set(theme_bw())
ggplot(p, aes(x=WORD, y=FREQ)) + 
  geom_bar(stat="identity", width=.5, fill="red") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Top Frequent Words When Product Is Not Recommended") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))





## Combine both corpora: all reviews
all_yes <- paste(review_yes, collapse = "")
all_no <- paste(review_no, collapse = "")
all_combine <- c(all_yes, all_no)
## Creating corpus for combination
corpus_review_all=Corpus(VectorSource(all_combine)) 
## Pre-processing corpus - all
#Convert to lower-case
corpus_review_all=tm_map(corpus_review_all, tolower)
#Remove punctuation
corpus_review_all=tm_map(corpus_review_all, removePunctuation)
#Remove stopwords
corpus_review_all=tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all=tm_map(corpus_review_all, removeWords,c("got","get","want","go","amazon", "fire","tv", "hd", "google", "play", "prime", "store", "i","just","tablet"))
#Stem document
corpus_review_all=tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Yes","No")
#Sum rows and frequency data frame
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)
#Make commonality cloud
commonality.cloud(all_m,colors = "steelblue1",max.words = 50)


# Create comparison cloud
comparison.cloud(all_m,colors = c("black", "red"),max.words = 50)





# Identify terms shared by both documents
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
# calculate common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],decreasing = T), ]
head(common_words)
head(all_m)

top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))
top25_df

# Make pyramid plot

pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 500,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Yes",
                            "Words",
                            "No")
             )
--------------------------------------------------------------------------
##Create bi-grams
review_bigram <- tokens(review$reviews.text) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 2) %>%
    dfm()
topfeatures(review_bigram)
features=c("fire_tv", "works_great", "year_old" ,"amazon_fire" ,"kindle_fire" ,"great_product","great_tablet","amazon_prime" ,"battery_life", "best_buy")
number=c(1380,1154,1153,1036,911,884,844,831,678,655)
data=data.frame(features,number)
data

library(ggplot2)
theme_set(theme_bw())

# Draw plot
ggplot(data, aes(x=features, y=number)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Top Features In A Bigram") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
#BIGRAM
review_bigram_yes <- tokens(subset(review$reviews.text,review$reviews.doRecommend==1)) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 2) %>%
    dfm()
topfeatures(review_bigram_yes)
features_bigram_yes=c("fire_tv", "works_great", "year_old" ,"amazon_fire" ,"kindle_fire" ,"great_product","great_tablet","amazon_prime" ,"battery_life")
numbers_bigram_yes=c(1303,1103,1106,982,831,869,840,781,651)
data_2=data.frame(features_bigram_yes,numbers_bigram_yes)
data_2

# Draw plot
ggplot(data_2, aes(x=features_bigram_yes, y=numbers_bigram_yes)) + 
  geom_bar(stat="identity", width=.5, fill="purple") + 
  labs(title="Ordered Bar Chart", 
       subtitle="top features in a bigram with recommendation=yes") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



review_bigram_no <- tokens(subset(review$reviews.text,review$reviews.doRecommend==0)) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 2) %>%
    dfm()
topfeatures(review_bigram_no)
features_bigram_no=c("app_store","black_friday","google_play","kindle_fire","fire_hd8","play_store","fire_hd","battery_life","can_get","new_one" )
numbers_bigram_no=c(9,5,5,5,5,4,4,4,4,4)
data_3=data.frame(features_bigram_no,numbers_bigram_no)
data_3
# Draw plot
ggplot(data_3, aes(x=features_bigram_no, y=numbers_bigram_no)) + 
  geom_bar(stat="identity", width=.5, fill="orange") + 
  labs(title="Ordered Bar Chart", 
       subtitle="top features in a bigram with recommendation=no") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))




##Create tri-grams
review_trigram <- tokens(review$reviews.text) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 3) %>%
    dfm()
topfeatures(review_trigram)
features_tri=c("fire_hd_8", "google_play_store","amazon_app_store", "kindle_fire_hd", "amazon_fire_hd", "amazon_fire_hd", "10_year_old", "8_year_old","amazon_prime_member","great_little_tablet")
numbers_tri=c(40,34,22,22,20,19,15,14,14,14)
data_1=data.frame(features_tri,numbers_tri)
data_1
ggplot(data_1, aes(x=features_tri, y=numbers_tri)) + 
  geom_bar(stat="identity", width=.5, fill="orange") + 
  labs(title="Ordered Bar Chart", 
       subtitle="top features in a trigram") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

##TRIGRAM
review_trigram_yes <- tokens(subset(review$reviews.text,review$reviews.doRecommend==1)) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 3) %>%
    dfm()
topfeatures(review_trigram_yes)
features_trigram_yes=c("amazon_fire_tv","3_year_old","google_play_store","amazon_fire_tablet","fire_tv_box","amazon_prime_member","year_old_son","year_old_daughter","4_year_old","5_year_old")
number_trigram_yes=c(381,118,117,114,109,107,102,102,100,100)
data_4=data.frame(features_trigram_yes,number_trigram_yes)
data_4

# Draw plot
ggplot(data_4, aes(x=features_trigram_yes, y=number_trigram_yes)) + 
  geom_bar(stat="identity", width=.5, fill="darkblue") + 
  labs(title="Ordered Bar Chart", 
       subtitle="top features in a trigram with recommendation=yes") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))




review_trigram_no<- tokens(subset(review$reviews.text,review$reviews.doRecommend==0)) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 3) %>%
    dfm()
topfeatures(review_trigram_no)
features_trigram_no=c("google_play_store","amazon_app_store","amazon_fire_tv","fire_hd_8","fire_tv_box","original_fire_tv","4_year_old","6_year_old","3_year_old","$_50_tablet")
number_trigram_no=c(24,10,8,7,7,7,6,6,6,6)
data_5=data.frame(features_trigram_no,number_trigram_no)
data_5
# Draw plot
ggplot(data_5, aes(x=features_trigram_no, y=number_trigram_no)) + 
  geom_bar(stat="identity", width=.5, fill="lightgreen") + 
  labs(title="Ordered Bar Chart", 
       subtitle="top features in a trigram with recommendation=no") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


----------------------------------------------------------------------------------





























--------------------------------------------------------------------------------

#SENTIMENT ANALYSIS
library(syuzhet)
dataset <- read.csv(file.choose(), head = TRUE)
names(dataset)
df <- dataset[sample(nrow(dataset)),]
review <- df$reviews.text
word.df <- as.vector(review)


sent.value <- get_sentiment(word.df)
head(sent.value)

category_sentii <- ifelse(sent.value > 0, "1", ifelse(sent.value < 0, "0", "0"))
head(category_sentii)

dataframe_final <- data.frame(df$reviews.doRecommend, df$reviews.rating, category_sentii)
df1 <- dataframe_final[complete.cases(dataframe_final),]
str(df1)

library(ggplot2)
ggplot(df1, aes(x = df.reviews.doRecommend)) + geom_bar(fill = "light blue")

prop.table(table(df1$df.reviews.doRecommend))


ggplot(df1, aes(x=df.reviews.rating, fill = df.reviews.doRecommend)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Customers",
       title = "Recommendation Rates")

ggplot(df1, aes(x=category_sentii, fill = df.reviews.doRecommend)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Customers",
       title = "Recommendation Rates")

library(caTools)
set.seed(123)
split = sample.split(df1$category_senti, SplitRatio = 0.8)
training_set = subset(df1, split == TRUE)
test_set = subset(df1, split == FALSE)
str(training_set)

logistic <- glm(df.reviews.doRecommend  ~  category_sentii + df.reviews.rating, 
                data = training_set, 
                family = "binomial")

summary(logistic)

training_set$prediction = predict(logistic,type= 'response')
head(prediction)

------------------------------------------------------
#--------------------------------------------------------------------- #SENSITIVITY AND SPECIFICITY FOR VARYING CUT VALUE #CREATIVITY AND SPECIFICITY FOR VARYING CUT VALUE #--------------------------------------------------------------------- 
cutpoints = seq(0,1,0.01) 
length(cutpoints)
 sensitivity = seq(1,101,1)
 specificity = seq(1,101,1) 
cutpoint_performance = cbind(cutpoints,sensitivity,specificity) 
table(training_set[,1])
 for(i in 1:101)
 { 
training_set$predicted = ifelse(training_set$prediction<cutpoint_performance[i,1],0,1)
training_set$sumed = training_set$predicted+training_set[,1]
training_set$pred1_1 = ifelse(training_set$sumed==2,1,0)
 correct1_1 = sum(training_set$pred1_1) 
training_set$pred0_0 = ifelse(training_set$sumed==0,1,0) 
correct0_0 = sum(training_set$pred0_0) 
cutpoint_performance[i,2] = correct1_1/2281 
cutpoint_performance[i,3] = correct0_0/87 }
 cutvalue_table = data.frame(cutpoint_performance) 
 
# Chart for sensitivity and specificity #---------------------------------------------------------------------
 cutpoint_performance=data.frame(cutpoint_performance)
 plot(cutpoint_performance$cutpoints, cutpoint_performance$sensitivity,"o",pch=8,col=6,main="Chart for sensitivity and specificity")
 lines(cutpoint_performance$cutpoints, cutpoint_performance$specificity,"o",pch=13,col="blue") 
 
# Optimum cut value #--------------------------------------------------------------------- 
 
cutvalue_table$diff = abs(cutvalue_table$sensitivity-cutvalue_table$specificity)
 head(cutvalue_table) 
optimum_value=subset(cutvalue_table,diff==min(diff))
 optimum_value 
training_set$pre_class=ifelse(training_set$prediction<0.95,0,1)
 
#Confusion Matrix #--------------------------------------------------------------------- 
attach(training_set)
 tble=table(df.reviews.doRecommend,pre_class) 
tble 

prop.table(tble,1)
overall_perf=(sum(prop.table(tble,1)[1,1],prop.table(tble,1)[2,2]))/2 
overall_perf*100
error_rate=(sum(prop.table(tble,1)[1,2],prop.table(tble,1)[2,1]))/2 
error_rate*100






























