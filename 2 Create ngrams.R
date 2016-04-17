## create ngrams (unigram to quadgram) and save in an rda file

library(tidyr)

## set the work directory on local 
setwd("C:/RStudio/capstone/final/en_US")

## open saveddata
sampleCorp <- readLines("samplecorpfilenames.txt")

source("ngramstokenizer.R")

## Unigram Analysis
unigram.tokenizer <- ngram_tokenizer(1)
unigrams <- unigram.tokenizer(sampleCorp)
unigram.df <- data.frame(V1 = as.vector(names(table(unlist(unigrams)))), V2 = as.numeric(table(unlist(unigrams))))
names(unigram.df) <- c("word","frequency")
unigram.df <- unigram.df[with(unigram.df, order(-unigram.df$frequency)),]
row.names(unigram.df) <- NULL

## Bigram Analysis
bigram.tokenizer <- ngram_tokenizer(2)
bigrams <- bigram.tokenizer(sampleCorp)
bigram.df <- data.frame(V1 = as.vector(names(table(unlist(bigrams)))), V2 = as.numeric(table(unlist(bigrams))))
bigram.df <- bigram.df[bigram.df[,2]>2,]
bigram.df <- separate(data = bigram.df, col = V1, into = c("word1", "word2"), sep = " ")
names(bigram.df) <- c("word1", "word2", "frequency")
row.names(bigram.df) <- NULL

bigram.df <- merge(bigram.df, unigram.df, by.x = "word1", by.y = "word")
bigram.df <- cbind(bigram.df, bigram.df[,3]/bigram.df[,4])
bigram.df <- bigram.df[,c(1,2,3,5)]
names(bigram.df) <- c("word1", "word2", "frequency", "probability")
bigram.df <- bigram.df[order(bigram.df$probability, decreasing=TRUE), ];

## Trigram Analysis
trigram.tokenizer <- ngram_tokenizer(3)
trigrams <- trigram.tokenizer(sampleCorp)
trigram.df <- data.frame(V1 = as.vector(names(table(unlist(trigrams)))), V2 = as.numeric(table(unlist(trigrams))))
trigram.df <- separate(data = trigram.df, col = V1, into = c("word1", "word2", "word3"), sep = " ")
names(trigram.df) <- c("word1", "word2", "word3", "frequency")
row.names(trigram.df) <- NULL

trigram.df <- merge(trigram.df, bigram.df[,c("word1","word2","frequency")], by = c("word1", "word2"))
trigram.df <- cbind(trigram.df, trigram.df[,4]/trigram.df[,5])
trigram.df <- trigram.df[,c(1,2,3,4,6)]
names(trigram.df) <- c("word1", "word2", "word3", "frequency", "probability")
trigram.df <- trigram.df[order(trigram.df$probability, decreasing=TRUE), ];

## quadgram Analysis 
quadgram.tokenizer <- ngram_tokenizer(4) 
quadgrams <- quadgram.tokenizer(sampleCorp) 
quadgram.df <- data.frame(V1 = as.vector(names(table(unlist(quadgrams)))), V2 = as.numeric(table(unlist(quadgrams)))) 
quadgram.df <- separate(data = quadgram.df, col = V1, into = c("word1", "word2", "word3", "word3"), sep = " ")
names(quadgram.df) <- c("word1","word2","word3", "word4", "frequency") 
row.names(quadgram.df) <- NULL 


quadgram.df <- merge(quadgram.df, trigram.df[,c("word1","word2","word3", "frequency")], by = c("word1", "word2", "word3")) 
quadgram.df <- cbind(quadgram.df, quadgram.df[,5]/quadgram.df[,6]) 
quadgram.df <- quadgram.df[,c(1,2,3,4,5,7)] 
names(quadgram.df) <- c("word1", "word2", "word3","predicted", "frequency", "probability") 
quadgram.df <- quadgram.df[order(quadgram.df$probability, decreasing=TRUE), ]; 

names(bigram.df) <- c("word1", "predicted", "frequency", "probability")
names(trigram.df) <- c("word1", "word2", "predicted", "frequency", "probability")
names(quadgram.df) <- c("word1", "word2", "word3","predicted", "frequency", "probability")

##save all the ngrams for use in prediction
save(unigram.df, bigram.df, trigram.df, quadgram.df,  file="allngrams.Rda")


rm(list = ls())

gc()

