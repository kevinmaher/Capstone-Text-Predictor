## test predict scripts

## set the work directory on local 
setwd("C:/RStudio/capstone/final/en_US")
require(stringr)

load("allngrams.Rda")
source("ngramsTokenizer.R")

inputs <- "Talking to your mom has the same effect as a hug and helps reduce your"

cleanInput <- function(inputs) {
    
    ## remove numbers
    inputstring <- gsub('[[:digit:]]+', '', inputs)
    ## remove all punctuation except apostrophes
    inputstring <- gsub("(.*?)($|'|[^[:punct:]]+?)(.*?)", "\\2", inputstring)
    inputstring  <- tolower(inputstring)
    # remove extra spaces
    trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))
    inputstring<-trim(inputstring)      
}

if (length(inputs) > 0) {
    
    inputstring <- cleanInput(inputs)
    unigram.tokenizer <- ngram_tokenizer(1)
    inputstring.token <- unigram.tokenizer(inputstring)
    
    if (length(inputstring.token) > 2) {
        inputstring.token <- tail(inputstring.token,3)
        prediction.df <- quadgram.df[(quadgram.df$word1==inputstring.token[1] & quadgram.df$word2==inputstring.token[2] & quadgram.df$word3==inputstring.token[3]),]
        row.names(prediction.df) <- NULL
        word1<- prediction.df$predicted[1]
        word2 <- prediction.df$predicted[2]
        word3 <- prediction.df$predicted[3]
        word4 <- prediction.df$predicted[4]
        word5 <- prediction.df$predicted[5]
        probability1 <- prediction.df$probability[1]
        probability2 <- prediction.df$probability[2]
        probability3 <- prediction.df$probability[3]
        probability4 <- prediction.df$probability[4]
        probability5 <- prediction.df$probability[5]
        
        ##stupid backoff
        if (is.na(word1)) {
            prediction.df <- trigram.df[(trigram.df$word1==inputstring.token[2] & trigram.df$word2==inputstring.token[3]),]
            row.names(prediction.df) <- NULL
            word1<- prediction.df$predicted[1]
            word2 <- prediction.df$predicted[2]
            word3 <- prediction.df$predicted[3]
            word4 <- prediction.df$predicted[4]
            word5 <- prediction.df$predicted[5]
            probability1 <- prediction.df$probability[1]
            probability2 <- prediction.df$probability[2]
            probability3 <- prediction.df$probability[3]
            probability4 <- prediction.df$probability[4]
            probability5 <- prediction.df$probability[5]
            ##stupid backoff
            if (is.na(word1)) {
                prediction.df <- bigram.df[(bigram.df$word1==inputstring.token[3]),]
                row.names(prediction.df) <- NULL
                word1<- prediction.df$predicted[1]
                word2 <- prediction.df$predicted[2]
                word3 <- prediction.df$predicted[3]
                word4 <- prediction.df$predicted[4]
                word5 <- prediction.df$predicted[5]
                probability1 <- prediction.df$probability[1]
                probability2 <- prediction.df$probability[2]
                probability3 <- prediction.df$probability[3]
                probability4 <- prediction.df$probability[4]
                probability5 <- prediction.df$probability[5]
                ##(c('morethan2 but a 1 res: ', inputstring.token[1], inputstring.token[2],word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))
            }
            
            
            ##(c('morethan2 but a 1 res: ', inputstring.token[1], inputstring.token[2],word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))
        }
        
        (c('morethan2: ', inputstring.token[1], inputstring.token[2],inputstring.token[3],word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))
        
        
     
        
            } else if (length(inputstring.token) == 2) {
        
        prediction.df <- trigram.df[(trigram.df$word1==inputstring.token[1] & trigram.df$word2==inputstring.token[2]),]
        row.names(prediction.df) <- NULL
        word1<- prediction.df$predicted[1]
        word2 <- prediction.df$predicted[2]
        word3 <- prediction.df$predicted[3]
        word4 <- prediction.df$predicted[4]
        word5 <- prediction.df$predicted[5]
        probability1 <- prediction.df$probability[1]
        probability2 <- prediction.df$probability[2]
        probability3 <- prediction.df$probability[3]
        probability4 <- prediction.df$probability[4]
        probability5 <- prediction.df$probability[5]
        
        ##stupid backoff
        if (is.na(word1)) {
            prediction.df <- bigram.df[(bigram.df$word1==inputstring.token[2]),]
            row.names(prediction.df) <- NULL
            word1<- prediction.df$predicted[1]
            word2 <- prediction.df$predicted[2]
            word3 <- prediction.df$predicted[3]
            word4 <- prediction.df$predicted[4]
            word5 <- prediction.df$predicted[5]
            probability1 <- prediction.df$probability[1]
            probability2 <- prediction.df$probability[2]
            probability3 <- prediction.df$probability[3]
            probability4 <- prediction.df$probability[4]
            probability5 <- prediction.df$probability[5]
            ##(c('morethan2 but a 1 res: ', inputstring.token[1], inputstring.token[2],word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))
        }
        (c ('equel to 2: ', inputstring.token[1], inputstring.token[2],word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))

        } else if (length(inputstring.token) == 1) {
        
        prediction.df <- bigram.df[(bigram.df$word1==inputstring.token[1]),]
        row.names(prediction.df) <- NULL
        word1<- prediction.df$predicted[1]
        word2 <- prediction.df$predicted[2]
        word3 <- prediction.df$predicted[3]
        word4 <- prediction.df$predicted[4]
        word5 <- prediction.df$predicted[5]
        probability1 <- prediction.df$probability[1]
        probability2 <- prediction.df$probability[2]
        probability3 <- prediction.df$probability[3]
        probability4 <- prediction.df$probability[4]
        probability5 <- prediction.df$probability[5]
        (c('equel to 1: ',inputstring.token[1],word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))
        
        
        } else if (length(inputstring.token) < 1) {
        word1<- "Please enter one or more words"
        word2 <- ""
        word3 <- ""
        word4 <-  ""
        word5 <-  ""
        probability1 <- 0
        probability2 <- 0
        probability3 <- 0
        probability4 <- 0
        probability5 <- 0
        (c(inputstring.token[2], inputstring.token[3],word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))
        
    }
  ## (c(word1,word2,word3, word4,word5, probability1,probability2,probability3,probability4,probability5))

}




##q1
##The guy in front of me just bought a pound of bacon, a bouquet, and a case of
##You're the reason why I smile everyday. Can you follow me please? It would mean the
##Hey sunshine, can you follow me and make me the
##Very early observations on the Bills game: Offense still struggling but the
##Go on a romantic date at the
##Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my
##Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some
##After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little
##Be grateful for the good times and keep the faith during the
##If this isn't the cutest thing you've ever seen, then you must be


##q2
##When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd
##Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his
##I'd give anything to see arctic monkeys this
##Talking to your mom has the same effect as a hug and helps reduce your
##When you were in Holland you were like 1 inch away from me but you hadn't time to take a
##I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the
##I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each
##Every inch of you is perfect from the bottom to the
##I'm thankful my childhood was filled with imagination and bruises from playing
##I like how the same people are in almost all of Adam Sandler's

# predictive-text-analysis
# 
# Program to predict the next word in a sentence using R's tm, tau, and RWeka (optional)
# 
# Incomplete - please check back later.
# 
# Importing text into the program:
# 
# Import text file.
# 
# Convert text file to corpus.
# 
# Given n, use tau/RWeka packages on corpus to produce df of frequencies of ngrams.
# 
# Create a list of dfs with ngrams with n from 1 to 5.
# 
# Convert frequencies to a more effective predictor.
# 
# Running the program:
# 
# Program attempts to predict the next word of an incomplete sentence.
# 
# User inputs a phrase or a list of strings of words.
# 
# Program runs through the ngram models to find highest match based on calculated probabilities.
# 
# Returns word, or a dataframe containing words sorted by their probabilities.
# 
# Other features:
# 
# Ignores profanity; profanity list customizable.
# 
# Ignores Arabic numbers.
# 
# Ignores emoticons; emoticon list customizable.


