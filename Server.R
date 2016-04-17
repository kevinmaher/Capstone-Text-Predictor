library(shiny)
library(stringr)


load("allngrams.Rda")
source("ngramstokenizer.R")

cleanInput <- function(inputs) {
    
    ## remove numbers
    inputstring <- gsub('[[:digit:]]+', '', inputs)
    ## remove all punctuation except apostrophes
    inputstring <- gsub("(.*?)($|'|[^[:punct:]]+?)(.*?)", "\\2", inputstring)
    # convert all words to lowercase
    inputstring  <- tolower(inputstring)
    # remove extra spaces
    trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))
    inputstring<-trim(inputstring)      
    
}

predict_fun <- function(inputString, unigram.df, bigram.df, trigram.df, ngram_tokenizer) {
    if (length(inputString) > 0) {
        inputstring <- cleanInput(inputString)
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
                }
            }
            
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
            }
            
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
          
        }
    }
        return (c(word1,word2,word3, word4, word5, sprintf("%.1f %%", 100*probability1),
                  sprintf("%.1f %%", 100*probability2), sprintf("%.1f %%", 100*probability3), sprintf("%.1f %%", 100*probability4),sprintf("%.1f %%", 100*probability5)))
        
    } 
    

shinyServer(
    function(input, output) {
        observe({
            param <- input$text
            prediction <- predict_fun(input$text, unigram.df, bigram.df, trigram.df, ngram_tokenizer)
            output$matrix <- renderTable({
                
                matrix <- matrix(prediction[1:10],nrow=5, ncol = 2)
                
                colnames(matrix) <- c('Word','Probability')
                matrix })
                
            }
      )    
    }
)