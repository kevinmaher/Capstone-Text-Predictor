## read, sample and clean the data

library(tm)
library(SnowballC)
library(RWeka)
library(stringi)

rm(list=ls())

## set the work directory on local 
setwd("C:/RStudio/capstone/final/en_US")
## list all files in local directory
list.files("C:/RStudio/capstone/final/en_US")

## read in twitter data
readTwitter <- file("C:/RStudio/capstone/final/en_US/en_US.twitter.txt", "r")
fullTwitter <- readLines(readTwitter,encoding="UTF-8",skipNul=TRUE, warn=FALSE)
##read news data
readNews <- file("C:/RStudio/capstone/final/en_US/en_US.news.txt", "r")
fullNews <- readLines(readNews, encoding="UTF-8",skipNul=TRUE,warn=FALSE)
##read blogs data
readBlogs <- file("C:/RStudio/capstone/final/en_US/en_US.blogs.txt", "r")
fullBlogs <- readLines(readBlogs, encoding="UTF-8",skipNul=TRUE, warn=FALSE)

## close connections
close(readTwitter)
close(readNews)
close(readBlogs)

## Basic file information
stri_stats_general(fullTwitter)
stri_stats_general(fullNews)
stri_stats_general(fullBlogs)

## sample 10% from each dataset for analysis
set.seed(1234)
# 10% sampling
sampleTwitter <- sample(fullTwitter, (round(0.1*length(fullTwitter))))
sampleNews <- sample(fullNews, (round(0.1*length(fullNews))))
sampleBlogs <- sample(fullBlogs, (round(0.1*length(fullBlogs))))

## find summary information on each sampled dataset
str(sampleTwitter)
str(sampleNews)
str(sampleBlogs)

## pull together sample data to form one dataset
sampleData <- c(sampleTwitter, sampleBlogs,sampleNews)
## create corpus
sampleCorp <- Corpus(VectorSource(list(sampleData)))
## clean data
removePunct <- function(x) gsub("(.*?)($|'|[^[:punct:]]+?)(.*?)", "\\2", x)
sampleCorp <- tm_map(sampleCorp, content_transformer(removePunct))
sampleCorp <- tm_map(sampleCorp, removeNumbers)# remove numbers
sampleCorp <- tm_map(sampleCorp, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
sampleCorp <- tm_map(sampleCorp, content_transformer(tolower), lazy = TRUE)
sampleCorp <- tm_map(sampleCorp, stripWhitespace)   ## remove whitespaces
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)  ## remove urls
sampleCorp <- tm_map(sampleCorp, content_transformer(removeURL))

## profanity removal
## swearwords to filter sourced from http://www.bannedwordlist.com/
swearwords <- file("C:/RStudio/capstone/final/en_US/swearwords.txt")
allswearwords <- readLines(swearwords, 76)
close(swearwords)
sampleCorp <- tm_map(sampleCorp, removeWords, allswearwords)

##save corpus as text file                           
writeCorpus(sampleCorp, filenames="samplecorpfilenames.txt")
##save as an rda file
save(sampleCorp, file="samplecorp.Rda")

rm(list = ls())

gc()