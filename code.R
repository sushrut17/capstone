library(tm)
library(ggplot2)
library(ngram)
library(dplyr)
library(parallel)
library(wordcloud)
library(shiny)
library(rsconnect)
library(rlang)
library(R.methodsS3)
library(R.oo)
library(R.utils)

dir <- "G:/Data Science Specialization/Capstone Project/Coursera-SwiftKey"

enTwitterLines <- readLines(paste(dir, "/final/en_US/en_US.twitter.txt", sep = ""))
enBlogLines <- readLines(paste(dir, "/final/en_US/en_US.blogs.txt", sep = ""))
enNewsLines <- readLines(paste(dir, "/final/en_US/en_US.news.txt", sep = ""))

lengthTwitter <- length(enTwitterLines)
lengthBlog <- length(enBlogLines)
lengthNews <- length(enNewsLines)

wordsTwitter <- wordcount(enTwitterLines)
wordsBlog <- wordcount(enBlogLines)
wordsNews <- wordcount(enNewsLines)


allLength <- lengthTwitter + lengthBlog + lengthNews
allWords <- wordsTwitter + wordsBlog + wordsNews
wordsPerLine <- allWords/allLength


#sample

sampleTwitter <- sample(enTwitterLines, 10000)
sampleBlog <- sample(enBlogLines, 10000)
sampleNews <- sample(enNewsLines, 10000)


#tokenize

twitterProcessed <- unlist(strsplit(sampleTwitter, "[\\.\\,!\\?\\:]+"))
twitterProcessed <- tolower(twitterProcessed)
twitterProcessed <- trimws(twitterProcessed)
twitterProcessed <- strsplit(twitterProcessed, "\\s")


blogProcessed <- unlist(strsplit(sampleBlog, "[\\.\\,!\\?\\:]+"))
blogProcessed <- tolower(blogProcessed)
blogProcessed <- trimws(blogProcessed)
blogProcessed <- strsplit(blogProcessed, "\\s")

newsProcessed <- unlist(strsplit(sampleNews, "[\\.\\,!\\?\\:]+"))
newsProcessed <- tolower(newsProcessed)
newsProcessed <- trimws(newsProcessed)
newsProcessed <- strsplit(newsProcessed, "\\s")

#merge

allProcessed <- c(twitterProcessed, blogProcessed, newsProcessed)

#length, wordcount



createNgram <- function(vec, n=2){
  l <- length(vec) 
  if(l < n){
    return(c())
  }else if(l == n){
    return(paste(vec, collapse=" "))
  }else{
    numNgrams <- l-n+1
    mtrx <- matrix(nrow=numNgrams, ncol=n)
    for(i in 1:n){
      m <- l - n + i
      mtrx[,i] <- vec[i:m]
    }
    ngrams <- apply(mtrx, 1, paste, collapse=" ")
    return(ngrams)
  }
} 

transformNGram <- function(termList, n=2){
  lapply(termList, createNgram, n=n)
}

allBiGrams <- transformNGram(allProcessed, 2)
allTriGrams <- transformNGram(allProcessed, 3)
allQuadGrams <- transformNGram(allProcessed, 4)


frequencyTable <- function(termList){
  s <- as.data.frame(table(data.frame(unlist(termList))))
  freq <- s[order(-s$Freq),]
  rownames(freq) <- 1:nrow(freq)
  total <- sum(freq$Freq)
  freq$CumFreq <- cumsum(freq$Freq)
  freq$Coverage <- freq$CumFreq/total
  colnames(freq) <- c("term", "Freq", "CumFreq", "Coverage")
  return(freq)
  
}

allFreq <- frequencyTable(allProcessed)
allBiFreq <- frequencyTable(allBiGrams)
allTriFreq <- frequencyTable(allTriGrams)
allQuadFreq <- frequencyTable(allQuadGrams)

barplot(head(allFreq$Freq,20), names.arg = head(allFreq$term, 20))
barplot(head(allBiFreq$Freq,20), names.arg = head(allBiFreq$term, 20))
head(allTriFreq[1:2],20)


#Bi

col1 <- as.matrix(allBiFreq$term)

col5 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][1]))
colnames(col5) <- "col5"

col6 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][2]))
colnames(col6) <- "col6"

allBiFreq <- cbind(allBiFreq, col5, col6)


#Tri

col1 <- as.matrix(allTriFreq$term)

col5 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][1]))
colnames(col5) <- "col5"

col6 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][2]))
colnames(col6) <- "col6"

col7 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][3]))
colnames(col7) <- "col7"

allTriFreq <- cbind(allTriFreq, col5, col6, col7)



#Quad

col1 <- matrix(allQuadFreq$term)

col5 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][1]))
colnames(col5) <- "col5"

col6 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][2]))
colnames(col6) <- "col6"

col7 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][3]))
colnames(col7) <- "col7"

col8 <- as.data.frame(apply(col1, 1, function(x) strsplit(removeNumbers(removePunctuation(tolower(x))), " ")[[1]][4]))
colnames(col8) <- "col8"

allQuadFreq <- cbind(allQuadFreq, col5, col6, col7, col8)


#save in to .RData files

save(allFreq, file="allFreq.RData")
save(allBiFreq, file="allBiFreq.RData")
save(allTriFreq, file="allTriFreq.RData")
save(allQuadFreq, file="allQuadFreq.RData")
