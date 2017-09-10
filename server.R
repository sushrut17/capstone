#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output) {
  
  load("allFreq.RData")
  load("allBiFreq.RData")
  load("allTriFreq.RData")
  load("allQuadFreq.RData")
  
  output$out1 <- renderText({
    
    predictNext <- vector()
    
    #clean
    
    library(dplyr)
    library(tm)
    
    x <- input$text1
    
    xclean <- removeNumbers(removePunctuation(tolower(x)))
    xs <- strsplit(xclean, " ")[[1]]
    
    if (length(xs) >= 4){
      xs <- tail(xs,3)
    }
    
    
    if (length(xs) == 1){
      p1 <- allBiFreq[xs[1] == allBiFreq[,5],]
      
      if (nrow(p1) > 0){
        predictNext <- head(p1,1)$col6
      }
      
      if (nrow(p1) == 0){
        predictNext <- head(allFreq,1)$term
      }
    }
    
    
    if (length(xs) == 2){
      p1 <- allTriFreq[xs[1] == allTriFreq[,5],]
      p2 <- p1[xs[2] == p1[,6],]
      
      if (((nrow(p2) >= 2) & (p2$Freq[1] > p2$Freq[2])) || (nrow(p2) == 1))  {
        predictNext <- head(p2,1)$col7
      }
      
      if ((nrow(p2) >= 0) & (p2$Freq[1] == p2$Freq[2])){
        sameFreq <- p2$Freq[1] 
        sameFreqNumber <- length(which(p2$Freq == sameFreq))
        words <- as.matrix(p2$col7[1:sameFreqNumber])
        maxIndex <- which.max(apply(words, 1, function(x) allFreq$Freq[which(allFreq$term == x)]))
        predictNext <- words[maxIndex]
        
      }
      
      if (nrow(p2) == 0){
        
        xs <- tail(xs, 1)
        p1 <- allBiFreq[xs[1] == allBiFreq[,5],]
        if (nrow(p1) != 0){
          predictNext <- head(p1,1)$col6
        }
        
        if (nrow(p1) == 0){
          predictNext <- head(allFreq,1)$term
        }
      }
      
      
    }
    
    if (length(xs) == 3){
      
      
      p1 <- allQuadFreq[xs[1] == allQuadFreq[,5],]
      p2 <- p1[xs[2] == p1[,6],]
      p3 <- p2[xs[3] == p2[,7],]
      
      
      
      if ((nrow(p3) >= 2) & (p3$Freq[1] == p3$Freq[2])){
        
        sameFreq <- p3$Freq[1]
        sameFreqNumber <- length(which(p3$Freq == sameFreq))
        words <- as.matrix(p3$col8[1:sameFreqNumber])
        maxIndex <- which.max(apply(words, 1, function(x) allFreq$Freq[which(allFreq$term == x)]))
        predictNext <- words[maxIndex]
      }
      
      if (((nrow(p3) >= 2) & (p3$Freq[1] > p3$Freq[2])) || (nrow(p3) == 1)) {
        predictNext <- head(p3, 1)$col8
      }
      
      if (nrow(p3) == 0){
        xs <- tail(xs,2)
        
        p1 <- allTriFreq[xs[1] == allTriFreq[,5],]
        p2 <- p1[xs[2] == p1[,6],]
        
        if (((nrow(p2) >= 2) & (p2$Freq[1] > p2$Freq[2])) || (nrow(p2) == 1)) {
          predictNext <- head(p2,1)$col7
        }
        
        if ((nrow(p2) >= 2) & (p2$Freq[1] == p2$Freq[2])){
          sameFreq <- p2$Freq[1] 
          sameFreqNumber <- length(which(p2$Freq == sameFreq))
          words <- as.matrix(p2$col7[1:sameFreqNumber])
          maxIndex <- which.max(apply(words, 1, function(x) allFreq$Freq[which(allFreq$term == x)]))
          predictNext <- words[maxIndex]
          
        }
        
        
        if (nrow(p2) == 0){
          
          xs <- tail(xs,1)
          p1 <- allBiFreq[xs[1] == allBiFreq[,5],]
          
          if (nrow(p1) != 0){
            predictNext <- head(p1,1)$col6
          }
          
          if (nrow(p1) == 0){
            predictNext <- head(allFreq,1)$term
          }
          
        }
        
        
      }
      
      
    }
    
    if (is.null(predictNext) == TRUE){
      predictNext <- "the"
    }
    
    return(as.character(predictNext))
  })
  
  
})
