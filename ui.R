#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Predictor"),
  
   
  sidebarLayout(
    sidebarPanel(
      
      textInput("text1", "Please enter a word/phrase"),
      
      submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h5("This application predicts the next word. The method of ngrams has been used to arrive at this model."),
      
    textOutput("out1")
    )
  )
))
