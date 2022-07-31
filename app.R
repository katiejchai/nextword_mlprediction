if(!require("tidytext")) install.packages("tidytext")
if(!require("tm"))       install.packages("tm")
if(!require("stringi"))  install.packages("stringi")

library(shiny)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(tm)
library(stringi)

load("outputdata/preddata.RData")

source("functions/inputclean_fxn.R", local=TRUE)
source("functions/predngram_fxn.R", local=TRUE)
source("functions/nextword_fxn.R", local=TRUE)

# define UI
ui <- fluidPage(
  theme=shinytheme("darkly"),
  
  navbarPage(
    "Predictive Text App",
    
    tabPanel(
      "Next Word Prediction",
      
      # sidebar with prediction output
      sidebarLayout(
        sidebarPanel(
          tags$h3("Predictions of Next Word:"),
          verbatimTextOutput("prediction")
        ),
        
        # main panel with inputs
        mainPanel(
          textAreaInput(
            "textinput", "Enter any word or phrase:",
            value="hello", width="80%", rows=4
          ),
          
          sliderInput(
            "npreds", "Select the number of predictions to view:",
            min=1, max=10, value=3
          )
        )
      ),
    ),
    
    tabPanel(
      "About this app",
      
      tags$div(
        tags$h4("How to use this app"),
        "You can use two different inputs on the 'Next Word Prediction' tab of this app: 
          a text input and a number of predictions to view. You may type in any word or
          phrase in English to view the model predictions for what word will come next.
          Changing either of the inputs will be reflected in the list of next word 
          predictions on the left side of the page.",
        tags$br(), tags$br(),
        
        tags$h4("Data description"),
        "The main corpus data used in this app was obtained from the Johns Hopkins 
          University Data Science Specialization Capstone Project, and was downloaded 
          from this",
        tags$a(href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
               "link."),
        "Another dataset of bad words was used to filter these words out of the model 
        predictions. This data was obtained from the 'Bad Bad Words' dataset on Kaggle
        by Nicapotato, and can be found at this",
        tags$a(href="https://www.kaggle.com/datasets/nicapotato/bad-bad-words?resource=download",
               "link."),
        tags$br(), tags$br(),
        
        tags$h4("Code"),
        "Code used to generate this Shiny site can be found on",
        tags$a(href="http://www.github.com/katiejchai/nextword_mlprediction", "Github.")
      )
    ),
    
    tabPanel(
      "Background",
      
      tags$h4("Prediction model description"),
      "The prediction model was built based on n-grams of n=1,2,3,4. The n-grams were 
        developed using English-based corpora from blog, news, and twitter data provided by 
        SwiftKey. Because the provided data was so large in size, only a small sample of the 
        total data was used: 8% of blogs, 20% of news, and 2% of twitter. This was done as a 
        way to use more data that will likely be grammatically correct (ie. news), but also 
        incorporate more natural, everyday language (ie. blogs and twitter).",
      tags$br(), tags$br(),
      
      tags$h4("Model description"),
      "The final predictive text model is a form of a natural language processing backoff 
        model, where the algorithm initially implements higher-order n-grams, then 'backs off' 
        to lower n-grams when there are no predictions based on the higher n-grams. This model 
        is applied on the last words of the phrase (up to three). In the case where a multi-word 
        phrase was entered but no quad-, tri-, or bi-gram-based predictions were available, the 
        most common unigrams were used.
      
      This type of model was chosen in an attempt to incorporate a large portion of input phrase's 
        context and to utilize the most common phrases in the language."
    )
  )
)


# define server
server <- function(input, output, session) {
  
  output$prediction <- renderPrint({
    prediction <- nextword_fxn(input$textinput) %>%
      slice_head(n=input$npreds) %>%
      as.data.frame() %>%
      rename(`-----------------`=prediction)

    prediction
  })
  
}


shinyApp(ui=ui, server=server)