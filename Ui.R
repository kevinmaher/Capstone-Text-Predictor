library(shiny)
## UI code for text prediction model
## Setup fields/panels/tabs layout for the landing page
shinyUI(
    navbarPage("Text Prediction Application",
        tabPanel("Find Next Word",
            fluidPage(
                titlePanel("Data Science Capstone : Text Prediction Application"),
                    mainPanel(
                        h4("Description:"),
                        p("Either type a sequence of words or paste a sentence into the text box below and you will see five most probable next word predictions"),
                        p("Please wait until the default top 5 predictions have been returned for the default sentence"),
                        textInput("text", label = h5("Please type your text below"), value = "this is a text "),
                        
                        h3('Top Five Predictions'),
                        textOutput("text1"),
                        textOutput("text2"),
                        textOutput("text3"),
                        textOutput("text4"),
                        textOutput("text5"),
                        uiOutput('matrix') 
                            )
                    )
               ),
                ## setup 2nd page (working of tabs) to display the basic data details
                tabPanel("Text Prediction Data Description",
                         hr(),
                         h3("Description"),
                         p("The goal of this exercise is to create a product to highlight the prediction algorithm that I have built and to provide an interface that can be accessed by others via a Shiny app."),
                         p("The data is from a corpus called HC Corpora (www.corpora.heliohost.org) and can be downloaded from the site below"),
                         a(href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "The Capstone Text Prediction Dataset"),
                         p("for details on the corpora available. The files have been language filtered but may still contain some foreign text."),
                         p(" The data was sampled , cleaned and tokenised. In my case I've sampled 5% of the data from each of the three files."),
                         p("The data and the code for this product can be found at"),a(href= "https://github.com/kevinmaher/Capstone", "Data and Code")
                         
                ),
               ## setup page for help
               tabPanel("Help",
                        hr(),
                        h3("Description"),
                        p("The goal of this exercise is to create a product to highlight the prediction algorithm that I have built and to provide an interface that can be accessed by others via a Shiny app."),
                        p("This Shiny app takes as input a phrase (multiple words) in a text box input and outputs a prediction of the next word"),
                        p("As well as the top prediction, it also shows the next four most probable words."),
                        p("The user can either type a sequence of words or paste a sentence into the text box"),
                        p("Please note that the user must wait until the default top 5 predictions have been returned for the default sentence before starting.")
                        
                          
                     )
               
    )
)