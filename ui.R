shinyUI(pageWithSidebar(
  headerPanel("Next word prediction"),

  sidebarPanel(

    textInput("Sentence", label = h4("Type a sentence...") ),
    


    helpText("Please type the beginning of a sentence", 
             "and the application will try to guess the next words.")
    ),

    mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Next Word Prediction", 
                             h3("Last 3 tokens extracted from sentence"),
                             textOutput('tokens'), 
                             h5("Note: zznumzz represents a number, and zzsebzz beginning of a sentence."),
                             br(""),
                             h3("Top 5 candidate words with probability"),
                             textOutput('text1'), 
                             textOutput('text2'), 
                             textOutput('text3'), 
                             textOutput('text4'),
                             textOutput('text5')
                                ), 
                    

                    
                    tabPanel("Help", h4("Help and Examples"),
                             p("This application is using a collection of unigrams, bigrams, trigrams and"),
                             p("quadrigrams and an interpolation model to compute the most probable"),
                             p("next words."),
                             p("The sentence is tokenized and beginning of sentence and numbers"),
                             p("are indentified as such and turned in specific tokens."),
                             br(),
                             p("Using the application is very simple:"),
                             p("- Type a sentence on the left"),
                             p("- See the predicted words on the right!"),
                             br(),     
                             p("Try to type 'see you at 8' ... and 'am' or 'pm' will be in the top predictions."),
                             p("Try to type 'I am so' ... and 'excited','happy' and 'glad' will be in the top predictions.")
                    )
                     )

  )
))
