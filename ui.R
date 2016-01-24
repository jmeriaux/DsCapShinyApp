shinyUI(pageWithSidebar(
  headerPanel("Next word prediction"),

  sidebarPanel(

    textInput("Sentence", label = h4("Type a sentence...") ),
    


    helpText("Please type the beginning of a sentence", 
             "and the application will try to guess the next words.")
    ),

    mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Words", 
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
                    
                    tabPanel("Help", h4("Online documentation"),
                             p("This application is based on a news, tweets and blog corpus."),
                             p(""),
                             p(""),
                             br(),
                             br(),
                             p("The word cloud will refresh automatically.")
                                                        ) )

  )
))
