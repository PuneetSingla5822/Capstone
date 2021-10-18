### Coursera's Data Science Capstone : Final Project
### ui.R file for the Shiny app
### It builds required UI for Next Word Predictor application which accepts an n-gram and predicts the next word.

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Data Science Capstone",
                   tabPanel("Predict the Next Word",
                            HTML("<strong>Author: Puneet Singla</strong>"),
                            br(),
                            HTML("<strong>Date: 17 October 2021</strong>"),
                            # Sidebar
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("This application takes your string and predict the next word to your string"),
                                    textInput("inputString", "Enter your word or partial phrase here",value = ""),
                                    helpText("Once finished, click on the button NextWord to see next word prediction"),
                                    submitButton('NextWord'),
                                    br(),
                                    br(),
                                    br(),
                                    br()
                                ),
                                mainPanel(
                                    h2("The suggested next word for your word or phrase is"),
                                    verbatimTextOutput("prediction"),
                                    strong("You entered the following word or phrase as Input to the application:"),
                                    tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                    textOutput('text1')
                                )
                            )
                            
                   )
)
)