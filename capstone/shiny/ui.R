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
   # titlePanel("Demonstration of the word prediction model for databases: Blogs, News, Twitter"),
    titlePanel(
        h1("Demonstration of the word prediction model for databases: Blogs, News, Twitter", align = "center")
    ),
    titlePanel(h3("In this version, unique bi-grams, tri-grams, and four-grams of 5000 each are used, 
                  functional words are excluded according to the library of the quanteda package.", align = "center")
    ),
    radioButtons("radio", label = h3("Select data type:"),
                 choices = list("Blogs" = 1, "News" = 2, "Twitter" = 3), 
                 selected = 1),
    textInput("txt", h3("Text input:"), "blues", width = "600px"),
    submitButton("Submit", width = "150px"),
    hr(),
    h3("Predicted Word:"),
    fluidRow(column(3, verbatimTextOutput("value"))),
))
