#
library(shiny)
library(quanteda)
library(stringr)
##############################
biblog <- readRDS(file = "Tidy/biBlog.rds")
triblog <- readRDS(file = "Tidy/triBlog.rds")
fourblog <- readRDS(file = "Tidy/fourBlog.rds")

biNews <- readRDS(file = "Tidy/biNews.rds")
triNews <- readRDS(file = "Tidy/triNews.rds")
fourNews <- readRDS(file = "Tidy/fourNews.rds")

biTwit <- readRDS(file = "Tidy/biTwit.rds")
triTwit <- readRDS(file = "Tidy/triTwit.rds")
fourTwit <- readRDS(file = "Tidy/fourTwit.rds")
############################### 
predictWord3 <- function(sent, biGram, triGram, fourGram){
  n <- 3
  if(n > 2) {
    ww3 <- word(sent, n-2, n)
    ns3 <- which(fourGram$feature1 == ww3)
    #print(length(ns3))
    if(length(ns3) == 0) {
      ww2 <- word(sent, n-1, n)
      ns2 <- which(triGram$feature1 == ww2)
      if(length(ns2) == 0) {
        ww1 <- word(sent, n, n)
        #print(ww1)
        ns1 <- which(biGram$feature1 == ww1) 
        #print('proba')
        #print(ns1)
        if(length(ns1) == 0) {
          predWorld <- "No match (replace the last word)"
          return(predWorld)
        } else {
          predWorld <- biGram[ns1]$feature2
          return(predWorld)
        }
      } else {
        predWorld <- triGram[ns2]$feature2
        return(predWorld)
      }
    } else {
      predWorld <- fourGram[ns3]$feature2
      return(predWorld)
    } 
  }
}

predictWord2 <- function(sent, biGram, triGram){
  n <- 2
  if(n == 2) {
    ww2 <- word(sent, n-1, n)
    #print(ww2)
    ns2 <- which(triGram$feature1 == ww2)
    #print(ns2)
    #print('proba2')
    if(length(ns2) == 0) {
      #print("proba1")
      ww1 <- word(sent, n, n)
      ns1 <- which(biGram$feature1 == ww1) 
      if(length(ns1) == 0) {
        predWorld <- "No match (replace the last word)"
        return(predWorld)
      }else {
        predWorld <- biGram[ns1]$feature2
        return(predWorld)
      } 
    }else {
      #print("proba")
      predWorld <- triGram[ns2]$feature2
      return(predWorld)
    }
  }
}

predictWord1 <- function(sent, biGram){
  n <- 1
  if(n == 1) {
    ww1 <- word(sent, n, n)
    ns1 <- which(biGram$feature1 == ww1) 
    if(length(ns1) == 0) {
      predWorld <- "No match (replace the last word)"
      return(predWorld)
    } else {
      predWorld <- biGram[ns1]$feature2
      return(predWorld)
    }
  }
}

clearSent <- function(sent){
  sent <- tokens(sent, 
                 remove_punct = TRUE,
                 remove_numbers = TRUE, 
                 remove_symbols = TRUE,
                 remove_twitter = TRUE,
                 remove_hyphens  =  TRUE,
                 remove_url  =  TRUE,
  )
  sent <- tokens_tolower(sent)
  sent <- tokens_remove(sent, pattern = stopwords('en'))
  n <- length(unlist(sent))
  sent <- toString(sent)
  sent <- gsub(",","",sent)
  if(n>3){
    sent <- word(sent, n-2, n)
    n <- 3
  }
  return(sent)
}

zeroSent <- "This sentence is of zero length or contains only functional (not meaningful) words"

###############################
shinyServer(function(input, output) {
    output$value <- renderPrint({switch(as.numeric(input$radio),
      switch(str_count(clearSent(input$txt), "\\w+")+1, zeroSent , predictWord1(clearSent(input$txt), biblog), 
             predictWord2(clearSent(input$txt), biblog, triblog), predictWord3(clearSent(input$txt), biblog, triblog, fourblog)),
      switch(str_count(clearSent(input$txt), "\\w+")+1, zeroSent, predictWord1(clearSent(input$txt), biNews),
             predictWord2(clearSent(input$txt), biNews, triNews), predictWord3(clearSent(input$txt), biNews, triNews, fourNews)),
      switch(str_count(clearSent(input$txt), "\\w+")+1, zeroSent, predictWord1(clearSent(input$txt), biTwit), 
             predictWord2(clearSent(input$txt), biTwit, triTwit), predictWord3(clearSent(input$txt), biTwit, triTwit, fourTwit)))})
    })
