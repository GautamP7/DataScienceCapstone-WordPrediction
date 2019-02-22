library(shiny)
library(stringr)
library(tm)

quadgram <- readRDS("quadgram.RData");
trigram <- readRDS("trigram.RData");
bigram <- readRDS("bigram.RData");
    
predictText <- function(x) {
    x <- tolower(x)
    x <- removePunctuation(x)
    x <- removeNumbers(x)
    x_split <- strsplit(x, " ")[[1]]
    
    # Back Off Algorithm
    # Predict the next term of the user input sentence
    # 1. For prediction of the next word, Quadgram is used first.
    # 2. If no Quadgram is found, back off to Trigram.
    # 3. If no Trigram is found, back off to Bigram.
    # 4. If no Bigram is found, back off to the most common word with highest frequency.
    
    
    if(length(x_split) >= 3) {
        x_split <- tail(x_split, 3)
        if(identical(character(0), head(quadgram[quadgram$unigram == x_split[1] & quadgram$bigram == x_split[2] & quadgram$trigram == x_split[3], 4], 3))){
            predictText(paste(x_split[2], x_split[3]))
        }
        else {
            head(quadgram[quadgram$unigram == x_split[1] & quadgram$bigram == x_split[2] & quadgram$trigram == x_split[3], 4], 3)
        }
    }
    else if(length(x_split) == 2) {
        x_split <- tail(x_split, 2)
        if(identical(character(0), head(trigram[trigram$unigram == x_split[1] & trigram$bigram == x_split[2], 3], 3))) {
            predictText(x_split[2])
        }
        else {
            head(trigram[trigram$unigram == x_split[1] & trigram$bigram == x_split[2], 3], 3)
        }
    }
    else if(length(x_split) == 1) {
        x_split <- tail(x_split, 1)
        if(identical(character(0), head(bigram[bigram$unigram == x_split[1], 2], 3))) {
            head("", 1)
        }
        else {
            head(bigram[bigram$unigram == x_split[1], 2], 3)
        }
    }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$prediction <- renderPrint({
    result <- predictText(input$inputString)
    result
  });
  
  output$text1 <- renderText({
    input$inputString});
  
})
